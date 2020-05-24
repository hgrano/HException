{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module HError(
  -- * Basic types.
  Error,
  (:^:),
  Error1,
  Result,
  Result1,
  TypeIndexed,
  -- * Combinator functions for working with @Result@s.
  extend,
  getMay,
  get,
  err,
  generalize,
  raise
) where

import qualified Data.HList.CommonMain as H
import qualified Data.HList.TIC        as T
import qualified Data.HList.TIP        as TP
import qualified Data.HList.Variant    as V
import           Data.Maybe            (fromJust)
import           GHC.TypeLits          (KnownNat)

-- | Type for containing an error which maybe one of known list of error types @e@.
newtype Error e = Error { unError :: T.TIC e }

instance V.ShowVariant e => Show (Error e) where
  show = ("Error" ++) . drop 3 . show . unError

deriving instance Eq (Error '[])
deriving instance (Eq e, Eq (V.Variant es)) => Eq (Error (e ': es))

deriving instance Ord (Error '[])
deriving instance (Ord e, Ord (V.Variant es)) => Ord (Error (e ': es))

-- | Type alias for ease of constructing 'Error's.
type e :^: es = H.Tagged e e ': es
infixr 7 :^:

type Error1 e = Error (e :^: '[])

-- | Type for returning results from computations which may fail with one of a known set of errors or return a value.
type Result e a = Either (Error e) a

-- | A specialization of 'Result' for computations which can return only one type of error.
type Result1 e a = Either (Error1 e) a

-- | Constrain that the type-list @l@ has no duplicate types, and so can be indexed via type only.
type TypeIndexed l = (TP.HAllTaggedEq l, H.HLabelSet (H.LabelsOf l), H.HAllTaggedLV l)

-- | Changes (extends) the type of an 'Error' so it is more general than the original. The underlying stored value
-- is left unchanged. For most use cases 'extend' will likely be more suitable than 'generalize'.
generalize :: (TypeIndexed f, V.ExtendsVariant e f) => Error e -> Error f
generalize (Error (T.TIC v)) = Error . T.TIC $ H.extendsVariant v

-- | Extend the given result so that it may be used in a context which can return a superset of the errors that may
-- arise from the original result. This is useful for calling multiple functions which return different errors types
-- from within a single function.
extend :: (TypeIndexed f, V.ExtendsVariant e f) => Result e a -> Result f a
extend (Left e) = Left $ generalize e
extend (Right x) = Right x

-- | Extract an error of a specific type from an 'Error'. Returns @Nothing@ if the error encased is not of the required
-- type. Calls to this function will not type check if the type @e@ does not exist in the type-list @es@.
getMay :: (H.HasField e (H.Record es) e,
          H.HFind1 e (H.UnLabel e (H.LabelsOf es)) (H.UnLabel e (H.LabelsOf es)) n,
          KnownNat (H.HNat2Nat n)) =>
          Error es -> Maybe e
getMay = H.hOccurs . unError

-- | Specialization of 'getMay' for cases where there is only a single type of error.
get :: Error1 e -> e
get = fromJust . getMay -- TODO how to avoid using fromJust although it is safe

-- | Lift a standard exception value into an 'Error'.
err :: (H.HasField e (H.Record es) e,
       H.HFind1 e (H.UnLabel e (H.LabelsOf es)) (H.UnLabel e (H.LabelsOf es)) n,
       KnownNat (H.HNat2Nat n),
       TypeIndexed es) =>
       e -> Error es
err = Error . H.mkTIC

-- | Return an error result using the provided error.
raise :: (H.HasField e (H.Record es) e,
          H.HFind1 e (H.UnLabel e (H.LabelsOf es)) (H.UnLabel e (H.LabelsOf es)) n,
          KnownNat (H.HNat2Nat n),
          TypeIndexed es) =>
          e -> Result es a
raise = Left . err
