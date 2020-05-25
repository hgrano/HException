{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE MonoLocalBinds  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE UndecidableInstances  #-} -- TODO how to remove this extension?

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
  raise,
  recover,
  value
) where

import qualified Data.HList.CommonMain as H
import qualified Data.HList.TIC        as T
import qualified Data.HList.TIP        as TP
import qualified Data.HList.Variant    as V
import           Data.Maybe            (fromJust)
import           GHC.TypeLits          (KnownNat)

-- | Type for containing an error which maybe one of known list of error types @es@.
newtype Error es = Error { unError :: T.TIC es }

instance V.ShowVariant es => Show (Error es) where
  show = ("Error" ++) . drop 3 . show . unError

deriving instance Eq (Error '[])
deriving instance (Eq e, Eq (V.Variant es)) => Eq (Error (e ': es))

deriving instance Ord (Error '[])
deriving instance (Ord e, Ord (V.Variant es)) => Ord (Error (e ': es))

-- | Type alias for ease of constructing 'Error's.
type e :^: es = H.Tagged e e ': es
infixr 7 :^:

type Error1 e = Error (e :^: '[])

-- | Type for returning results from computations which may fail with one of a known set of errors @es@ or return a
-- value @a@.
type Result es a = Either (Error es) a

-- | A specialization of 'Result' for computations which can return only one type of error.
type Result1 e a = Either (Error1 e) a

-- | Constrain that the type-list @xs@ has no duplicate types, and so can be indexed via type only.
type TypeIndexed xs = (TP.HAllTaggedEq xs, H.HLabelSet (H.LabelsOf xs), H.HAllTaggedLV xs)

-- | Changes (extends) the type of an 'Error' so it is more general than the original. The underlying stored value
-- is left unchanged. For most use cases 'extend' will likely be more suitable than 'generalize'.
generalize :: (TypeIndexed es', V.ExtendsVariant es es') => Error es -> Error es'
generalize (Error (T.TIC v)) = Error . T.TIC $ H.extendsVariant v

-- | Extend the given result so that it may be used in a context which can return a superset of the errors that may
-- arise from the original result. This is useful for calling multiple functions which return different errors types
-- from within a single function.
extend :: (TypeIndexed es', V.ExtendsVariant es es') => Result es a -> Result es' a
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
get = fromJust . getMay -- TODO how to avoid using fromJust although it is safe -- try unvariant

-- | Lift a standard exception value into an 'Error'.
err :: (H.HasField e (H.Record es) e,
       H.HFind1 e (H.UnLabel e (H.LabelsOf es)) (H.UnLabel e (H.LabelsOf es)) n,
       KnownNat (H.HNat2Nat n),
       TypeIndexed es) =>
       e -> Error es
err = Error . H.mkTIC

-- | Return an error result using the provided error. HError equivalent of "throwing" an exception - but a value is
-- returned instead of being thrown.
raise :: (H.HasField e (H.Record es) e,
          H.HFind1 e (H.UnLabel e (H.LabelsOf es)) (H.UnLabel e (H.LabelsOf es)) n,
          KnownNat (H.HNat2Nat n),
          TypeIndexed es) =>
          e -> Result es a
raise = Left . err

type Handler es es' a = Error es -> Result es' a

-- | Handle errors and return a new result. HError equivalent of "catching" an exception.
recover :: Result es a -> Handler es es' a -> Result es' a
recover (Left e) f = f e
recover (Right x) _ = Right x

class Recovers es fs es' a | es fs -> es' where
  recovers :: Result es a -> H.HList fs -> Result es' a

-- | Delete all members of the type-list @l@ from the type-list @m@.
class HDeleteAll (l :: [*])  (m :: [*]) (m' :: [*]) | l m -> m'

instance (H.HDeleteMany l (H.HList m) (H.HList m'), HDeleteAll l' m' m'') => HDeleteAll (l ': l') m m''

instance HDeleteAll l '[] '[]

sliceVariant :: (H.SplitVariant x xl xr, HDeleteAll xl x xr) =>
                V.Variant x ->
                Either (V.Variant xl) (V.Variant xr)
sliceVariant = H.splitVariant

instance (H.SplitVariant es (e :^: es') os,
          HDeleteAll (e :^: es') es os,
          Recovers os fs es'' a) => Recovers es (Handler (e :^: es') es'' a ': fs) es'' a where
  recovers (Left (Error (T.TIC v))) fs = case sliceVariant v of
    Left e -> H.hHead fs . Error $ T.TIC e
    Right o -> recovers (Left (Error (T.TIC o))) $ H.hTail fs
  recovers (Right x) _ = Right x

instance Recovers es '[] es a where
  recovers x _ = x

value :: Result '[] a -> a
value (Right x) = x
value (Left _) = error "HError internal error: enexpected inhabitant of `Error '[]`."
