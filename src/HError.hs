{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module HError(
  Error,
  extend,
  Nil,
  raise,
  Result,
  Result1,
  TypeIndexed,
  (:^:)
) where

import qualified Data.HList.CommonMain as H
import qualified Data.HList.TIC        as T
import qualified Data.HList.TIP        as TP
import qualified Data.HList.Variant    as V
import           GHC.TypeLits          (KnownNat)

-- * Basic types and data types.

-- | Type for containing an error which maybe one of known list of error types @e@.
newtype Error e = Error (T.TIC e) deriving (Show)

deriving instance Eq (Error '[])
deriving instance (Eq e, Eq (V.Variant es)) => Eq (Error (e ': es))

deriving instance Ord (Error '[])
deriving instance (Ord e, Ord (V.Variant es)) => Ord (Error (e ': es))

-- | Type alias for ease of constructing errors.
type e :^: es = H.Tagged e e ': es
infixr 7 :^:

-- | An empty set of error types. To be used in conjunction with ':^:'.
type Nil = '[]

-- | Type for returning results from computations which may fail with one of a known set of errors or return a value.
type Result e a = Either (Error e) a

-- | The type of a computation result which may only return one type of error.
type Result1 e a = Result (e :^: Nil) a

-- | Constrain that the type-list @l@ has no duplicate types, and so can be indexed via type only.
type TypeIndexed l = (TP.HAllTaggedEq l, H.HLabelSet (H.LabelsOf l), H.HAllTaggedLV l)

-- * Combinator functions for working with 'Result`s.

-- | Extend the given result so that it may used in a context which should return a superset of the errors that may
-- arise from the original result. This is useful for calling multiple functions which return different errors types
-- from within a single function.
extend :: (TypeIndexed f, V.ExtendsVariant e f) => Result e a -> Result f a
extend (Left (Error (T.TIC v))) = Left . Error . T.TIC $ H.extendsVariant v
extend (Right x)                = Right x

-- | Return an error result using the provided error.
raise :: (H.HAllTaggedEq es,
          H.HAllTaggedLV es,
          H.HasField e (H.Record es) e,
          H.HFind1 e (H.UnLabel e (H.LabelsOf es)) (H.UnLabel e (H.LabelsOf es)) n,
          H.HLabelSet (H.LabelsOf es),
          KnownNat (H.HNat2Nat n),
          TypeIndexed es) =>
          e -> Result es a
raise = Left . Error . H.mkTIC
