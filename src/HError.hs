{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

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
import qualified Data.HList.TIC as T
import qualified Data.HList.TIP as TP
import qualified Data.HList.Variant as V
import GHC.TypeLits (KnownNat)

newtype Error e = Error (T.TIC e) deriving (Show)

deriving instance Eq (Error '[])
deriving instance (Eq e, Eq (V.Variant es)) => Eq (Error (e ': es))

deriving instance Ord (Error '[])
deriving instance (Ord e, Ord (V.Variant es)) => Ord (Error (e ': es))

type e :^: es = H.Tagged e e ': es
infixr 7 :^:

type Nil = '[]

type Result e a = Either (Error e) a

type Result1 e a = Result (e :^: Nil) a

-- | The type-list @l@ has no duplicate types.
type TypeIndexed l = (TP.HAllTaggedEq l, H.HLabelSet (H.LabelsOf l), H.HAllTaggedLV l)

extend :: (TypeIndexed f, V.ExtendsVariant e f) => Result e a -> Result f a
extend (Left (Error (T.TIC v))) = Left . Error . T.TIC $ H.extendsVariant v
extend (Right x) = Right x

raise :: (H.HAllTaggedEq es,
          H.HAllTaggedLV es,
          H.HasField e (H.Record es) e,
          H.HFind1 e (H.UnLabel e (H.LabelsOf es)) (H.UnLabel e (H.LabelsOf es)) n,
          H.HLabelSet (H.LabelsOf es),
          KnownNat (H.HNat2Nat n),
          TypeIndexed es) =>
          e -> Result es a
raise = Left . Error . H.mkTIC
