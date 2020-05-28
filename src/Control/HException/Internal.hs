{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.HException.Internal where

import qualified Control.Exception     as E
import qualified Data.HList.CommonMain as H
import qualified Data.HList.TIC        as T
import qualified Data.HList.TIP        as TP
import qualified Data.HList.Variant    as V
import           GHC.TypeLits          (KnownNat)
import           Type.Reflection       (Typeable)

-- This module provides an internal interface which enables de-constructing a HException into a TIC. Type classes
-- instances for HExceptions are also defined here
newtype HException es = HException { unHException :: H.TIC es }

type e :^: es = H.Tagged e e ': es
infixr 7 :^:

type Only e = e :^: '[]

type HException1 e = HException (Only e)

instance V.ShowVariant (e ': es) => Show (HException (e ': es)) where
  show = ("HException" ++) . drop 3 . show . unHException

-- We need this instance so we can show 'Value's
instance Show (HException '[]) where
  show _ = error "HException internal error: show called on 'HException '[]'"

deriving instance Eq (HException '[])
deriving instance (Eq e, Eq (H.Variant es)) => Eq (HException (e ': es))

deriving instance Ord (HException '[])
deriving instance (Ord e, Ord (V.Variant es)) => Ord (HException (e ': es))

type TypeIndexed xs = (TP.HAllTaggedEq xs, H.HLabelSet (H.LabelsOf xs), H.HAllTaggedLV xs)

type CoProductMember x xs n = (H.HasField x (H.Record xs) x,
                               H.HFind1 x (H.UnLabel x (H.LabelsOf xs)) (H.UnLabel x (H.LabelsOf xs)) n,
                               KnownNat (H.HNat2Nat n))

hException :: (CoProductMember e es n, TypeIndexed es) => e -> HException es
hException = HException . H.mkTIC

instance (E.Exception e,
          E.Exception (HException (e' :^: es)),
          Typeable (e' :^: es),
          TypeIndexed (e :^: e' :^: es),
          V.ShowVariant (e :^: e' :^: es)) => E.Exception (HException (e :^: e' :^: es)) where
  toException (HException (T.TIC v)) = case H.splitVariant1 v of
    Left e   -> E.toException e
    Right es -> E.toException . HException $ T.TIC es

  fromException se = case E.fromException se of
    Just (e :: e) -> Just $ hException e
    Nothing -> (\(HException (T.TIC v)) -> HException . T.TIC $ H.extendVariant v) <$> E.fromException se

instance E.Exception e => E.Exception (HException (Only e)) where
  toException (HException (T.TIC v)) = E.toException $ V.unvariant v

  fromException se = case E.fromException se of
    Just (e :: e) -> Just $ hException e
    Nothing       -> Nothing
