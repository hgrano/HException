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

-- | A heterogeneously typed 'E.Exception', which holds an underlying exception that is statically constrained
-- to be one of the type-level list @es@.
newtype HException es = HException { unHException :: H.TIC es }

-- | Type alias for ease of constructing exception type-level lists.
type e :^: es = H.Tagged e e ': es
infixr 7 :^:

-- | A singleton exception type-level list.
type Only e = e :^: '[]

-- | The type of 'HException's containing only one type of exception.
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

-- | The type-level list @xs@ is constrained to contain only distinct types, and so can be indexed via type alone.
type TypeIndexed xs = (TP.HAllTaggedEq xs, H.HLabelSet (H.LabelsOf xs), H.HAllTaggedLV xs)

-- | The type @x@ is contained in the type-level list @xs@ at index @n@.
type MemberAt x xs n = (H.HasField x (H.Record xs) x,
                      H.HFind1 x (H.UnLabel x (H.LabelsOf xs)) (H.UnLabel x (H.LabelsOf xs)) n,
                      KnownNat (H.HNat2Nat n))

-- | Construct a 'HException' from a standard exception value.
hException :: (MemberAt e es n, TypeIndexed es) => e -> HException es
hException = HException . H.mkTIC

-- | The type @x@ is contained in the type-level list @xs@.
type Member x xs = (H.HasField x (H.TIC xs) (Maybe x))

-- | Extract an exception of a specific type from a 'HException'. Returns 'Nothing' if the exception encased is
-- not of the required type.
getMay :: Member e es => HException es -> Maybe e
getMay = H.hOccurs . unHException

-- | Specialization of 'getMay' for cases where there is only a single type of exception.
get :: HException1 e -> e
get (HException (T.TIC v)) = H.unvariant v

-- | Constrain that the type-level list @xs@ contains all of the elements of the type-level list @xs'@.
type Subset xs xs' = V.ExtendsVariant xs xs'

-- | Changes the type of a 'HException' so it is more general than the original, without changing the underlying
-- exception.
generalize :: (Subset es es', TypeIndexed es') => HException es -> HException es'
generalize (HException (T.TIC v)) = HException . T.TIC $ H.extendsVariant v

-- | The 'E.Exception' instance for @HException es@ is able to catch any 'E.Exception's thrown whose type is in @es@.
-- The instance throws the underlying 'E.Exception' contained in the 'HException'.
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
  toException = E.toException . get

  fromException se = case E.fromException se of
    Just (e :: e) -> Just $ hException e
    Nothing       -> Nothing
