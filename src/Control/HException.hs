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

module Control.HException(
  HException,
  (:^:),
  Only,
  HException1,
  TypeIndexed,
  CoProductMember,
  hException,
  Gettable,
  getMay,
  get,
  generalize
) where

import qualified Control.Exception     as E
import qualified Data.HList.CommonMain as H
import qualified Data.HList.TIC        as T
import qualified Data.HList.TIP        as TP
import qualified Data.HList.Variant    as V
import           GHC.TypeLits          (KnownNat)
import           Type.Reflection       (Typeable)

-- | Type for containing an error which maybe one of known list of error types @es@.
newtype HException es = HException { unHException :: H.TIC es }

instance V.ShowVariant (e ': es) => Show (HException (e ': es)) where
  show = ("HException" ++) . drop 3 . show . unHException

-- We need this instance so we can show 'Value's
instance Show (HException '[]) where
  show _ = error "HException internal error: show called on 'HException '[]'"

deriving instance Eq (HException '[])
deriving instance (Eq e, Eq (H.Variant es)) => Eq (HException (e ': es))

deriving instance Ord (HException '[])
deriving instance (Ord e, Ord (V.Variant es)) => Ord (HException (e ': es))

-- | Type alias for ease of constructing 'HException's.
type e :^: es = H.Tagged e e ': es
infixr 7 :^:

-- | A singleton set of error types.
type Only e = e :^: '[]

-- | An error of only one type.
type HException1 e = HException (Only e)

-- | Constrain that the type-list @xs@ has no duplicate types, and so can be indexed via type only.
type TypeIndexed xs = (TP.HAllTaggedEq xs, H.HLabelSet (H.LabelsOf xs), H.HAllTaggedLV xs)

-- | The type @x@ exists in @xs@ at index @n@.
type CoProductMember x xs n = (H.HasField x (H.Record xs) x,
                               H.HFind1 x (H.UnLabel x (H.LabelsOf xs)) (H.UnLabel x (H.LabelsOf xs)) n,
                               KnownNat (H.HNat2Nat n))

-- | Lift a standard exception value into a 'HException'.
hException :: (CoProductMember e es n, TypeIndexed es) => e -> HException es
hException = HException . H.mkTIC

-- Constraint such that the type @x@ may exist in the co-product of types @xs@.
type Gettable xs x = (H.HasField x (H.TIC xs) (Maybe x))

-- | Extract an error of a specific type from an 'Error'. Returns @Nothing@ if the error encased is not of the required
-- type. Calls to this function will not type check if the type @e@ does not exist in the type-list @es@.
getMay :: Gettable es e => HException es -> Maybe e
getMay = H.hOccurs . unHException

-- | Specialization of 'getMay' for cases where there is only a single type of error.
get :: HException1 e -> e
get (HException (T.TIC v)) = H.unvariant v

-- | Constrain that the input type list @xs@ contains all of the elements of the output type-list @xs'@.
type Generalizable xs xs' = V.ExtendsVariant xs xs'

-- | Changes the type of an 'HException' so it is more general than the original. The underlying stored value
-- is left unchanged. For most use cases 'extend' will likely be more suitable than 'generalize'. Note: this can also
-- be used to re-order the error types.
generalize :: (Generalizable es es', TypeIndexed es') => HException es -> HException es'
generalize (HException (T.TIC v)) = HException . T.TIC $ H.extendsVariant v

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
