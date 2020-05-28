{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

import qualified Control.HException.Internal as I
import qualified Data.HList.CommonMain       as H
import qualified Data.HList.TIC              as T
import qualified Data.HList.Variant          as V
import           GHC.TypeLits                (KnownNat)

-- | Type for containing an error which maybe one of known list of error types @es@.
type HException = I.HException

-- | Type alias for ease of constructing 'HException's.
type e :^: es = e I.:^: es

-- | A singleton set of error types.
type Only e = I.Only e

-- | An error of only one type.
type HException1 e = I.HException1 e

-- | Constrain that the type-list @xs@ has no duplicate types, and so can be indexed via type only.
type TypeIndexed xs = I.TypeIndexed xs

-- | The type @x@ exists in @xs@ at index @n@.
type CoProductMember x xs n = (H.HasField x (H.Record xs) x,
                               H.HFind1 x (H.UnLabel x (H.LabelsOf xs)) (H.UnLabel x (H.LabelsOf xs)) n,
                               KnownNat (H.HNat2Nat n))

-- | Lift a standard exception value into a 'HException'.
hException :: (CoProductMember e es n, TypeIndexed es) => e -> HException es
hException = I.HException . H.mkTIC

-- Constraint such that the type @x@ may exist in the co-product of types @xs@.
type Gettable xs x = (H.HasField x (H.TIC xs) (Maybe x))

-- | Extract an error of a specific type from an 'Error'. Returns @Nothing@ if the error encased is not of the required
-- type. Calls to this function will not type check if the type @e@ does not exist in the type-list @es@.
getMay :: Gettable es e => HException es -> Maybe e
getMay = H.hOccurs . I.unHException

-- | Specialization of 'getMay' for cases where there is only a single type of error.
get :: HException1 e -> e
get (I.HException (T.TIC v)) = H.unvariant v

-- | Constrain that the input type list @xs@ contains all of the elements of the output type-list @xs'@.
type Generalizable xs xs' = V.ExtendsVariant xs xs'

-- | Changes the type of an 'HException' so it is more general than the original. The underlying stored value
-- is left unchanged. For most use cases 'extend' will likely be more suitable than 'generalize'. Note: this can also
-- be used to re-order the error types.
generalize :: (Generalizable es es', TypeIndexed es') => HException es -> HException es'
generalize (I.HException (T.TIC v)) = I.HException . T.TIC $ H.extendsVariant v
