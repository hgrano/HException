{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module HError(
  -- * Basic types
  Error,
  (:^:),
  Only,
  Error1,
  Result,
  Result1,
  Value,
  TypeIndexed,
  Handler,
  Done,
  -- * Pure functions for working with @Result@s
  generalize,
  extend,
  getMay,
  get,
  err,
  raise,
  recover,
  Recovers(..),
  DeleteAll,
  orElse,
  done,
  orDefault,
  value,
  -- * Handling 'Error's from the IO monad
  Attempt(attempt)
) where

import qualified Control.Exception     as E
import qualified Data.HList.CommonMain as H
import qualified Data.HList.TIC        as T
import qualified Data.HList.TIP        as TP
import qualified Data.HList.Variant    as V
import           Data.Proxy            (Proxy(Proxy))
import           GHC.TypeLits          (KnownNat)

-- | Type for containing an error which maybe one of known list of error types @es@.
newtype Error es = Error { unError :: T.TIC es }

instance V.ShowVariant (e ': es) => Show (Error (e ': es)) where
  show = ("Error" ++) . drop 3 . show . unError

-- We need this instance so we can show 'Value's
instance Show (Error '[]) where
  show _ = error "HError internal error: show called on 'Error '[]'"

deriving instance Eq (Error '[])
deriving instance (Eq e, Eq (V.Variant es)) => Eq (Error (e ': es))

deriving instance Ord (Error '[])
deriving instance (Ord e, Ord (V.Variant es)) => Ord (Error (e ': es))

-- | Type alias for ease of constructing 'Error's.
type e :^: es = H.Tagged e e ': es
infixr 7 :^:

-- | A singleton set of error types.
type Only e = e :^: '[]

-- | An error of only one type.
type Error1 e = Error (Only e)

-- | Type for returning results from computations which may fail with one of a known set of errors @es@ or return a
-- value @a@.
type Result es a = Either (Error es) a

-- | A specialization of 'Result' for computations which can return only one type of error.
type Result1 e a = Either (Error1 e) a

-- | A specialization of 'Result' for computations in which all possible error types have been dealt with using
-- 'handle' or 'handles'. This means the domain of possible errors is empty.
type Value a = Result '[] a

-- | Constrain that the type-list @xs@ has no duplicate types, and so can be indexed via type only.
type TypeIndexed xs = (TP.HAllTaggedEq xs, H.HLabelSet (H.LabelsOf xs), H.HAllTaggedLV xs)

-- | Changes the type of an 'Error' so it is more general than the original. The underlying stored value
-- is left unchanged. For most use cases 'extend' will likely be more suitable than 'generalize'. Note: this can also
-- be used to re-order the error types.
generalize :: (TypeIndexed es', V.ExtendsVariant es es') => Error es -> Error es'
generalize (Error (T.TIC v)) = Error . T.TIC $ H.extendsVariant v

-- | Extend the given result so that it may be used in a context which can return a superset of the errors that may
-- arise from the original result. This is useful for calling multiple functions which return different errors types
-- from within a single function. Note: this can also be used to re-order the error types.
extend :: (TypeIndexed es', V.ExtendsVariant es es') => Result es a -> Result es' a
extend (Left e)  = Left $ generalize e
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
get (Error (T.TIC v))= H.unvariant v

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

-- | The type of functions used to handle errors.
type Handler es es' a = Error es -> Result es' a

-- | Handle errors and return a new result. HError equivalent of "catching" an exception.
recover :: Result es a -> Handler es es' a -> Result es' a
recover (Left e) f  = f e
recover (Right x) _ = Right x

class Recovers es fs es' a | es fs -> es' a where
  -- | Handle errors using a heterogeneous list of 'Handler's, and (optionally) a default value.
  recovers :: Result es a -> H.HList fs -> Result es' a
  infixr 1 `recovers`

-- | Delete all members of the type-list @l@ from the type-list @m@.
class DeleteAll (l :: [*])  (m :: [*]) (m' :: [*]) | l m -> m'

instance (H.HDeleteMany l (H.HList m) (H.HList m'), DeleteAll l' m' m'') => DeleteAll (l ': l') m m''

instance DeleteAll '[] m m

sliceVariant :: (H.SplitVariant x xl xr, DeleteAll xl x xr) => V.Variant x -> Either (V.Variant xl) (V.Variant xr)
sliceVariant = H.splitVariant

instance (H.SplitVariant es es' os,
          DeleteAll es' es os,
          Recovers os (Handler x x' a ': fs) es'' a,
          TypeIndexed es'') => Recovers es (Handler es' es'' a ': Handler x x' a ': fs) es'' a where
  recovers (Left (Error (T.TIC v))) fs = case sliceVariant v of
    Left e  -> H.hHead fs . Error $ T.TIC e
    Right o -> recovers (Left (Error (T.TIC o))) $ H.hTail fs
  recovers (Right x) _ = Right x

instance (H.SameLength es es', H.ExtendsVariant es es', TypeIndexed es'') =>
         Recovers es '[Handler es' es'' a] es'' a where
  recovers (Left (Error (T.TIC v))) fs = H.hHead fs . Error . T.TIC $ V.rearrangeVariant v
  recovers (Right x) _ = Right x

instance (DeleteAll es' es os,  -- Constrain this so that the instance only applies if not all errors have been handled
         H.HLengthGe os ('H.HSucc 'H.HZero), -- using this length constraint.
         H.ProjectVariant es es',
         TypeIndexed es'') =>
         Recovers es '[Handler es' es'' a, Result es'' a] es'' a where
  recovers (Left (Error (T.TIC v))) fs = case H.projectVariant v of
    Just e  -> H.hHead fs . Error $ T.TIC e
    Nothing -> H.hLast fs
  recovers (Right x) _ = Right x

-- | Chain 'Handler's together.
orElse :: Handler es es' a -> H.HList fs -> H.HList (Handler es es' a ': fs)
orElse = H.HCons

infixr 2 `orElse`

-- | Type indicating all error types have been handled.
type Done = H.HList '[]

-- | Use this at the end of a chain of 'Handler's to indicate complete coverage of all error types.
done :: Done
done = H.HNil

-- | An alternative to @`orElse` done@ which attempts to use the provided 'Handler', or if the 'Handler' does not apply,
-- returns a default 'Result'.
orDefault :: Handler es es' a -> Result es' a -> H.HList '[Handler es es' a, Result es' a]
orDefault h r = h `H.HCons` r `H.HCons` H.HNil

-- | Extract the value from a computation result after all errors have been handled. This is type-safe because the
-- type of errors is constrained to be empty.
value :: Value a -> a
value (Right x) = x
value (Left _) = error "HError internal error: unexpected error inside a 'Value'"

--instance (Typeable e, Typeable es, V.ShowVariant (e ': es)) => E.Exception (Error (e ': es)) where

class Attempt es where
  handleSome :: proxy es -> E.SomeException -> IO (Result es a)

  -- | Attempt the given IO action: if one of the 'E.Exception's in @es@ is thrown then it will be returned on the
  -- @Left@ of the 'Result', if any other 'E.Exception' is thrown it will not be handled, otherwise the @Right@ is
  -- returned.
  attempt :: IO a -> IO (Result es a)
  attempt action = (Right <$> action) `E.catch` handleSome (Proxy :: Proxy es)

instance (Attempt es, E.Exception e, TypeIndexed (e :^: es), V.ExtendsVariant es (e :^: es)) => Attempt (e :^: es) where
  handleSome _ s = case E.fromException s of
    Just (x :: e) -> return $ raise x
    Nothing -> do
      r <- handleSome (Proxy :: Proxy es) s
      return $ extend r

instance Attempt '[] where
  handleSome _ = E.throwIO
