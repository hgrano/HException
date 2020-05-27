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
  ResultT,
  ResultT1,
  Value,
  TypeIndexed,
  Handler,
  Done,
  -- * General functions for working with @Result@s
  Generalizable,
  generalize,
  extend,
  Gettable,
  getMay,
  get,
  CoProductMember,
  err,
  raise,
  Recovers(..),
  DeleteAll,
  orElse,
  done,
  orDefault,
  value,
  -- * Handling @Error@s from the @IO@ monad
  Panic(..),
  Attempt(attempt)
) where

import qualified Control.Exception     as E
import qualified Control.Monad.Trans.Except as TE
import           Data.Functor.Identity (Identity)
import qualified Data.HList.CommonMain as H
import qualified Data.HList.TIC        as T
import qualified Data.HList.TIP        as TP
import qualified Data.HList.Variant    as V
import           Data.Proxy            (Proxy (Proxy))
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

-- | Type for returning results from pure computations which may fail with one of a known set of errors @es@ or return a
-- value @a@.
type Result es = ResultT es Identity

-- | A specialization of 'Result' for computations which can return only one type of error.
type Result1 e = Result (Only e)

-- | A generalization of 'Result' for computations lifted into a monad @m@.
type ResultT es m = TE.ExceptT (Error es) m

-- | A specialization of 'ResultT' for computations which can return only one type of error.
type ResultT1 e m = ResultT (Only e) m

-- | A specialization of 'Result' for computations in which all possible error types have been dealt with using
-- 'recover' or 'recovers'. This means the domain of possible errors is empty.
type Value = ValueT Identity

-- | A generalization of 'Value' for computations lifted into a monad @m@.
type ValueT m = ResultT '[] m

-- | Constrain that the type-list @xs@ has no duplicate types, and so can be indexed via type only.
type TypeIndexed xs = (TP.HAllTaggedEq xs, H.HLabelSet (H.LabelsOf xs), H.HAllTaggedLV xs)

-- | Constrain that the input type list @xs@ contains all of the elements of the output type-list @xs'@.
type Generalizable xs xs' = V.ExtendsVariant xs xs'

-- | Changes the type of an 'Error' so it is more general than the original. The underlying stored value
-- is left unchanged. For most use cases 'extend' will likely be more suitable than 'generalize'. Note: this can also
-- be used to re-order the error types.
generalize :: (Generalizable es es', TypeIndexed es') => Error es -> Error es'
generalize (Error (T.TIC v)) = Error . T.TIC $ H.extendsVariant v

-- | Extend the given result so that it may be used in a context which can return a superset of the errors that may
-- arise from the original result. This is useful for calling multiple functions which return different errors types
-- from within a single function. Note: this can also be used to re-order the error types.
extend :: (Generalizable es es', Monad m, TypeIndexed es') => ResultT es m a -> ResultT es' m a
extend r = TE.catchE r (TE.throwE .  generalize)

-- Constraint such that the type @x@ may exist in the co-product of types @xs@.
type Gettable xs x = (H.HasField x (H.TIC xs) (Maybe x))

-- | Extract an error of a specific type from an 'Error'. Returns @Nothing@ if the error encased is not of the required
-- type. Calls to this function will not type check if the type @e@ does not exist in the type-list @es@.
getMay :: Gettable es e => Error es -> Maybe e
getMay = H.hOccurs . unError

-- | Specialization of 'getMay' for cases where there is only a single type of error.
get :: Error1 e -> e
get (Error (T.TIC v)) = H.unvariant v

-- | The type @x@ exists in @xs@ at index @n@.
type CoProductMember x xs n = (H.HasField x (H.Record xs) x,
                               H.HFind1 x (H.UnLabel x (H.LabelsOf xs)) (H.UnLabel x (H.LabelsOf xs)) n,
                               KnownNat (H.HNat2Nat n))

-- | Lift a standard exception value into an 'Error'.
err :: (CoProductMember e es n, TypeIndexed es) => e -> Error es
err = Error . H.mkTIC

-- | Return an error result using the provided error. HError equivalent of "throwing" an exception - but a value is
-- returned instead of being thrown.
raise :: (CoProductMember e es n, Monad m, TypeIndexed es) => e -> ResultT es m a
raise = TE.throwE . err

-- | The type of functions used to handle errors.
type HandlerT es es' m a = Error es -> ResultT es' m a

type Handler es es' a = HandlerT es es' Identity a

class Monad m => Recovers es fs es' m a | es fs -> es' m a where
  -- | Handle errors using a heterogeneous list of 'Handler's, and (optionally) a default value.
  recovers :: ResultT es m a -> H.HList fs -> ResultT es' m a
  infixr 1 `recovers`

-- | Delete all members of the type-list @l@ from the type-list @m@.
class DeleteAll (l :: [*])  (m :: [*]) (m' :: [*]) | l m -> m'

instance (H.HDeleteMany l (H.HList m) (H.HList m'), DeleteAll l' m' m'') => DeleteAll (l ': l') m m''

instance DeleteAll '[] m m

sliceVariant :: (H.SplitVariant x xl xr, DeleteAll xl x xr) => V.Variant x -> Either (V.Variant xl) (V.Variant xr)
sliceVariant = H.splitVariant

instance (H.SplitVariant es es' os,
          DeleteAll es' es os,
          Recovers os (HandlerT x x' m a ': fs) es'' m a,
          TypeIndexed es'') => Recovers es (HandlerT es' es'' m a ': HandlerT x x' m a ': fs) es'' m a where
  recovers r fs = TE.catchE r $ \(Error (T.TIC v)) -> case sliceVariant v of
    Left e  -> H.hHead fs . Error $ T.TIC e
    Right o -> recovers (TE.throwE . Error $ T.TIC o) $ H.hTail fs

instance (H.SameLength es es', H.ExtendsVariant es es', Monad m, TypeIndexed es'') =>
         Recovers es '[HandlerT es' es'' m a] es'' m a where
  recovers r fs = TE.catchE r $ \(Error (T.TIC v)) ->
    H.hHead fs . Error . T.TIC $ V.rearrangeVariant v

instance (DeleteAll es' es os,  -- Constrain this so that the instance only applies if not all errors have been handled
         H.HLengthGe os ('H.HSucc 'H.HZero), -- using this length constraint.
         H.ProjectVariant es es',
         Monad m,
         TypeIndexed es'') =>
         Recovers es '[HandlerT es' es'' m a, ResultT es'' m a] es'' m a where
  recovers r fs = TE.catchE r $ \(Error (T.TIC v)) -> case H.projectVariant v of
    Just e  -> H.hHead fs . Error $ T.TIC e
    Nothing -> H.hLast fs

-- | Chain 'HandlerT's together.
orElse :: HandlerT es es' m a -> H.HList fs -> H.HList (HandlerT es es' m a ': fs)
orElse = H.HCons
infixr 2 `orElse`

-- | Type indicating all error types have been handled.
type Done = H.HList '[]

-- | Use this at the end of a chain of 'Handler's to assert (at compile time) complete coverage of all error types.
done :: Done
done = H.HNil

-- | An alternative to @`orElse` done@ which attempts to use the provided 'Handler', or if the 'Handler' does not apply,
-- returns a default 'Result'.
orDefault :: HandlerT es es' m a -> ResultT es' m a -> H.HList '[HandlerT es es' m a, ResultT es' m a]
orDefault h r = h `H.HCons` r `H.HCons` H.HNil

-- | Extract the value from a computation result after all errors have been handled. This is type-safe because the
-- type of errors is constrained to be empty.
value :: Monad m => ValueT m a -> m a
value v = do
  x <- TE.runExceptT v
  case x of
    Right r -> return r
    Left _ -> error "HError internal error: unexpected error inside a 'Value'"

class Panic es where
  -- | Uses 'E.throw' to throw the 'E.Exception' contained in the 'Error'. Where possible, it is advised to use pure
  -- error handling (i.e. 'raise' instead of 'panic'). This function acts as a back up where impure 'E.Exception'
  -- handling is required.
  panic :: Error es -> IO a

instance (E.Exception e, Panic (e' :^: es)) => Panic (e :^: e' :^: es) where
  panic (Error (T.TIC v)) = case H.splitVariant1 v of
    Left x   -> E.throwIO x
    Right es -> panic . Error $ T.TIC es

instance E.Exception e => Panic (Only e) where
  panic = E.throwIO . get

class Attempt es where
  handleSome :: proxy es -> E.SomeException -> ResultT es IO a

  -- | Attempt the given IO action: if one of the 'E.Exception's in @es@ is thrown then it will be returned on the
  -- @Left@ of the 'Result', if any other 'E.Exception' is thrown it will not be handled, otherwise the @Right@ is
  -- returned.
  attempt :: IO a -> ResultT es IO a
  attempt action = TE.ExceptT $ (Right <$> action) `E.catch` (TE.runExceptT . handleSome (Proxy :: Proxy es))

instance (Attempt es, E.Exception e, TypeIndexed (e :^: es)) => Attempt (e :^: es) where
  handleSome _ s = case E.fromException s of
    Just (x :: e) -> raise x
    Nothing -> TE.withExceptT
      (\(Error (T.TIC v)) -> Error . T.TIC $ H.extendVariant v)
      (handleSome (Proxy :: Proxy es) s)

instance Attempt '[] where
  handleSome _ = TE.ExceptT . fmap Right . E.throwIO
