{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.Trans.HExcept(
  HExcept,
  HExcept1,
  HExceptT,
  HExceptT1,
  hThrowE,
  Handler,
  HandlerT,
  HCatchesE(..),
  orElse,
  Done,
  done,
  orDefault,
  Value,
  ValueT,
  value
) where

import           Control.HException          (HException)
import qualified Control.HException          as H
import qualified Control.HException.Internal as I
import qualified Control.Monad.Trans.Except  as TE
import           Data.Functor.Identity       (Identity)
import qualified Data.HList.CommonMain       as HC
import qualified Data.HList.TIC              as T
import qualified Data.HList.Variant          as V

-- | The heterogeneous exception monad. Computations are either one of a known set of exceptions @es@ or a value @a@.
type HExcept es = TE.Except (HException es)

-- | A specialization of 'HExcept' for computations which can return only one type of exception.
type HExcept1 e = HExcept (H.Only e)

-- | A monad transformer that adds heterogeneous exceptions into other monads.
type HExceptT es = TE.ExceptT (HException es)

-- | A specialization of 'HExceptT' for computations which can return only one type of exception.
type HExceptT1 e = HExceptT (H.Only e)

-- | Signal an exception value @e@.
hThrowE :: (H.MemberAt e es n, Monad m, H.TypeIndexed es) => e -> HExceptT es m a
hThrowE = TE.throwE . H.hException

-- | The type of functions used to handle the exceptions @es@, which may return the exceptions @es'@.
type Handler es es' a = HandlerT es es' Identity a

-- | The monad transformer version of 'Handler'.
type HandlerT es es' m a = HException es -> HExceptT es' m a

-- | Type class needed for heterogeneous and polymorphic  exception handling. Do not create instances of this class.
class Monad m => HCatchesE es fs es' m a | es fs -> es' m a where
  -- | Handle exceptions using a heterogeneous list of 'HandlerT's, and (optionally) a default value.
  hCatchesE :: HExceptT es m a -> HC.HList fs -> HExceptT es' m a
  infixr 1 `hCatchesE`

-- | Delete all members of the type-level list @l@ from the type-level list @m@.
class DeleteAll (l :: [*])  (m :: [*]) (m' :: [*]) | l m -> m'

instance (HC.HDeleteMany l (HC.HList m) (HC.HList m'), DeleteAll l' m' m'') => DeleteAll (l ': l') m m''

instance DeleteAll '[] m m

sliceVariant :: (HC.SplitVariant x xl xr, DeleteAll xl x xr) => HC.Variant x -> Either (HC.Variant xl) (HC.Variant xr)
sliceVariant = HC.splitVariant

instance (HC.SplitVariant es es' os,
          DeleteAll es' es os,
          HCatchesE os (HandlerT x x' m a ': fs) es'' m a,
          H.TypeIndexed es'') => HCatchesE es (HandlerT es' es'' m a ': HandlerT x x' m a ': fs) es'' m a where
  hCatchesE r fs = TE.catchE r $ \(I.HException (T.TIC v)) -> case sliceVariant v of
    Left e  -> HC.hHead fs . I.HException $ T.TIC e
    Right o -> hCatchesE (TE.throwE . I.HException $ T.TIC o) $ HC.hTail fs

instance (HC.SameLength es es', HC.ExtendsVariant es es', Monad m, H.TypeIndexed es'') =>
         HCatchesE es '[HandlerT es' es'' m a] es'' m a where
  hCatchesE r fs = TE.catchE r $ \(I.HException (T.TIC v)) ->
    HC.hHead fs . I.HException . T.TIC $ V.rearrangeVariant v

instance (DeleteAll es' es os,  -- Constrain this so that the instance only applies if not all  exceptions have been handled
         HC.HLengthGe os ('HC.HSucc 'HC.HZero), -- using this length constraint.
         HC.ProjectVariant es es',
         Monad m,
         H.TypeIndexed es'') =>
         HCatchesE es '[HandlerT es' es'' m a, HExceptT es'' m a] es'' m a where
  hCatchesE r fs = TE.catchE r $ \(I.HException (T.TIC v)) -> case HC.projectVariant v of
    Just e  -> HC.hHead fs . I.HException $ T.TIC e
    Nothing -> HC.hLast fs

-- | Chain 'HandlerT's together.
orElse :: HandlerT es es' m a -> HC.HList fs -> HC.HList (HandlerT es es' m a ': fs)
orElse = HC.HCons
infixr 2 `orElse`

-- | Type indicating all  exception types have been handled.
type Done = HC.HList '[]

-- | Use this at the end of a chain of 'Handler's to assert (at compile time) complete coverage of all  exception types.
done :: Done
done = HC.HNil

-- | An alternative to 'done' which attempts to use the provided 'Handler', or if the 'Handler' does not apply,
-- returns a default 'Result'.
orDefault :: HandlerT es es' m a -> HExceptT es' m a -> HC.HList '[HandlerT es es' m a, HExceptT es' m a]
orDefault h r = h `HC.HCons` r `HC.HCons` HC.HNil

-- | A specialization of 'HExcept' for computations in which all possible exception types have been dealt with using
-- 'TE.catchE' or 'hCatchesE'. This means the domain of possible  exceptions is empty.
type Value = HExcept '[]

-- | The monad transformer version of 'Value'.
type ValueT = HExceptT '[]

-- | Extract the value from a computation result after all exceptions have been handled. This is type-safe because the
-- type of  exceptions is constrained to be empty.
value :: Monad m => ValueT m a -> m a
value v = do
  x <- TE.runExceptT v
  case x of
    Right r -> return r
    Left _ -> error "HException internal error: unexpected error inside a 'Value'"
