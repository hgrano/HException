{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.Trans.HExcept where

import           Control.HException          (HException)
import qualified Control.HException          as H
import qualified Control.HException.Internal as I
import qualified Control.Monad.Trans.Except  as TE
import           Data.Functor.Identity       (Identity)
import qualified Data.HList.CommonMain       as HC
import qualified Data.HList.TIC              as T
import qualified Data.HList.Variant          as V

-- | Type for returning results from pure computations which may fail with one of a known set of errors @es@ or return a
-- value @a@.
type HExcept es = TE.Except (HException es)

-- | A specialization of 'Result' for computations which can return only one type of error.
type HExcept1 e = HExcept (H.Only e)

-- | A generalization of 'Result' for computations lifted into a monad @m@.
type HExceptT es = TE.ExceptT (HException es)

-- | A specialization of 'HExceptT' for computations which can return only one type of error.
type HExceptT1 e = HExceptT (H.Only e)

-- | A specialization of 'Result' for computations in which all possible error types have been dealt with using
-- 'recover' or 'hCatchesE'. This means the domain of possible errors is empty.
type Value = HExcept '[]

-- | A generalization of 'Value' for computations lifted into a monad @m@.
type ValueT = HExceptT '[]

-- | Return an error result using the provided error. HException equivalent of "throwing" an exception - but a value is
-- returned instead of being thrown.
hThrowE :: (H.CoProductMember e es n, Monad m, H.TypeIndexed es) => e -> HExceptT es m a
hThrowE = TE.throwE . H.hException

-- | The type of functions used to handle errors.
type HandlerT es es' m a = HException es -> HExceptT es' m a

type Handler es es' a = HandlerT es es' Identity a

class Monad m => HCatchesE es fs es' m a | es fs -> es' m a where
  -- | Handle errors using a heterogeneous list of 'Handler's, and (optionally) a default value.
  hCatchesE :: HExceptT es m a -> HC.HList fs -> HExceptT es' m a
  infixr 1 `hCatchesE`

-- | Delete all members of the type-list @l@ from the type-list @m@.
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

instance (DeleteAll es' es os,  -- Constrain this so that the instance only applies if not all errors have been handled
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

-- | Type indicating all error types have been handled.
type Done = HC.HList '[]

-- | Use this at the end of a chain of 'Handler's to assert (at compile time) complete coverage of all error types.
done :: Done
done = HC.HNil

-- | An alternative to 'done' which attempts to use the provided 'Handler', or if the 'Handler' does not apply,
-- returns a default 'Result'.
orDefault :: HandlerT es es' m a -> HExceptT es' m a -> HC.HList '[HandlerT es es' m a, HExceptT es' m a]
orDefault h r = h `HC.HCons` r `HC.HCons` HC.HNil

-- | Extract the value from a computation result after all errors have been handled. This is type-safe because the
-- type of errors is constrained to be empty.
value :: Monad m => ValueT m a -> m a
value v = do
  x <- TE.runExceptT v
  case x of
    Right r -> return r
    Left _ -> error "HException internal error: unexpected error inside a 'Value'"
