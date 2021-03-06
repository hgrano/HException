-- | Functions for handling 'HException's using 'Control.Monad.Trans.Except.ExceptT'.

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Trans.HExcept(
  HExcept,
  HExcept1,
  HExceptT,
  HExceptT1,
  hExceptT,
  extend,
  hThrowE,
  Handler,
  HandlerT,
  handler1,
  handlerT,
  catchSubset,
  orElse,
  orDefault,
  Value,
  ValueT,
  value,
  valueT
) where

import           Control.HException          (HException)
import qualified Control.HException          as H
import qualified Control.HException.Internal as I
import qualified Control.Monad.Trans.Except  as TE
import           Data.Functor.Identity       (Identity (runIdentity))
import qualified Data.HList.CommonMain       as HC
import qualified Data.HList.TIC              as T

-- | The heterogeneous exception monad. Computations are either one of a known set of exceptions @es@ or a value.
type HExcept es = TE.Except (HException es)

-- | A specialization of 'HExcept' for computations which can return only one type of exception.
type HExcept1 e = HExcept (H.Only e)

-- | A monad transformer that adds heterogeneous exceptions into other monads.
type HExceptT es = TE.ExceptT (HException es)

-- | A specialization of 'HExceptT' for computations which can return only one type of exception.
type HExceptT1 e = HExceptT (H.Only e)

-- | Lift a 'HExcept' into its transformer version.
hExceptT :: Applicative m => HExcept es a -> HExceptT es m a
hExceptT = TE.mapExceptT (pure . runIdentity)

-- | Extend the given result so that it may be used in a context which can return a superset of the errors that may
-- arise from the original result. This is useful for calling multiple functions which return different errors types
-- from within a single function. Note: this can also be used to re-order the error types.
extend :: (Functor m, H.Subset es es', H.TypeIndexed es') => HExceptT es m a -> HExceptT es' m a
extend = TE.withExceptT H.generalize

-- | Signal an exception value @e@.
hThrowE :: (H.Member e es, Monad m, H.TypeIndexed es) => e -> HExceptT es m a
hThrowE = TE.throwE . H.hException

-- | The type of functions used to handle the exceptions @es@, which may return the exceptions @es'@.
type Handler es es' a = HandlerT es es' Identity a

-- | The monad transformer version of 'Handler'.
type HandlerT es es' m a = HException es -> HExceptT es' m a

-- | Construct a handler from a function which operates on the underlying exception type.
handler1 :: (e -> HExceptT es m a) -> HandlerT (H.Only e) es m a
handler1 f = f . H.get

-- | Lift a (pure) handler into @m@.
handlerT :: Applicative m => Handler es es' a -> HandlerT es es' m a
handlerT f = hExceptT . f

-- | Similar to 'TE.catchE' but instead of needing to handle all types of exceptions, we can handle a specific subset
-- of the exception types. Exceptions that are not handled are carried through to the output without transformation.
catchSubset :: (H.TransformSubset es sub sub' es', Monad m) =>
               HExceptT es m a ->
               (HException sub -> HExceptT es' m a) ->
               HExceptT es' m a
catchSubset compute f = TE.catchE compute (swapExceptT . H.transformSubset f')
  where
    f' = swapExceptT . f

    swapExceptT = TE.mapExceptT (fmap swapEither)

    swapEither (Left x)  = Right x
    swapEither (Right x) = Left x

-- | Chain 'HandlerT's together.
orElse :: (H.Slice es'' es es', H.TypeIndexed es, H.TypeIndexed es') =>
           HandlerT es os m a ->
           HandlerT es' os m a ->
           HandlerT es'' os m a
orElse f g e = either f g $ H.slice e
infixr 2 `orElse`

-- | Try to use the provided 'HandlerT' or, if that handler does not apply, return the default.
orDefault :: HC.ProjectVariant es' es => HandlerT es os m a -> HExceptT os m a -> HandlerT es' os m a
orDefault f d (I.HException (T.TIC  v)) = case HC.projectVariant v of
  Just x  -> f . I.HException $ T.TIC x
  Nothing -> d

-- | A specialization of 'HExcept' for computations in which all possible exception types have been dealt with. This
-- means the domain of possible  exceptions is empty.
type Value = HExcept '[]

-- | The monad transformer version of 'Value'.
type ValueT = HExceptT '[]

-- | Extract the value from a computation result after all exceptions have been handled. This is type-safe because the
-- type of  exceptions is constrained to be empty.
value :: Value a -> a
value = runIdentity . valueT

-- | Monad transformer version of 'value'.
valueT :: Monad m => ValueT m a -> m a
valueT v = do
  x <- TE.runExceptT v
  case x of
    Right r -> return r
    Left _ -> error "HException internal error: unexpected error inside a 'ValueT'"
