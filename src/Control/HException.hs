-- | Heterogeneously typed exceptions.

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
  I.HException,
  (I.:^:),
  I.Only,
  I.HException1,
  I.Concat,
  I.TypeIndexed,
  I.Member,
  I.hException,
  I.getMay,
  I.get,
  I.Slice,
  I.slice,
  I.Subset,
  I.generalize,
  I.TransformSubset(..),
  I.mapSubset
) where

import qualified Control.HException.Internal as I
