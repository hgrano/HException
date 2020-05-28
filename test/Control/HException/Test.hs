{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.HException.Test (suite) where

import qualified Control.Exception  as CE
import           Control.HException ((:^:), HException, HException1)
import qualified Control.HException as H
import qualified Test.HUnit         as HUnit

testGet :: HUnit.Test
testGet = HUnit.TestLabel "do" . HUnit.TestCase $ do
  let simple1 :: HException1 CE.ArithException = H.hException CE.DivideByZero
  let extended1 :: HException (CE.ArithException :^: CE.AsyncException :^: '[]) = H.generalize simple1
      extended2 :: HException (CE.AsyncException :^: CE.ArithException :^: '[]) = H.hException CE.StackOverflow
  HUnit.assertEqual "get simple1" CE.DivideByZero $ H.get simple1
  HUnit.assertEqual "getMay (Just) extended1" (Just CE.DivideByZero) $ H.getMay extended1
  HUnit.assertEqual "getMay (Just) extended2" (Just CE.StackOverflow) $ H.getMay extended2
  HUnit.assertEqual "getMay (Nothing) extended2" (Nothing :: Maybe CE.ArithException) $ H.getMay extended2

testTry :: HUnit.Test
testTry = HUnit.TestLabel "abc" . HUnit.TestCase $ do
  t1 <- CE.try . CE.throwIO $ CE.DivideByZero
  HUnit.assertEqual
    "catch HException"
    (Left (H.hException CE.DivideByZero) :: Either (HException1 CE.ArithException) ())
    t1

  t2 <- CE.try $ CE.throwIO (H.hException CE.DivideByZero :: HException1 CE.ArithException)
  HUnit.assertEqual "catch ArithException" (Left CE.DivideByZero :: Either CE.ArithException ()) t2

  t3 <- CE.try . CE.throwIO $ CE.DivideByZero
  HUnit.assertEqual
    "catch HException (2)"
    (Left (H.hException CE.DivideByZero) :: Either (HException (CE.ArithException :^: CE.IOException :^: '[])) ())
    t3

  t4 <- CE.try $ CE.throwIO (H.hException CE.DivideByZero :: HException (CE.ArithException :^: CE.IOException :^: '[]))
  HUnit.assertEqual "catch ArithException (2)" (Left CE.DivideByZero :: Either CE.ArithException ()) t4

  t5 <- CE.try . CE.throwIO $ CE.DivideByZero
  HUnit.assertEqual
    "catch HException (2) - reveresed"
    (Left (H.hException CE.DivideByZero) :: Either (HException (CE.IOException :^: CE.ArithException :^: '[])) ())
    t5

  t6 <- CE.try $ CE.throwIO (H.hException CE.DivideByZero :: HException (CE.IOException :^: CE.ArithException :^: '[]))
  HUnit.assertEqual "catch ArithException (2) - reversed" (Left CE.DivideByZero :: Either CE.ArithException ()) t6

  let a :: IO (Either (HException1 CE.ArithException) ()) = CE.try . CE.throwIO $ CE.StackOverflow
  x <- CE.try a
  HUnit.assertEqual "we missed it" (Left CE.StackOverflow) x

suite :: HUnit.Test
suite = HUnit.TestList [testGet, testTry]
