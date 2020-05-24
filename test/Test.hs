{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import qualified Control.Exception as CE
import Data.Char (isAlpha)
import HError ((:^:))
import qualified HError as H
import qualified System.Exit as E
import qualified Test.HUnit as HUnit

newtype SimpleError = SimpleError String deriving (Eq, Ord, Show)
newtype IntError = IntError Int deriving (Eq, Ord, Show)

testDoExample :: HUnit.Test
testDoExample = HUnit.TestLabel "do" . HUnit.TestCase $ do
  HUnit.assertEqual "Invalid string" (H.raise $ SimpleError "Invalid string") $ wordsDivision "hello1" 1
  HUnit.assertEqual "Divide by zero" (H.raise CE.DivideByZero) $ wordsDivision "hello" 0
  HUnit.assertEqual "Valid" (Right 0.5) $ wordsDivision "hello" 2

  where
    processStr :: String -> H.Result1 SimpleError Int
    processStr str
      | all isAlpha str = return . length $ words str
      | otherwise = H.raise $ SimpleError "Invalid string"

    safeDivide :: Int -> Int -> H.Result1 CE.ArithException Double
    safeDivide x y
      | y /= 0 = return $ fromIntegral x / fromIntegral y
      | otherwise = H.raise CE.DivideByZero

    wordsDivision :: String -> Int -> H.Result (CE.ArithException :^: SimpleError :^: H.Nil) Double
    wordsDivision str n = do
      numWords <- H.extend $ processStr str
      H.extend $ safeDivide numWords n

testExtend :: HUnit.Test
testExtend = HUnit.TestLabel "extend" . HUnit.TestCase $ do
  let simple1 :: H.Result1 SimpleError Int = H.raise $ SimpleError "error1"
  let extended1a :: H.Result (SimpleError :^: IntError :^: H.Nil) Int = H.extend simple1
      extended1b :: H.Result (IntError :^: SimpleError :^: H.Nil) Int = H.extend simple1
  let extended2 :: H.Result (CE.ArithException :^: SimpleError :^: IntError :^: H.Nil) Int = H.extend extended1a
      expectedShow = "Left (Error TIC{simpleError=SimpleError \"error1\"})"
  HUnit.assertEqual "Show extended1a" expectedShow $ show extended1a
  HUnit.assertEqual "Show extended1b" expectedShow $ show extended1b
  HUnit.assertEqual "Show extended2" expectedShow $ show extended2

testRaise :: HUnit.Test
testRaise = HUnit.TestLabel "raise" . HUnit.TestCase $ do
  let simple1 :: H.Result1 SimpleError Int = H.raise $ SimpleError "error1"
      simple2 :: H.Result1 SimpleError Int = H.raise $ SimpleError "error2"
  HUnit.assertBool "Error not equal to non-error" (simple1 /= Right 1)
  HUnit.assertBool "Error not equal to different error" (simple1 /= simple2)

  let composite1 :: H.Result (SimpleError :^: IntError :^: H.Nil) Int = H.raise $ SimpleError "error1"
      composite2 :: H.Result (SimpleError :^: IntError :^: H.Nil) Int = H.raise $ IntError 1
  HUnit.assertEqual "Show composite1" "Left (Error TIC{simpleError=SimpleError \"error1\"})" $ show composite1
  HUnit.assertEqual "Show composite2" "Left (Error TIC{intError=IntError 1})" $ show composite2

main :: IO ()
main = do
  counts <- HUnit.runTestTT $ HUnit.TestList [testDoExample, testExtend, testRaise]
  if HUnit.errors counts > 0 || HUnit.failures counts > 0 then
    E.exitFailure
  else
    E.exitSuccess
