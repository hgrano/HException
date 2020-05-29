{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Monad.Trans.HExcept.Test (suite) where

import qualified Control.Exception           as CE
import           Control.HException          ((:^:))
import qualified Control.HException          as H
import qualified Control.Monad.Trans.HExcept as HE
import           Data.Char                   (isAlpha)
import qualified Test.HUnit                  as HUnit

newtype SimpleError = SimpleError { unSimpleError :: String } deriving (Eq, Ord, Show)
instance CE.Exception SimpleError

newtype IntError = IntError Int deriving (Eq, Ord, Show)
instance CE.Exception IntError

suite :: HUnit.Test
suite = HUnit.TestList [testDoExample, testRecover, testHThrowE, testValue]

testDoExample :: HUnit.Test
testDoExample = HUnit.TestLabel "do" . HUnit.TestCase $ do
  HUnit.assertEqual "Invalid string" (HE.hThrowE $ SimpleError "Invalid string") $ wordsDivision "hello1" 1
  HUnit.assertEqual "Divide by zero" (HE.hThrowE CE.DivideByZero) $ wordsDivision "hello" 0
  HUnit.assertEqual "Valid" (return 0.5) $ wordsDivision "hello" 2

  where
    processStr :: String -> HE.HExcept1 SimpleError Int
    processStr str
      | all isAlpha str = return . length $ words str
      | otherwise = HE.hThrowE $ SimpleError "Invalid string"

    safeDivide :: Int -> Int -> HE.HExcept1 CE.ArithException Double
    safeDivide x y
      | y /= 0 = return $ fromIntegral x / fromIntegral y
      | otherwise = HE.hThrowE CE.DivideByZero

    wordsDivision :: String -> Int -> HE.HExcept (CE.ArithException :^: SimpleError :^: '[]) Double
    wordsDivision str n = do
      numWords <- HE.extend $ processStr str
      HE.extend $ safeDivide numWords n

testHThrowE :: HUnit.Test
testHThrowE = HUnit.TestLabel "raise" . HUnit.TestCase $ do
  let simple1 :: HE.HExcept1 SimpleError Int = HE.hThrowE $ SimpleError "error1"
      simple2 :: HE.HExcept1 SimpleError Int = HE.hThrowE $ SimpleError "error2"
  HUnit.assertBool "HException not equal to non-error" (simple1 /= return 1)
  HUnit.assertBool "HException not equal to different error" (simple1 /= simple2)

  let composite1 :: HE.HExcept (SimpleError :^: IntError :^: '[]) Int = HE.hThrowE $ SimpleError "error1"
      composite2 :: HE.HExcept (SimpleError :^: IntError :^: '[]) Int = HE.hThrowE $ IntError 1
  HUnit.assertEqual
    "Show composite1" "ExceptT (Identity (Left HException{simpleError=SimpleError {unSimpleError = \"error1\"}}))"
    (show composite1)
  HUnit.assertEqual "Show composite2" "ExceptT (Identity (Left HException{intError=IntError 1}))" $ show composite2

testRecover :: HUnit.Test
testRecover = HUnit.TestLabel "raise" . HUnit.TestCase $ do
  let simpleErr :: HE.HExcept (SimpleError :^: IntError :^: '[]) Bool = HE.hThrowE $ SimpleError "error1"
      intErr :: HE.HExcept (SimpleError :^: IntError :^: '[]) Bool = HE.hThrowE $ IntError 1
      good :: HE.HExcept (SimpleError :^: IntError :^: '[]) Bool = return True
  HUnit.assertEqual "handle simpleErr" (return False) $ simpleErr `HE.hCatchesE` basicHandler `HE.orElse` HE.done
  HUnit.assertEqual "handle intErr" (HE.hThrowE CE.Overflow) $ intErr `HE.hCatchesE` basicHandler `HE.orElse` HE.done
  HUnit.assertEqual "handle good" (return True) $ good `HE.hCatchesE` basicHandler `HE.orElse` HE.done

  let overFlowErr :: HE.HExcept (CE.ArithException :^: SimpleError :^: IntError :^: '[]) Bool = HE.hThrowE CE.Overflow
      simpleErrExt :: HE.HExcept (CE.ArithException :^: SimpleError :^: IntError :^: '[]) Bool = HE.extend simpleErr
      goodExt :: HE.HExcept (CE.ArithException :^: SimpleError :^: IntError :^: '[]) Bool = return True
  HUnit.assertEqual "Recovers arithErr" (return False) $
    overFlowErr `HE.hCatchesE` handleSimple `HE.orElse` handleArithAndInt `HE.orElse` HE.done
  HUnit.assertEqual "Recovers arithErr (reversed)" (return False) $
    overFlowErr `HE.hCatchesE` handleArithAndInt `HE.orElse` handleSimple `HE.orElse` HE.done
  HUnit.assertEqual "Recovers arithErr (minimal)" (return False) $
    overFlowErr `HE.hCatchesE` handleArithAndInt `HE.orDefault` return True
  HUnit.assertEqual "Recovers arithErr (default)" (HE.hThrowE "Arithmetic") $
    overFlowErr `HE.hCatchesE` handleInt `HE.orElse` handleSimple `HE.orDefault` HE.hThrowE "Arithmetic"
  HUnit.assertEqual "Recovers simpleErrExt" (HE.hThrowE "error1") $
    simpleErrExt `HE.hCatchesE` handleSimple `HE.orDefault` HE.hThrowE "fall through"
  HUnit.assertEqual "Recovers goodExt" (return True) $
    goodExt `HE.hCatchesE` handleSimple `HE.orElse` handleArithAndInt `HE.orElse` HE.done

  where
    handleInt :: HE.Handler (H.Only IntError) (H.Only String) Bool
    handleInt e = if H.get e == IntError 0 then return True else HE.hThrowE "Non-zero"

    handleSimple :: HE.Handler (H.Only SimpleError) (H.Only String) Bool
    handleSimple = HE.hThrowE . unSimpleError . H.get

    handleArithAndInt :: HE.Handler (CE.ArithException :^: IntError :^: '[]) (H.Only String) Bool
    handleArithAndInt e = case H.getMay e of
      Just CE.Overflow -> return False
      Just x           -> HE.hThrowE $ show x
      Nothing          -> HE.hThrowE "not arith"

    basicHandler :: H.HException (SimpleError :^: IntError :^: '[]) -> HE.HExcept1 CE.ArithException Bool
    basicHandler e = case H.getMay e of
      Just (SimpleError _) -> return False
      Nothing              -> HE.hThrowE CE.Overflow

testValue :: HUnit.Test
testValue = HUnit.TestLabel "raise" . HUnit.TestCase $
  HUnit.assertEqual "value" 1 (HE.value (return 1 :: HE.Value Int))
