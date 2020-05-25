{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

import qualified Control.Exception as CE
import           Data.Char         (isAlpha)
import           HError            ((:^:))
import qualified HError            as H
import qualified System.Exit       as E
import qualified Test.HUnit        as HUnit

newtype SimpleError = SimpleError { unSimpleError ::  String } deriving (Eq, Ord, Show)
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

    wordsDivision :: String -> Int -> H.Result (CE.ArithException :^: SimpleError :^: '[]) Double
    wordsDivision str n = do
      numWords <- H.extend $ processStr str
      H.extend $ safeDivide numWords n

testExtend :: HUnit.Test
testExtend = HUnit.TestLabel "extend" . HUnit.TestCase $ do
  let simple1 :: H.Result1 SimpleError Int = H.raise $ SimpleError "error1"
  let extended1a :: H.Result (SimpleError :^: IntError :^: '[]) Int = H.extend simple1
      extended1b :: H.Result (IntError :^: SimpleError :^: '[]) Int = H.extend simple1
  let extended2 :: H.Result (CE.ArithException :^: SimpleError :^: IntError :^: '[]) Int = H.extend extended1a
      expectedShow = "Left Error{simpleError=SimpleError \"error1\"}"
  HUnit.assertEqual "Show extended1a" expectedShow $ show extended1a
  HUnit.assertEqual "Show extended1b" expectedShow $ show extended1b
  HUnit.assertEqual "Show extended2" expectedShow $ show extended2

testGet :: HUnit.Test
testGet = HUnit.TestLabel "do" . HUnit.TestCase $ do
  let simple1 :: H.Error1 SimpleError = H.err $ SimpleError "error1"
  let extended1 :: H.Error (SimpleError :^: IntError :^: '[]) = H.generalize simple1
      extended2 :: H.Error (IntError :^: SimpleError :^: '[]) = H.err $ IntError 1
  HUnit.assertEqual "get simple1" (SimpleError "error1") $ H.get simple1
  HUnit.assertEqual "getMay (Just) extended1" (Just $ SimpleError "error1") $ H.getMay extended1
  HUnit.assertEqual "getMay (Just) extended2" (Just $ IntError 1) $ H.getMay extended2
  HUnit.assertEqual "getMay (Nothing) extended2" (Nothing :: Maybe SimpleError) $ H.getMay extended2

testRaise :: HUnit.Test
testRaise = HUnit.TestLabel "raise" . HUnit.TestCase $ do
  let simple1 :: H.Result1 SimpleError Int = H.raise $ SimpleError "error1"
      simple2 :: H.Result1 SimpleError Int = H.raise $ SimpleError "error2"
  HUnit.assertBool "Error not equal to non-error" (simple1 /= Right 1)
  HUnit.assertBool "Error not equal to different error" (simple1 /= simple2)

  let composite1 :: H.Result (SimpleError :^: IntError :^: '[]) Int = H.raise $ SimpleError "error1"
      composite2 :: H.Result (SimpleError :^: IntError :^: '[]) Int = H.raise $ IntError 1
  HUnit.assertEqual "Show composite1" "Left Error{simpleError=SimpleError \"error1\"}" $ show composite1
  HUnit.assertEqual "Show composite2" "Left Error{intError=IntError 1}" $ show composite2

testRecover :: HUnit.Test
testRecover = HUnit.TestLabel "raise" . HUnit.TestCase $ do
  let simpleErr :: H.Result (SimpleError :^: IntError :^: '[]) Bool = H.raise $ SimpleError "error1"
      intErr :: H.Result (SimpleError :^: IntError :^: '[]) Bool = H.raise $ IntError 1
      good :: H.Result (SimpleError :^: IntError :^: '[]) Bool = Right True
  HUnit.assertEqual "handle simpleErr" (Right False) $ simpleErr `H.recover` simpleHandler
  HUnit.assertEqual "handle intErr" (H.raise CE.Overflow) $ intErr `H.recover` simpleHandler
  HUnit.assertEqual "handle good" (Right True) $ good `H.recover` simpleHandler

  let arithErr :: H.Result (CE.ArithException :^: SimpleError :^: IntError :^: '[]) Bool = H.raise CE.Overflow
--      simpleErrExt :: H.Result (CE.ArithException :^: SimpleError :^: IntError :^: '[]) Bool = H.extend simpleErr
--      intErrExt :: H.Result (CE.ArithException :^: SimpleError :^: IntError :^: '[]) Bool = H.extend intErr
--      goodExt :: H.Result (CE.ArithException :^: SimpleError :^: IntError :^: '[]) Bool = Right True

  HUnit.assertEqual "" (Right False) $
    arithErr `H.recovers` handleSimple `H.orElse` handleArithAndInt `H.orElse` H.done

  where
    handleSimple :: H.Handler (H.Only SimpleError) (H.Only String) Bool
    handleSimple = H.raise . unSimpleError . H.get

    handleArithAndInt :: H.Handler (CE.ArithException :^: IntError :^: '[]) (H.Only String) Bool
    handleArithAndInt e = case H.getMay e of
      Just CE.Overflow -> Right False
      Just x -> H.raise $ show x
      Nothing -> H.raise "not arith"

    simpleHandler :: H.Error (SimpleError :^: IntError :^: '[]) -> H.Result1 CE.ArithException Bool
    simpleHandler e = case H.getMay e of
      Just (SimpleError _) -> Right False
      Nothing -> H.raise CE.Overflow

main :: IO ()
main = do
  counts <- HUnit.runTestTT $ HUnit.TestList [testDoExample, testExtend, testGet, testRaise, testRecover]
  if HUnit.errors counts > 0 || HUnit.failures counts > 0 then
    E.exitFailure
  else
    E.exitSuccess
