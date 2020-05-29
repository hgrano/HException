import qualified Control.HException.Test          as H
import qualified Control.Monad.Trans.HExcept.Test as HE
import qualified System.Exit                      as E
import qualified Test.HUnit                       as HUnit

main :: IO ()
main = do
  counts <- HUnit.runTestTT $ HUnit.TestList [H.suite, HE.suite]
  if HUnit.errors counts > 0 || HUnit.failures counts > 0 then
    E.exitFailure
  else
    E.exitSuccess
