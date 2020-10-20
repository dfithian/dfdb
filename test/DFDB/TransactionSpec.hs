module DFDB.TransactionSpec where

import ClassyPrelude
import Control.Monad.Except (Except, throwError)
import Control.Monad.State (StateT, modify, runState)
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified DFDB.Transaction as Transaction

type TestTransaction = Transaction.Transaction' Int Text

runTestTransaction :: StateT Int (Except Text) () -> (Either Text (), Int)
runTestTransaction t = runState (Transaction.runTransaction (Transaction.Transaction t)) 0

spec :: Spec
spec = describe "Transaction" $ do
  it "updates state" $
    runTestTransaction (modify (+1)) `shouldBe` (Right (), 1)

  it "rolls back state" $
    runTestTransaction (modify (+1) >> throwError "foo") `shouldBe` (Left "foo", 0)
