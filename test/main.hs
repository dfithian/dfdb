import ClassyPrelude
import Test.Hspec (hspec)

import qualified DFDB.TreeSpec
import qualified DFDB.TransactionSpec

main :: IO ()
main = hspec $ do
  DFDB.TreeSpec.spec
  DFDB.TransactionSpec.spec
