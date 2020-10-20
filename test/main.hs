import ClassyPrelude
import Test.Hspec (hspec)

import qualified DFDB.DatabaseSpec
import qualified DFDB.TransactionSpec
import qualified DFDB.TreeSpec

main :: IO ()
main = hspec $ do
  DFDB.DatabaseSpec.spec
  DFDB.TransactionSpec.spec
  DFDB.TreeSpec.spec
