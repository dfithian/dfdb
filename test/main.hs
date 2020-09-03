import ClassyPrelude
import Test.Hspec (hspec)

import qualified DFDB.TreeSpec

main :: IO ()
main = hspec $ do
  DFDB.TreeSpec.spec
