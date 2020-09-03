module DFDB.TreeSpec where

import ClassyPrelude
import Test.Hspec (HasCallStack, Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===), Gen, arbitrary, forAll, listOf)

import DFDB.Tree (Tree)
import DFDB.TreeLaws (checkBinary, checkColor, checkPathCount)
import qualified DFDB.Tree as Tree

genTree :: Ord a => Gen a -> Gen (Tree a)
genTree gen = Tree.fromList <$> listOf gen

genIntTree :: Gen (Tree Int)
genIntTree = genTree arbitrary

runTests :: (HasCallStack, Ord a, Show b) => String -> Gen b -> (b -> Tree a) -> Spec
runTests d gen f = do
  prop (d <> ": binary") $ forAll gen $ checkBinary . f
  prop (d <> ": color") $ forAll gen $ checkColor . f
  prop (d <> ": path count") $ forAll gen $ checkPathCount . f

spec :: Spec
spec = describe "Tree" $ do
  prop "toList == fold (:)" $
    forAll genIntTree $ \ t -> Tree.toList t === Tree.fold (:) [] t
  prop "membership" $
    forAll genIntTree $ \ t -> all (`Tree.member` t) $ Tree.toList t

  runTests "generation" genIntTree id
  runTests "insertion" ((,) <$> arbitrary <*> genIntTree) $ uncurry Tree.insert
