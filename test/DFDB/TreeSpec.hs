module DFDB.TreeSpec where

import ClassyPrelude
import Test.Hspec (HasCallStack, Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===), Gen, arbitrary, elements, forAll, listOf)

import DFDB.Tree (Color, Tree)
import qualified DFDB.Tree as Tree

genColor :: Gen Color
genColor = elements [Tree.Black, Tree.Red]

genTree :: Ord a => Gen a -> Gen (Tree a)
genTree gen = Tree.fromList <$> listOf gen

genIntTree :: Gen (Tree Int)
genIntTree = genTree arbitrary

checkBinary :: (HasCallStack, Ord a) => Tree a -> Bool
checkBinary = \ case
  Tree.Nil -> True
  Tree.Node x _ tl tr -> all (\ l -> l < x) (Tree.toList tl) && all (\ r -> r > x) (Tree.toList tr)

checkRedNodeChildrenAreBlack :: HasCallStack => Tree a -> Bool
checkRedNodeChildrenAreBlack = \ case
  Tree.Nil -> True
  Tree.Node _ Tree.Black tl tr -> checkRedNodeChildrenAreBlack tl && checkRedNodeChildrenAreBlack tr
  Tree.Node _ Tree.Red tl tr -> case (tl, tr) of
    (Tree.Nil, Tree.Nil) -> True
    (Tree.Nil, Tree.Node _ Tree.Black rtl rtr) -> checkRedNodeChildrenAreBlack rtl && checkRedNodeChildrenAreBlack rtr
    (Tree.Node _ Tree.Black ltl ltr, Tree.Nil) -> checkRedNodeChildrenAreBlack ltl && checkRedNodeChildrenAreBlack ltr
    (Tree.Node _ Tree.Black ltl ltr, Tree.Node _ Tree.Black rtl rtr) ->
      checkRedNodeChildrenAreBlack ltl && checkRedNodeChildrenAreBlack ltr && checkRedNodeChildrenAreBlack rtl && checkRedNodeChildrenAreBlack rtr
    _ -> False

checkPathCount :: HasCallStack => Tree a -> Bool
checkPathCount t =
  let go = \ case
        Tree.Nil -> (1 :: Int, True)
        Tree.Node _ c tl tr ->
          let (il, bl) = go tl
              (ir, br) = go tr
              j = if c == Tree.Black then 1 + il else il
          in (j, bl && br && il == ir)
  in snd $ go t

runTests :: (HasCallStack, Ord a, Show b) => String -> Gen b -> (b -> Tree a) -> Spec
runTests d gen f = do
  prop (d <> ": binary") $ forAll gen $ checkBinary . f
  prop (d <> ": red children") $ forAll gen $ checkRedNodeChildrenAreBlack . f
  prop (d <> ": path count") $ forAll gen $ checkPathCount . f

spec :: Spec
spec = describe "Tree" $ do
  runTests "generation" genIntTree id
  runTests "insertion" ((,) <$> arbitrary <*> genIntTree) $ uncurry Tree.insert
  runTests "deletion" genIntTree $ \ t -> case Tree.rootMay t of
    Nothing -> Tree.Nil
    Just i -> Tree.delete i t
  runTests "merge" ((,) <$> genIntTree <*> genIntTree) $ uncurry Tree.merge
  prop "toList == fold (:)" $
    forAll genIntTree $ \ t -> Tree.toList t === Tree.fold (:) [] t
  prop "membership" $
    forAll genIntTree $ \ t -> all (`Tree.member` t) $ Tree.toList t
