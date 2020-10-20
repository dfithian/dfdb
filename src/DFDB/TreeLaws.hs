module DFDB.TreeLaws where

import ClassyPrelude
import Test.Hspec (HasCallStack)

import DFDB.Tree (Tree)
import qualified DFDB.Tree as Tree

checkBinary :: (HasCallStack, Ord a) => Tree a -> Bool
checkBinary = \ case
  Tree.Nil -> True
  Tree.Node x _ _ tl tr -> all (\ l -> l < x) (Tree.setToList tl) && all (\ r -> r > x) (Tree.setToList tr)

checkColor :: HasCallStack => Tree a -> Bool
checkColor = \ case
  Tree.Nil -> True
  Tree.Node _ _ Tree.Red (Tree.Node _ _ Tree.Red _ _) _ -> False
  Tree.Node _ _ Tree.Red _ (Tree.Node _ _ Tree.Red _ _) -> False
  Tree.Node _ _ _ tl tr -> checkColor tl && checkColor tr

checkPathCount :: HasCallStack => Tree a -> Bool
checkPathCount t =
  let go = \ case
        Tree.Nil -> (0 :: Int, True)
        Tree.Node _ _ c tl tr ->
          let (il, bl) = go tl
              (ir, br) = go tr
              j = if c == Tree.Black then 1 + il else il
          in (j, bl && br && il == ir)
  in snd $ go t
