module DFDB.Tree where

import Prelude hiding (map)

data Color = Red | Black
  deriving (Eq, Ord, Show)

data Tree a = Node a Color (Tree a) (Tree a) | Nil
  deriving (Eq, Ord, Show)

empty :: Tree a
empty = Nil

singleton :: a -> Tree a
singleton x = Node x Black Nil Nil

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert empty

toList :: Tree a -> [a]
toList = \ case
  Nil -> []
  Node x _ tl tr -> toList tl <> [x] <> toList tr

rootMay :: Tree a -> Maybe a
rootMay = \ case
  Nil -> Nothing
  Node x _ _ _ -> Just x

map :: (a -> b) -> Tree a -> Tree b
map f = \ case
  Nil -> Nil
  Node x c tl tr -> Node (f x) c (map f tl) (map f tr)

fold :: (a -> b -> b) -> b -> Tree a -> b
fold f y = \ case
  Nil -> y
  Node x _ tl tr -> fold f (f x (fold f y tr)) tl

-- FIXME
repair :: Ord a => Tree a -> Tree a
repair = id

merge :: Ord a => Tree a -> Tree a -> Tree a
merge l = fold insert l

-- FIXME
insert :: Ord a => a -> Tree a -> Tree a
insert x = \ case
  Nil -> Node x Red Nil Nil
  Node y c tl tr -> case compare x y of
    EQ -> Node y c tl tr
    LT -> Node y c (insert x tl) tr
    GT -> Node y c tl (insert x tr)

-- FIXME
delete :: Ord a => a -> Tree a -> Tree a
delete x = \ case
  Nil -> Nil
  Node y c tl tr -> case compare x y of
    EQ -> merge tl tr
    LT -> Node y c (delete x tl) tr
    GT -> Node y c tl (delete x tr)

member :: Ord a => a -> Tree a -> Bool
member x = \ case
  Nil -> False
  Node y _ tl tr -> x == y || (if x > y then member x tr else member x tl)
