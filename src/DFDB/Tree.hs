module DFDB.Tree
  ( Tree(Node, Nil), Color(Red, Black)
  , empty, singleton, toList, fromList, map, fold
  , insert, member
  ) where

import ClassyPrelude hiding (delete, empty, fold, fromList, map, member, singleton, toList)
import Control.Monad (fail)
import Data.Aeson (Value(Null, String), (.:), (.=), FromJSON, ToJSON, object, parseJSON, toJSON, withObject, withText)

data Color = Red | Black
  deriving (Eq, Ord)

instance Show Color where
  show = \ case
    Red -> "red"
    Black -> "black"

instance ToJSON Color where
  toJSON = \ case
    Red -> String "red"
    Black -> String "black"

instance FromJSON Color where
  parseJSON = withText "Color" $ \ case
    "red" -> pure Red
    "black" -> pure Black
    other -> fail $ "Unknown color " <> unpack other

data Tree a = Node a Color !(Tree a) !(Tree a) | Nil
  deriving (Eq, Ord)

showTree :: Show a => Int -> Tree a -> String
showTree depth = \ case
  Nil -> spaces <> "nil" <> "\n"
  Node x c l r -> spaces <> "node " <> show x <> " " <> show c <> "\n"
    <> showTree (depth + 1) l
    <> showTree (depth + 1) r
  where
    spaces = mconcat $ replicate depth "  "

instance Show a => Show (Tree a) where
  show = showTree 0

instance ToJSON a => ToJSON (Tree a) where
  toJSON = \ case
    Nil -> Null
    Node x c tl tr -> object
      [ "value" .= x
      , "color" .= c
      , "left" .= tl
      , "right" .= tr
      ]

instance FromJSON a => FromJSON (Tree a) where
  parseJSON = \ case
    Null -> pure Nil
    other -> flip (withObject "Tree") other $ \ obj -> Node
      <$> obj .: "value"
      <*> obj .: "color"
      <*> obj .: "left"
      <*> obj .: "right"

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

map :: (a -> b) -> Tree a -> Tree b
map f = \ case
  Nil -> Nil
  Node x c tl tr -> Node (f x) c (map f tl) (map f tr)

fold :: (a -> b -> b) -> b -> Tree a -> b
fold f y = \ case
  Nil -> y
  Node x _ tl tr -> fold f (f x (fold f y tr)) tl

balance :: a -> Color -> Tree a -> Tree a -> Tree a
balance z Black (Node y Red (Node x Red a b) c) d = Node y Red (Node x Black a b) (Node z Black c d)
balance z Black (Node x Red a (Node y Red b c)) d = Node y Red (Node x Black a b) (Node z Black c d)
balance x Black a (Node z Red (Node y Red b c) d) = Node y Red (Node x Black a b) (Node z Black c d)
balance x Black a (Node y Red b (Node z Red c d)) = Node y Red (Node x Black a b) (Node z Black c d)
balance x c a b = Node x c a b

blackRoot :: Tree a -> Tree a
blackRoot = \ case
  Nil -> Nil
  Node x _ tl tr -> Node x Black tl tr

insert :: Ord a => a -> Tree a -> Tree a
insert x = blackRoot . unsafeInsert
  where
    unsafeInsert = \ case
      Nil -> Node x Red Nil Nil
      Node y c tl tr -> case compare x y of
        EQ -> Node y c tl tr
        LT -> balance y c (unsafeInsert tl) tr
        GT -> balance y c tl (unsafeInsert tr)

member :: Ord a => a -> Tree a -> Bool
member x = \ case
  Nil -> False
  Node y _ tl tr -> x == y || (if x > y then member x tr else member x tl)
