module DFDB.Tree
  ( TreeMap(Node, Nil), Tree, Color(Red, Black)
  , empty, singletonMap, singletonSet, mapToList, setToList, mapFromList, mapFromListWith, setFromList
  , map, mapKeys, mapValues, fold, foldKeys
  , insertMap, insertMapWith, insertSet, lookup, member
  ) where

import ClassyPrelude hiding
  ( delete, empty, fold, fromList, insertMap, insertSet, lookup, map, mapFromList, mapToList, member
  , setFromList, setToList, singleton, singletonMap, singletonSet, toList
  )
import Control.Monad (fail)
import Data.Aeson (Value(Null, String), (.:), (.=), FromJSON, ToJSON, object, parseJSON, toJSON, withObject, withText)
import qualified Data.List as List

data Color = Red | Black
  deriving (Eq, Ord, Generic)

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

data TreeMap a b = Node !a !b Color !(TreeMap a b) !(TreeMap a b) | Nil
  deriving (Eq, Ord, Generic)

type Tree a = TreeMap a ()

showTreeMap :: (Show a, Show b) => Int -> TreeMap a b -> String
showTreeMap depth = \ case
  Nil -> spaces <> "nil" <> "\n"
  Node x y c l r -> spaces <> "node " <> show x <> " " <> show y <> " " <> show c <> "\n"
    <> showTreeMap (depth + 1) l
    <> showTreeMap (depth + 1) r
  where
    spaces = mconcat $ replicate depth "  "

showTree :: (Show a) => Int -> Tree a -> String
showTree depth = \ case
  Nil -> spaces <> "nil" <> "\n"
  Node x _ c l r -> spaces <> "node " <> show x <> " " <> show c <> "\n"
    <> showTree (depth + 1) l
    <> showTree (depth + 1) r
  where
    spaces = mconcat $ replicate depth "  "

instance {-# OVERLAPPABLE #-} (Show a, Show b) => Show (TreeMap a b) where
  show = showTreeMap 0

instance {-# OVERLAPPING #-} (Show a) => Show (Tree a) where
  show = showTree 0

instance (ToJSON a, ToJSON b) => ToJSON (TreeMap a b) where
  toJSON = \ case
    Nil -> Null
    Node x y c tl tr -> object
      [ "key" .= x
      , "value" .= y
      , "color" .= c
      , "left" .= tl
      , "right" .= tr
      ]

instance (FromJSON a, FromJSON b) => FromJSON (TreeMap a b) where
  parseJSON = \ case
    Null -> pure Nil
    other -> flip (withObject "TreeMap") other $ \ obj -> Node
      <$> obj .: "key"
      <*> obj .: "value"
      <*> obj .: "color"
      <*> obj .: "left"
      <*> obj .: "right"

empty :: TreeMap a b
empty = Nil

singletonMap :: a -> b -> TreeMap a b
singletonMap x y = Node x y Black Nil Nil

singletonSet :: a -> Tree a
singletonSet x = singletonMap x ()

mapFromList :: Ord a => [(a, b)] -> TreeMap a b
mapFromList = foldr (uncurry insertMap) empty

mapFromListWith :: Ord a => (b -> b -> b) -> [(a, b)] -> TreeMap a b
mapFromListWith f = foldr (uncurry (insertMapWith f)) empty

setFromList :: Ord a => [a] -> Tree a
setFromList = mapFromList . List.map (, ())

mapToList :: TreeMap a b -> [(a, b)]
mapToList = \ case
  Nil -> []
  Node x y _ tl tr -> mapToList tl <> [(x, y)] <> mapToList tr

setToList :: Tree a -> [a]
setToList = List.map fst . mapToList

map :: ((a, b) -> (c, d)) -> TreeMap a b -> TreeMap c d
map f = \ case
  Nil -> Nil
  Node x y c tl tr -> let (z, w) = f (x, y) in Node z w c (map f tl) (map f tr)

mapValues :: (b -> c) -> TreeMap a b -> TreeMap a c
mapValues f = map $ \ (x, y) -> (x, f y)

mapKeys :: (a -> c) -> TreeMap a b -> TreeMap c b
mapKeys f = map $ \ (x, y) -> (f x, y)

fold :: (b -> c -> c) -> c -> TreeMap a b -> c
fold f z = \ case
  Nil -> z
  Node _ y _ tl tr -> fold f (f y (fold f z tr)) tl

foldKeys :: (a -> c -> c) -> c -> TreeMap a b -> c
foldKeys f z = \ case
  Nil -> z
  Node x _ _ tl tr -> foldKeys f (f x (foldKeys f z tr)) tl

balance :: (a, b) -> Color -> TreeMap a b -> TreeMap a b -> TreeMap a b
balance (z1, z2) Black (Node y1 y2 Red (Node x1 x2 Red a b) c) d = Node y1 y2 Red (Node x1 x2 Black a b) (Node z1 z2 Black c d)
balance (z1, z2) Black (Node x1 x2 Red a (Node y1 y2 Red b c)) d = Node y1 y2 Red (Node x1 x2 Black a b) (Node z1 z2 Black c d)
balance (x1, x2) Black a (Node z1 z2 Red (Node y1 y2 Red b c) d) = Node y1 y2 Red (Node x1 x2 Black a b) (Node z1 z2 Black c d)
balance (x1, x2) Black a (Node y1 y2 Red b (Node z1 z2 Red c d)) = Node y1 y2 Red (Node x1 x2 Black a b) (Node z1 z2 Black c d)
balance (x1, x2) c a b = Node x1 x2 c a b

blackRoot :: TreeMap a b -> TreeMap a b
blackRoot = \ case
  Nil -> Nil
  Node x y _ tl tr -> Node x y Black tl tr

insertMap :: Ord a => a -> b -> TreeMap a b -> TreeMap a b
insertMap x y = blackRoot . unsafeInsert
  where
    unsafeInsert = \ case
      Nil -> Node x y Red Nil Nil
      Node z w c tl tr -> case compare x z of
        EQ -> Node z w c tl tr
        LT -> balance (z, w) c (unsafeInsert tl) tr
        GT -> balance (z, w) c tl (unsafeInsert tr)

insertMapWith :: (Ord a) => (b -> b -> b) -> a -> b -> TreeMap a b -> TreeMap a b
insertMapWith f x y = blackRoot . unsafeInsert
  where
    unsafeInsert = \ case
      Nil -> Node x y Red Nil Nil
      Node z w c tl tr -> case compare x z of
        EQ -> Node z (f w y) c tl tr
        LT -> balance (z, w) c (unsafeInsert tl) tr
        GT -> balance (z, w) c tl (unsafeInsert tr)

insertSet :: Ord a => a -> Tree a -> Tree a
insertSet x = insertMap x ()

lookup :: Ord a => a -> TreeMap a b -> Maybe b
lookup x = \ case
  Nil -> Nothing
  Node y z _ tl tr -> case compare x y of
    EQ -> Just z
    LT -> lookup x tl
    GT -> lookup x tr

member :: Ord a => a -> Tree a -> Bool
member x = isJust . lookup x
