module Sudoku where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

-- set types
type Digit = Char
type Square = (Char, Char)
type Unit = [Square]

digits   = "123456789"
rows     = "ABCDEFGHI"
cols     = digits

cross :: String -> String -> [Square]
cross xs ys = [ (x, y) | x <- xs, y <- ys]

squares :: [Square]
squares = cross rows cols

unitlist :: [Unit]
unitlist = 
        [cross rows [c] | c <- cols] ++
        [cross [r] cols | r <- rows] ++
        [cross rs cs | rs <- ["ABC", "DEF", "GHI"], cs <- ["123", "456", "789"]]

units :: Map.Map Square [Unit]
units = Map.fromList [(s, [u | u <- unitlist, s `elem` u]) | s <- squares]

lookup' :: Ord k => k -> Map.Map k c -> c
lookup' key _map = fromJust . Map.lookup key $ _map

peers :: Map.Map Square (Set.Set Square)
peers = Map.fromList [(s, Set.difference (Set.fromList . concat . lookup' s $ units) (Set.fromList [s])) | s <- squares]


