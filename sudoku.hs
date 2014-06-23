module Sudoku where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

-- set types
type Digit = Char
type Square = (Char, Char)
type Unit = [Square]
type Grid = Map.Map Square Digit

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

gridValues :: [Char] -> Map.Map Square Char
gridValues grid = 
    let chars = [c | c <- grid, c `elem` digits || c `elem` "0."]
    in Map.fromList $ zip squares chars

-- values = dict((s, digits) for s in squares)
--gridParse grid = 
--    let values = Map.fromList [(s, digits) | s <- squares]
        

eliminate values s d = 
    if d `elem` (lookup' values s) == False then values
    else 
    let newV = Map.update (\vals -> Just $ del d vals) s values

    -- (1) If a square s is reduced to one value d2, then eliminate d2 from the peers.
    propagate_peers vs 
        | length (lookup' s vs) == 0    = Nothing
        | length (lookup' s vs) == 1    =
            let d2 = (lookup' s vs)




