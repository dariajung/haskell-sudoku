module Sudoku where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad

-- set types
type Digit = Char
type Square = (Char, Char)
type Unit = [Square]
type Grid = Map.Map Square [Char]

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
    in Map.fromList (zip squares chars)

parseGrid :: [Char] -> Maybe Grid
parseGrid str = 
    let values = Map.fromList [(s, digits) | s <- squares]
        reduce vs (s, d) = if d `elem` digits then assign vs s d else Just vs
        gridVals = Map.toList (gridValues str)
    in foldM reduce values gridVals

eliminate :: Grid -> Square -> Char -> Maybe Grid
eliminate values s d = 
    if (d `elem` (values Map.! s)) == False then Just values -- Already eliminated 
    else 
    let newV = Map.update (\vals -> Just $ delete d vals) s values

    in do
        vs' <- propagatePeers newV s d
        result <- propagateUnits vs' s d
        return result

propagatePeers :: Grid -> Square -> Digit -> Maybe Grid
propagatePeers vs s d
    -- (1) If a square s is reduced to one value d2, then eliminate d2 from the peers.
    | length (vs Map.! s) == 0    = Nothing -- contradiction: removed last value
    | length (vs Map.! s) == 1    =
        let d2 = (vs Map.! s) !! 0 -- vs Map.! s should be length 1
            inner_prop_peers values s2 = eliminate values s2 d2 
        in foldM inner_prop_peers vs (Set.toList $ peers Map.! s)
    | otherwise                    = Just vs

propagateUnits :: Grid -> Square -> Digit -> Maybe Grid
propagateUnits vs s d = 
    -- (2) If a unit u is reduced to only one place for a value d, then put it there.
    let reduceProp vs' u = inner_prop_units vs' u              
        inner_prop_units vals unit
            | length (dplaces) == 0 = Nothing -- contradiction
            | length (dplaces) == 1 = assign vals (dplaces !! 0) d
            | otherwise             = Just vals
            where dplaces = [sq | sq <- unit, d `elem` (vals Map.! sq)]

    in foldM reduceProp vs (units Map.! s)

assign :: Grid -> Square -> Digit -> Maybe Grid
assign values s d = 
    let otherValues = (delete d $ values Map.! s)
        reduce vals dig = eliminate vals s dig
    in foldM reduce values otherValues
