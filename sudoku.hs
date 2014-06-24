module Sudoku where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

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

-- values = dict((s, digits) for s in squares)
-- gridParse :: [Char] -> Grid
--gridParse grid = 
--    let values = Map.fromList [(s, digits) | s <- squares]
        
eliminate :: Grid -> Square -> Char -> Maybe Grid
eliminate values s d = 
    if (d `elem` (lookup' s values)) == False then Just values
    else 
    let newV = Map.update (\vals -> Just $ delete d vals) s values

    -- (1) If a square s is reduced to one value d2, then eliminate d2 from the peers.
        propagate_peers vs 
            | length (vs Map.! s) == 0    = Nothing
            | length (vs Map.! s) == 1    =
                let d2 = (vs Map.! s) !! 0
                --  not all(eliminate(values, s2, d2) for s2 in peers[s]):
                    --inner_prop_peers = if (all (==True) (map (/=Nothing) [eliminate vs s2 d2 | s2 <- Set.toList $ peers Map.! s])) == False then Nothing
                    --else Just vs
                    inner_prop_peers
                        | (all (==True) (map (/=Nothing) [eliminate vs s2 d2 | s2 <- Set.toList $ peers Map.! s])) == False = Nothing
                in inner_prop_peers

    -- (2) If a unit u is reduced to only one place for a value d, then put it there.
        propagate_units vs = 
        -- [s for s in u if d in values[s]]
            let u = concat ([x | x <- units Map.! s])
                dplaces = [y | y <- u, d `elem` (concat $ Map.fold (:) [] vs)]
                inner_prop_units 
                    | length (dplaces) == 0 = Nothing
                    | length (dplaces) == 1 = assign vs (dplaces !! 0) d
                    
                    -- | otherwise             = Just vs
            in inner_prop_units

    in do
        vs' <- propagate_peers newV
        result <- propagate_units vs'
        return result

assign :: Grid -> Square -> Digit -> Maybe Grid
assign values s d = 
    let other_values = Map.update (\vals -> Just $ delete d vals) s values
    in if (all (==True) (map (/=Nothing) [eliminate values s d2 | d2 <- (concat $ Map.fold (:) [] other_values)])) == False then Nothing
        else Just values
