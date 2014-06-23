import Data.List

-- set types
type Digit = Char
type Square = (Char, Char)
type Unit = [Square]


cross :: String -> String -> [Square]
cross xs ys = [ (x, y) | x <- xs, y <- ys]

squares :: [Square]
squares = cross rows cols

digits   = "123456789"
rows     = "ABCDEFGHI"
cols     = digits


--unitlist = ([cross(rows, c) for c in cols] +
--            [cross(r, cols) for r in rows] +
--            [cross(rs, cs) for rs in ('ABC','DEF','GHI') for cs in ('123','456','789')])

