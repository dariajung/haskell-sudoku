import Sudoku
import Test.QuickCheck


-- Unit Tests --

check_length_squares = length squares == 81 
check_length_unitlist = length unitlist == 27

check_length_units = [(length . lookup' s $ units) == 3 | s <- squares]

check_c2 = lookup' ('C', '2') units == 
    [[('A','2'),('B','2'),('C','2'),('D','2'),('E','2'),('F','2'),('G','2'),('H','2'),('I','2')],[('C','1'),('C','2'),('C','3'),('C','4'),('C','5'),('C','6'),('C','7'),('C','8'),('C','9')],[('A','1'),('A','2'),('A','3'),('B','1'),('B','2'),('B','3'),('C','1'),('C','2'),('C','3')]]