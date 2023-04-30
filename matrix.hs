module Matrix (GenMatrix (Matrix), Position,
               getElement, changeElement, getColumnsNumber, getRowsNumber,
               addElement) where

import Data.List

data GenMatrix t = Matrix [[t]]
            deriving (Eq, Show)

type Position = (Int, Int)

-- getSize :: GenMatrix t -> (Int, Int)
-- getSize (Matrix []) = (0, 0)
-- getSize (Matrix mat) = (length mat, length (mat!!0))

getRowsNumber:: GenMatrix t -> Int
getRowsNumber (Matrix [[]]) = 0
getRowsNumber (Matrix mat) = length mat

getColumnsNumber::GenMatrix t -> Int -> Int
getColumnsNumber (Matrix []) _ = 0
getColumnsNumber (Matrix mat) row | row < 0 || row >= length (mat) = error "Index out of bounds"
                                  | otherwise = length (mat!!row)

getElement :: GenMatrix t -> Position -> t
getElement  (Matrix mat) (row, col) = (mat !! row !! col)

changeElementList :: [t] -> Int -> t -> [t]
changeElementList [] _ _ = error "Empty"
changeElementList (a:tail) i value | (i == 0) = (value:tail)
                                   | otherwise = a : (changeElementList tail (i-1) value)

changeElement :: GenMatrix t -> Position -> t -> GenMatrix t
changeElement (Matrix []) (_, _) _ = error "Empty"
changeElement (Matrix (a:tail)) (row, col) value | row < 0 || col < 0 || row >= length (a:tail) || col >= length a = error "Index out of bounds"
                                                 | (row == 0) = Matrix (new_list:tail)
                                                 | otherwise = Matrix (a : rows)
    where
        new_list = (changeElementList ((a:tail)!!row) col value)
        rows = getRows (changeElement (Matrix tail) (row - 1, col) value)
        getRows (Matrix m) = m


addElementList :: [t] -> Int -> t -> [t]
addElementList [] i value = [value]
addElementList (a:tail) i value | (i == 0) = (value:a:tail)
                                | otherwise = a : (addElementList tail (i-1) value)

addElement :: GenMatrix t -> Position -> t -> GenMatrix t
addElement (Matrix []) (row, col) value = Matrix ([[value]])
addElement (Matrix (a:tail)) (row, col) value | row < 0 || col < 0 || row > length (a:tail) || (row < length (a:tail) && col > length ((a:tail)!!row)) = error "Index out of bounds"
                                         | (row == length(a:tail)) = Matrix (a:tail ++ [new_row])
                                         | (row == 0) = Matrix (new_list:tail)
                                         | otherwise = Matrix (a : rows)
    where
        new_list = (addElementList ((a:tail)!!row) col value)
        rows = getRows (addElement (Matrix tail) (row - 1, col) value)
        getRows (Matrix m) = m
        new_row = (addElementList [] col value)
        