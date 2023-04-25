module Matrix (GenMatrix (Matrix),
               getSize, getElement, changeElement) where

import Data.List

data GenMatrix t = Matrix [[t]]
            deriving (Eq, Show)

getSize :: GenMatrix t -> Int
getSize (Matrix mat) = length mat

getElement :: GenMatrix t -> (Int, Int) -> t
getElement  (Matrix mat) (row, col) = (mat !! row !! col)

-- addElement :: GenMatrix t -> (Int, Int) -> t
-- addElement (Matrix mat) (row, col) = 

changeElementList :: [t] -> Int -> t -> [t]
changeElementList [] _ _ = error "Empty"
changeElementList (a:tail) i value | (i == 0) = (value:tail)
                                   | otherwise = a : (changeElementList tail (i-1) value)

changeElement :: GenMatrix t -> (Int, Int) -> t -> GenMatrix t
changeElement (Matrix []) (_, _) _ = error "Empty"
changeElement (Matrix (a:tail)) (row, col) value | row < 0 || col < 0 || row >= length (a:tail) || col >= length a = error "Index out of bounds"
                                                 | (row == 0) = Matrix (new_list:tail)
                                                 | otherwise = Matrix (a : rows)
    where
        new_list = (changeElementList ((a:tail)!!row) col value)
        rows = getRows (changeElement (Matrix tail) (row - 1, col) value)
        getRows (Matrix m) = m

