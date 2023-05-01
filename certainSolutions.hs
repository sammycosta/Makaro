module CertainSolutions (missingNumbers, iterateList, fillMissingOne) where

import Matrix
import Data.List

-- Primeiro elemento de uma posição
getFirst :: Position -> Int
getFirst (a, _) = a

-- Segundo elemento de uma posição
getSecond :: Position -> Int
getSecond (_, b) = b

decreaseFirst :: Position -> Position
decreaseFirst (a, b) = (a-1, b)

decreaseSecond :: Position -> Position
decreaseSecond (a, b) = (a, b-1)

iterateList :: [Int] -> [Position] -> [Position] -> GenMatrix Int -> ([Int], [Position])
iterateList possibilities empty_pos list mat =
    if (length list) > 0 then
        if (number) /= 0 then
            iterateList (possibilities \\[number]) empty_pos (deleteFirst list) mat
        else
            iterateList possibilities (empty_pos++[(list!!0)]) (deleteFirst list) mat
    else
        (possibilities, empty_pos)
    where
        number = (getElement mat (list!!0))


missingNumbers :: [Position] -> GenMatrix Int -> ([Int], [Position])
missingNumbers region_list mat =
    iterateList [1..((getFirst (region_list!!0)))] [] (deleteFirst region_list) mat

fillMat :: Int -> ([Int], [Position]) -> GenMatrix Int -> GenMatrix Position -> (GenMatrix Int, GenMatrix Position)
fillMat i (missing, positions) mat regions= 
    if (getSecond (getElement regions (i, 0))) == 1 then 
        ((changeElement mat (positions!!0) (missing!!0)), (changeElement regions  (i, 0) (decreaseSecond (getElement regions (i, 0)))))
    else
        (mat, regions)
        

fillMissingOne :: Int -> (GenMatrix Int, GenMatrix Position) -> (GenMatrix Int, GenMatrix Position)
fillMissingOne i (mat, regions) = 
    if i+1 < ((getRowsNumber regions)) then
        fillMissingOne (i+1) missing_num
    else
        missing_num
    where
        missing_num = (fillMat i (missingNumbers (getRow regions i) mat) mat regions)

-- fillMissingTwo :: Int -> (GenMatrix Int, GenMatrix Position) -> (GenMatrix Int, GenMatrix Position)
-- fillMissingTwo i (mat, regions) = 
--     if i+1 < ((getRowsNumber regions)) then
--         fillMissingOne (i+1) missing_num
--     else
--         missing_num
