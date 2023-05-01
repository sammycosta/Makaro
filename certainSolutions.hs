module CertainSolutions (missingNumbers, iterateList, 
                        fillMissingOne, fillMissingTwo) where


import Data.List
import Matrix
import PositionUtils
import Validations


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

fillMat1 :: Int -> ([Int], [Position]) -> GenMatrix Int -> GenMatrix Position -> (GenMatrix Int, GenMatrix Position)
fillMat1 i (missing, positions) mat regions = 
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
        missing_num = (fillMat1 i (missingNumbers (getRow regions i) mat) mat regions)

changeMatrices :: Int -> Int -> Int -> Int -> Int -> ([Int], [Position]) -> GenMatrix Int -> GenMatrix Position -> (GenMatrix Int, GenMatrix Position)
changeMatrices p1 m1 p2 m2 i (missing, positions) mat regions = 
    ((changeElement (changeElement mat (positions!!p1) (missing!!m1)) (positions!!p2) (missing!!m2)), 
        (changeElement regions  (i, 0) (decreaseSecond(decreaseSecond (getElement regions (i, 0))))))

verifications :: Int -> ([Int], [Position]) -> GenMatrix Int -> GenMatrix Position -> (GenMatrix Int, GenMatrix Position)
verifications i (missing, positions) mat regions
    | ((isAdjacent mat (missing!!0) (positions!!0)) || (isAdjacent mat (missing!!1) (positions!!1)))= 
        (changeMatrices 0 1 1 0 i (missing, positions) mat regions)
    | ((isAdjacent mat (missing!!1) (positions!!0)) || (isAdjacent mat (missing!!0) (positions!!1))) =
        (changeMatrices 1 1 0 0 i (missing, positions) mat regions)
    | otherwise = (mat, regions)

fillMat2 :: Int -> GenMatrix Int -> GenMatrix Position -> (GenMatrix Int, GenMatrix Position)
fillMat2 i mat regions = 
    if (getSecond (getElement regions (i, 0))) == 2 then 
       (verifications i (missingNumbers (getRow regions i) mat) mat regions)
    else
        (mat, regions)

fillMissingTwo :: Int -> (GenMatrix Int, GenMatrix Position) -> (GenMatrix Int, GenMatrix Position)
fillMissingTwo i (mat, regions) = 
    if i+1 < ((getRowsNumber regions)) then
        fillMissingTwo (i+1) (fillMat2 i mat regions)
    else
       (fillMat2 i mat regions)
    