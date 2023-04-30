module CertainSolutions (missingNumbers, iterateList) where

import Matrix
import Data.List


getFirst :: Position -> Int
getFirst (a, _) = a

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
    iterateList [1..(getFirst (region_list!!0))] [] (deleteFirst region_list) mat