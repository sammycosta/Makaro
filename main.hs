module Main (main) where

import Data.List
import Matrix
import ReadPuzzle
import CertainSolutions
import Validations
import Solve
import SolveRegion

main :: IO()
main = do
    contents <- readFile "puzzle_14.txt"
    let list = lines contents
    let size = read (head list) :: Int
    -- create matrices of certainties and regions to map the puzzle
    let regionsMatrix = (createMatrix (take size (deleteFirst list)))
    let certaintyMatrix = (createIntMatrix (drop size (deleteFirst list)))
    let regions = (findRegions regionsMatrix certaintyMatrix (Matrix([[]])) (0, 0))

    let (newMat, newRegions) = certainties (certaintyMatrix, regions)
    let (succeeded, mat) = solve newMat regionsMatrix newRegions

    print succeeded
    printMatrix mat 0