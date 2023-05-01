module Main (main) where

import Data.List
import Matrix
import ReadPuzzle
import CertainSolutions


main :: IO()
main = do
    contents <- readFile "puzzle_01.txt"
    print contents
    let list = lines contents
    let size = read (head list) :: Int
    print size

    -- create matrices of certainties and regions to map the puzzle
    -- for now, both have only Strings (convert to Int can be necessary)
    -- let regionsMatrix = (Matrix (createMatrix (take size (deleteFirst list))))
    -- let certaintyMatrix = (Matrix (createMatrix (drop size (deleteFirst list))))
    let regionsMatrix = (createMatrix (take size (deleteFirst list)))
    let certaintyMatrix = (createIntMatrix (drop size (deleteFirst list)))
    print certaintyMatrix
    print regionsMatrix
    print (getElement regionsMatrix (0,1))
    print (isLetter (getElement regionsMatrix (0,1)))
    print (getColumnsNumber regionsMatrix 2)
    print (getRowsNumber certaintyMatrix)

    let test = Matrix([[]])
    print (getRowsNumber test)
    print (getColumnsNumber test 0)
    print (addElement (Matrix([[(0, 1)]])) (0, 0) (1, 4))
    print (addElement (Matrix([[(0, 1)], [(2, 2)]])) (1, 0) (1, 4))
    print (addElement (addElement (addElement (Matrix([])) (0, 0) 'r') (1, 0) 'f') (0, 1) 'a')
    print (getRowsNumber (Matrix([[]])))
    print (addElement (Matrix([[]])) (0, 0) 1)
    print (addElement (Matrix([]++[[(3,1)]])) (1, 0) (1, 4))

    let regions = (findRegions regionsMatrix certaintyMatrix (Matrix([[]])) (0, 0))
    print regions
    print (getRow regions 2)

    -- print (missingNumbers (getRow regions 4) certaintyMatrix)
    let res = (fillMissingOne 0 (certaintyMatrix, regions))
    print (res)
    print ("\n")
    print (fillMissingTwo 0 res)