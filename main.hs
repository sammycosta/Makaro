module Main (main) where

import Data.List
import Matrix

-- getWords :: FilePath -> IO [String]
-- getWords path = do contents <- readFile path
--                    return (lines contents)


deleteFirst :: [String] -> [String]
deleteFirst (a:b) = b

getListFromStr :: String -> [String]
getListFromStr line = words line

-- getIntListFromStr :: 

-- creates a list of lists from a list of strings
createMatrix :: [String] -> [[String]]
createMatrix [] = []
createMatrix (a:tail) = (getListFromStr a):(createMatrix tail)


main :: IO()
main = do
    contents <- readFile "puzzle_01_1.txt"
    print contents
    let list = lines contents
    let size = read (head list) :: Int
    print size

    -- create matrices of certainties and regions to map the puzzle
    -- for now, both have only Strings (convert to Int can be necessary)
    let certaintyMatrix = (Matrix (createMatrix (take size (deleteFirst list))))
    let regionsMatrix = (Matrix (createMatrix (drop size (deleteFirst list))))
    print certaintyMatrix
    print regionsMatrix

    let matrix = (Matrix [[0, 1], [2, 3]])
    print (getSize matrix)
    print (getElement matrix (0, 0))
