module ReadPuzzle (deleteFirst, getListFromStr, stringToInt, createMatrix, createIntMatrix, 
                    isLetter, findRegions) where

import Matrix

-- functions to create matrices from text

deleteFirst :: [String] -> [String]
deleteFirst (a:b) = b

getListFromStr :: String -> [String]
getListFromStr line = words line

stringToInt :: String -> Int
stringToInt s = read s


-- cria uma matriz tipo Matrix a partir de uma lista de strings
createMatrix :: [String] -> GenMatrix String
createMatrix list = Matrix (createDoubleList list)
    where
        -- cria uma lista de listas a partir de uma lista de strings
        createDoubleList :: [String] -> [[String]]
        createDoubleList [] = []
        createDoubleList (a:tail) = (getListFromStr a):(createDoubleList tail)


createIntMatrix :: [String] -> GenMatrix Int
createIntMatrix list = Matrix (createIntDoubleList list)
    where
        createIntDoubleList :: [String] -> [[Int]]
        createIntDoubleList [] = []
        createIntDoubleList (a:tail) = (listToInt (getListFromStr a)):(createIntDoubleList tail)

        listToInt :: [String] -> [Int]
        listToInt [] = []
        listToInt (a:tail) = (stringToInt a):(listToInt tail)


---------------------

isLetter :: String -> Bool
isLetter l = (l =="R" || l == "L" || l == "D" || l == "U" || l == "X")

increaseFirst :: Position -> Position
increaseFirst (a, b) = (a+1, b)

increaseSecond :: Position -> Position
increaseSecond (a, b) = (a, b+1)

regionNumber :: String -> Int
regionNumber c = ((stringToInt c)-1)


newRegion :: String -> GenMatrix Position -> GenMatrix Position
newRegion c regions | ((stringToInt c) > (getRowsNumber regions)) = (addElement regions ((getRowsNumber regions), 0) (0, 0))
                    | otherwise = regions

increaseSize :: String -> GenMatrix Position -> GenMatrix Position
increaseSize c regions = 
    (changeElement regions ((regionNumber c), 0) (increaseFirst (getElement regions ((regionNumber c), 0))))

increaseUnfilled :: String -> GenMatrix Int -> GenMatrix Position -> Position -> GenMatrix Position
increaseUnfilled c mat_cert regions pos
    | ((getElement mat_cert pos) == 0) = (changeElement regions ((regionNumber c), 0) (increaseSecond (getElement regions ((regionNumber c), 0))))
    | otherwise = regions

addToRegion :: String -> GenMatrix Position -> Position -> GenMatrix Position
addToRegion c regions pos =
    (addElement regions ((regionNumber c), (getColumnsNumber regions (regionNumber c))) pos)

treatCharacter :: String -> GenMatrix Int ->  GenMatrix Position -> Position ->  GenMatrix Position
treatCharacter c mat_cert regions (row, col) = 
    if not (isLetter c) then
        (addToRegion c (increaseUnfilled c mat_cert (increaseSize c (newRegion c regions)) (row, col)) (row, col))
    else
        regions

findRegions :: GenMatrix String -> GenMatrix Int ->  GenMatrix Position -> Position ->  GenMatrix Position
findRegions mat_reg mat_cert regions (row, col) = 
    if (col+1) == (getRowsNumber mat_reg) && (row+1) == (getRowsNumber mat_reg) then
        regions
    else if (col+1) == (getRowsNumber mat_reg) && (row+1) /= (getRowsNumber mat_reg) then
        findRegions mat_reg mat_cert (treatCharacter (getElement mat_reg (row, col)) mat_cert regions (row, col)) (row+1, 0)
    else
        findRegions mat_reg mat_cert (treatCharacter (getElement mat_reg (row, col)) mat_cert regions (row, col)) (row, col+1)