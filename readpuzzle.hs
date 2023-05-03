module ReadPuzzle (getListFromStr, stringToInt, createMatrix, createIntMatrix, 
                    isLetter, findRegions) where

import Matrix
import PositionUtils


-- Retorna uma lista de palavras a partir de uma String
getListFromStr :: String -> [String]
getListFromStr line = words line

-- Converte String para Int
stringToInt :: String -> Int
stringToInt s = read s


-- Cria uma matriz tipo Matrix de Strings a partir de uma lista de strings
createMatrix :: [String] -> GenMatrix String
createMatrix list = Matrix (createDoubleList list)
    where
        -- cria uma lista de listas a partir de uma lista de strings
        createDoubleList :: [String] -> [[String]]
        createDoubleList [] = []
        createDoubleList (a:tail) = (getListFromStr a):(createDoubleList tail)


-- Cria uma matriz tipo Matrix de Ints a partir de uma lista de strings
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

-- Retorna True se for uma das letras utilizadas no mapeamento do jogo
isLetter :: String -> Bool
isLetter l = (l =="R" || l == "L" || l == "D" || l == "U" || l == "X")

-- Retorna o número da região (recebida como String)
regionNumber :: String -> Int
regionNumber c = ((stringToInt c)-1)

-- Cria uma nova região caso ela ainda não exista, senão retorna a matriz de regiões inalterada
newRegion :: String -> GenMatrix Position -> GenMatrix Position
newRegion c regions | ((stringToInt c) > (getRowsNumber regions)) = (addElement regions ((getRowsNumber regions), 0) (0, 0))
                    | otherwise = regions

-- Aumenta o tamanho da região (o primeiro elemento da primeira tupla armazenada na linha correspondente à região)
increaseSize :: String -> GenMatrix Position -> GenMatrix Position
increaseSize c regions = 
    (changeElement regions ((regionNumber c), 0) (increaseFirst (getElement regions ((regionNumber c), 0))))

-- Aumenta o a quantidade de elementos não preenchidos na região
-- (o segundo elemento da primeira tupla armazenada na linha correspondente à região)
increaseUnfilled :: String -> GenMatrix Int -> GenMatrix Position -> Position -> GenMatrix Position
increaseUnfilled c mat_cert regions pos
    | ((getElement mat_cert pos) == 0) = (changeElement regions ((regionNumber c), 0) (increaseSecond (getElement regions ((regionNumber c), 0))))
    | otherwise = regions

-- Adiciona uma posição a uma região e retorna a matrix de regiões
addToRegion :: String -> GenMatrix Position -> Position -> GenMatrix Position
addToRegion c regions pos =
    (addElement regions ((regionNumber c), (getColumnsNumber regions (regionNumber c))) pos)

-- Recebe um caractere (da matriz de regiões) e chama as funções acima para adicionar a posição 
-- à região respectiva na matriz de regiões
treatCharacter :: String -> GenMatrix Int ->  GenMatrix Position -> Position ->  GenMatrix Position
treatCharacter c mat_cert regions (row, col) = 
    if not (isLetter c) then
        (addToRegion c (increaseUnfilled c mat_cert (increaseSize c (newRegion c regions)) (row, col)) (row, col))
    else
        regions

-- Percorre a matriz de regiões e passa os caracteres para a função acima
findRegions :: GenMatrix String -> GenMatrix Int ->  GenMatrix Position -> Position ->  GenMatrix Position
findRegions mat_reg mat_cert regions (row, col) = 
    if (col+1) == (getRowsNumber mat_reg) && (row+1) == (getRowsNumber mat_reg) then
        (treatCharacter (getElement mat_reg (row, col)) mat_cert regions (row, col))
    else if (col+1) == (getRowsNumber mat_reg) && (row+1) /= (getRowsNumber mat_reg) then
        findRegions mat_reg mat_cert (treatCharacter (getElement mat_reg (row, col)) mat_cert regions (row, col)) (row+1, 0)
    else
        findRegions mat_reg mat_cert (treatCharacter (getElement mat_reg (row, col)) mat_cert regions (row, col)) (row, col+1)