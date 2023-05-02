module Solve() where

import Matrix
import PositionUtils
import Validations
import Data.List

type Possibility = [Int, Position]
type RegionError = ([Int], [Position]) -- Caminho, posições falhas
-- AUXILIARES

-- NAO TESTADA Junta um número com várias posições, criando uma lista de possibilidades.
positionNumber:: Int -> [Position] -> [Possibility] -> [Possibility]
positionNumber _ [] possibilities = possibilities
positionNumber number (head:tail) possibilities =
    positionNumber number tail newPossibilities
    where
        newPossibilities = possibilities ++ [(number, head)]

-- NAO TESTADA Chamada positionNumber para todos os números na lista
numberPosition:: [Int] -> [Position] -> [Possibility] -> [Possibility]
numberPosition [] _ possibilities = possibilities
numberPosition (head:tail) possiblePositions possibilities =
        numberPosition tail possiblePositions newPossibilities 
    where
        newPossibilities = positionNumber head possiblePositions possibilities

-- NAO TESTADA Faz a lista de possibilidades a partir de uma lista de números e uma de posições
getPossibilitiesList:: [Int] -> [Position] -> [Possibility]
getPossibilitiesList possibleNumbers possiblePositions = 
    numberPosition possibleNumbers possiblePositions []

-- NAO TESTADA Tenta preencher um número em todas as posições até conseguir
-- Retorna: booleano, posição que deve ser colocada caso True
tryFillNumber :: GenMatrix Int -> GenMatrix String -> Int -> [Position] -> [Position] -> (Bool, Position)
tryFillNumber _ _ [] _ = (False, (-1, -1))
tryFillNumber mat matRegions number (head:tail) region
    | isValid mat matRegions number head = (True, head)
    | otherwise = fillNumber mat matRegions number tail region

-- NAO TESTADA Dada uma posição e número, retorna a matriz e a região com o número colocado na posição;
-- PS: DEVOLVENDO LISTA DE UMA REGIÃO SÓ, TERIA QUE ALTERAR NA MATRIZONA DE REGIÕES EM ALGUM LUGAR
fillNumber :: GenMatrix Int -> Int -> Position -> [Position] -> (GenMatrix Int, [Position])
fillNumber mat number position region = (newMatrix, newRegion)
    where
        newMatrix = changeElement mat position number
        regionInfo = head region -- Primeira tupla da região
        newRegion = changeElementList region 0 (decreaseSecond regionInfo)

-- NAO TESTADA Remove itens da lista principal se existirem na auxiliar. Retorna a lista mudada
removeItemsFromList :: [t] -> [t] -> [t]
removeItemsFromList major [] = major
removeItemsFromList major (head:tail)
    | elem head major = removeItemsFromList newMajorList tail 
    | otherwise = removeItemsFromList major tail
    where
        newMajorList = filter (\x -> x /= head) major


-- [1, 5, 8]
-- [2, 8, 9]
 -- [5, 8, 9]

-- NAO TESTADA Retorna a lista de posições possíveis alterada e se alterou ela, caso:
-- O caminho que estou agora está seguindo um caminho que já deu errado antes.
forAllWrongPaths :: [Int] -> [[Int]] -> [Position] -> [Possibility] -> Bool -> ([Position], Bool)
forAllWrongPaths  _ [[]] possiblePositions _ hasAltered = (possiblePositions, hasAltered)
forAllWrongPaths currentPath (head:tail) possiblePositions possibilities hasAltered
    | currentPath isPrefixOf head = 
        forAllWrongPaths currentPath tail newPossiblePositions possibilities True
    | otherwise = forAllWrongPaths currentPath tail possiblePositions possibilities hasAltered
    where
        positionToRemove = getSecond (possibilities !! (length caminho - 1))
        newPossiblePositions = removeItemsFromList possiblePositions [positionToRemove]


-- próximo: backtracking_preenche_numero (interno à região)

-- Dado um caminho, verificar se não há possições falhas a serem removidas nesse ponto da árvore.
-- Retornar a lista de posições alterada (ou não)
removeErrorPositions :: [Int] -> [RegionError] -> [Position] -> [Position]
removeErrorPositions path errorList possiblePositions
    | isCurrentError || isFirstNumberError = removeItemsFromList possiblePositions (getSecond currentError)
    | otherwise = possiblePositions
    where
        order = length path
        currentError = errorList !! order -- É uma tupla, tipo RegionError
        lenErrorList = length (getSecond currentError) -- Isso vai funcionar? Preciso verificar se há valores errados
        isCurrentError = order > 0 && lenErrorList > 0 && (getFirst currentError) == path 
        isFirstNumberError = order == 0 && lenErrorList > 0

-- Retorna a matriz de regiões e a região sem o preenchimento 
cleanFillNumber :: GenMatrix Int -> [Int] -> Position -> (GenMatrix Int, [Int])
cleanFillNumber mat region position = (newMatrix, newRegion)
    where
        newMatrix = changeElement mat position 0
        regionInfo = head region -- Primeira tupla da região
        newRegion = changeElementList region 0 (increaseSecond regionInfo)

-- tenta um novo preenchimento após um anterior pois está seguindo um caminho errado
tryAgainFillNumber :: GenMatrix Int -> GenMatrix String -> Int -> [Position] -> [Position] 
                    -> Position -> [Int] -> ()
tryAgainFillNumber mat matRegions number possiblePositions region lastPosition currentPath
    | succeeded = 
    | otherwise = (mat, region, lastPosition, currentPath)
    where
        (newMatrix, newRegion) = cleanFillNumber mat region lastPosition 
        (succeeded, pos) = tryFillNumber mat matRegions number possiblePositions region
        newPath = path -- remover ultimo da lista e adicionar um novo


-- Conseguiu preencher. Agora, chamar a forAllWrongPaths e retornar matriz, região, caminho e posição?
checkWrongPaths :: GenMatrix Int -> GenMatrix String -> [Int] -> GenMatrix Int 
                -> [Position] -> [Possibility] -> Position -> (GenMatrix Int, [Int], [Int], Position)
checkWrongPaths mat matRegions path wrongPaths lastPosition possiblePositions
    | hasAltered = 
    | otherwise = -- retornar tudo com o preenchimento normal
   where
    (newPossiblePositions, hasAltered) =  forAllWrongPaths path (getListFromMatrix wrongPaths) possiblePositions possibilities False

backtrackingTryFillNumber :: GenMatrix Int -> GenMatrix String -> [Int] -> [Position] 
                        -> [Position] -> [Possibility] -> [Int] -> [RegionError] 
                        -> GenMatrix Int -> (Bool)
backtrackingTryFillNumber mat matRegions head:tail possiblePositions region possibilities currentPath errorList wrongPaths 
    | succeeded = checkWrongPaths 
    | otherwise = False
    where
        newPossiblePositions = removeErrorPositions currentPath errorList possiblePositions
        (succeeded, pos) = tryFillNumber mat matRegions head newPossiblePositions region
        currentPath = path ++ [elemIndex (head, pos) possibilities] -- elemIndex é dó data.list


-- Itera pela lista de números pra colocar eles em cada posição
fillWholeRegion :: GenMatrix Int -> GenMatrix String -> [Int] -> [Position] 
                        -> [Position] -> [Possibility] -> [Int] -> [RegionError] 
                        -> GenMatrix Int -> Bool
fillWholeRegion mat matRegions head:tail possiblePositions region possibilities currentPath errorList wrongPaths
    | (((length (head:tail)) == 0) && ((region!!1) /= 0)) = 
        False
    | (((length (head:tail)) == 0) && ((region!!1) == 0)) =
        (not (elem currentPath (getListFromMatrix wrongPaths)))
    | otherwise = 
        backtrackingTryFillNumber mat matRegions head:tail possiblePositions region possibilities currentPath errorList wrongPaths


solveByRegion :: GenMatrix Int -> GenMatrix String -> GenMatrix Int -> GenMatrix Int 
                        -> [Int] -> [Position] -> Bool
solveByRegion mat matRegions regionsPaths wrongPaths possibleNumbers possiblePositions
    | (fillWholeRegion mat matRegions possibleNumbers newPossiblePos [] errorList? wrongPaths) =
        -- print caminho >> printMatrix >> True
    | otherwise = False
    where
        newPossiblePos = (getPossibilitiesList possibleNumbers possiblePositions)
