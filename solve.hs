module Solve() where

import Matrix
import PositionUtils
import Validations

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

-- NAO TESTADA Retorna a lista de posições possíveis alterada e se alterou ela, caso:
-- O caminho que estou agora está seguindo um caminho que já deu errado antes.
forAllWrongPaths :: [Int] -> GenMatrix Int -> [Position] -> [Possibility] -> Bool -> ([Position], Bool)
forAllWrongPaths  _ [[]] possiblePositions _ hasAltered = (possiblePositions, hasAltered)
-- MATRIZ DE CAMINHOS ERRADOS VAZIA, TO CHECANDO CERTO ACIMA?
-- PRECISO PEGAR HEAD:TAIL DA MATRIZ, OU SEJA, ROW POR ROW, COMO?
forAllWrongPaths :: currentPath (head:tail) possiblePositions possibilities hasAltered
    | currentPath isPrefixOf head = 
        forAllWrongPaths currentPath tail newPossiblePositions possibilities True
    | otherwise = forAllWrongPaths currentPath tail possiblePositions possibilities hasAltered
    where
        positionToRemove = getSecond (possibilities !! (length caminho - 1))
        newPossiblePositions = removeItemsFromList possiblePositions [positionToRemove]


-- próximo: backtracking_preenche_numero (interno à região)

removeErrorPositions :: [Int]

backtrackingTryFillNumber :: GenMatrix Int -> GenMatrix String -> [Int] -> [Position] 
                        -> [Position] -> [Possibility] -> [Int] -> [RegionError] 
                        -> GenMatrix Int -> (Bool)
backtrackingTryFillNumber mat matRegions possibleNumbers possiblePositions region possibilities currentPath errorList wrongPaths 
