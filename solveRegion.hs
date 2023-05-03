module SolveRegion(solveByRegion,
fillWholeRegion, backtrackingTryFillNumber, continueBackTrackingTryFillNumber, tryAgainSameNumber, removeErrorPositions,
tryAgainFillNumber) where

import Matrix
import PositionUtils
import Validations
import Data.List
import Data.Maybe (fromMaybe)

type Possibility = (Int, Position) -- Número, posição
type RegionError = ([Int], [Position]) -- Caminho, posições falhas

-- Junta um número com várias posições, criando uma lista de possibilidades
positionNumber:: Int -> [Position] -> [Possibility] -> [Possibility]
positionNumber _ [] possibilities = possibilities
positionNumber number (head:tail) possibilities =
    positionNumber number tail newPossibilities
    where
        newPossibilities = possibilities ++ [(number, head)]

-- Chamada positionNumber para todos os números na lista
numberPosition:: [Int] -> [Position] -> [Possibility] -> [Possibility]
numberPosition [] _ possibilities = possibilities
numberPosition (head:tail) possiblePositions possibilities =
        numberPosition tail possiblePositions newPossibilities 
    where
        newPossibilities = positionNumber head possiblePositions possibilities

-- Faz a lista de possibilidades a partir de uma lista de números e uma de posições
getPossibilitiesList:: [Int] -> [Position] -> [Possibility]
getPossibilitiesList possibleNumbers possiblePositions = 
    numberPosition possibleNumbers possiblePositions []

-- Tenta preencher um número em todas as posições até conseguir
-- Retorna: booleano, posição que deve ser colocada caso True
tryFillNumber :: GenMatrix Int -> GenMatrix String -> Int -> [Position] -> [Position] -> (Bool, Position)
tryFillNumber _ _ _ [] _ = (False, (-1, -1))
tryFillNumber mat matRegions number (head:tail) region
    | isValid mat matRegions number head = (True, head)
    | otherwise = tryFillNumber mat matRegions number tail region

-- Dada uma posição e número, retorna a matriz e a região com o número colocado na posição;
fillNumber :: GenMatrix Int -> Int -> Position -> [Position] -> (GenMatrix Int, [Position])
fillNumber mat number position region = (newMatrix, newRegion)
    where
        newMatrix = changeElement mat position number
        regionInfo = head region -- Primeira tupla da região
        newRegion = changeElementList region 0 (decreaseSecond regionInfo)

-- Remove itens da lista principal se existirem na auxiliar. Retorna a lista mudada
removeItemsFromList :: Eq t => [t] -> [t] -> [t]
removeItemsFromList major [] = major
removeItemsFromList major (head:tail)
    | elem head major = removeItemsFromList newMajorList tail 
    | otherwise = removeItemsFromList major tail
    where
        newMajorList = filter (\x -> x /= head) major

-- Retorna a lista de posições possíveis alterada e se alterou ela, caso:
-- O caminho que estou agora está seguindo um caminho que já deu errado antes.
forAllWrongPaths :: [Int] -> [[Int]] -> [Position] -> [Possibility] -> Bool -> ([Position], Bool)
forAllWrongPaths  _ [] possiblePositions _ hasAltered = (possiblePositions, hasAltered)
forAllWrongPaths currentPath (head:tail) possiblePositions possibilities hasAltered
    | isPrefixOf currentPath head = 
        forAllWrongPaths currentPath tail newPossiblePositions possibilities True
    | otherwise = forAllWrongPaths currentPath tail possiblePositions possibilities hasAltered
    where
        positionToRemove = snd (possibilities!!(length currentPath - 1))
        newPossiblePositions = removeItemsFromList possiblePositions [positionToRemove]


------ INICIA FUNÇÃO DE BACKTRACKING (relativa à backtracking_preenche_numero em Python) ------

-- Dado um caminho, verificar se não há posições falhas a serem removidas nesse ponto da árvore.
-- (a partir da errorList). Retornar a lista de posições alterada (ou não).
removeErrorPositions :: [Int] -> [RegionError] -> [Position] -> [Position]
removeErrorPositions path errorList possiblePositions
    | isCurrentError || isFirstNumberError = removeItemsFromList possiblePositions (snd currentError)
    | otherwise = possiblePositions
    where
        order = length path -- Referente ao indíce do número na lista de erros
        currentError = errorList!!order -- É uma tupla, tipo RegionError
        lenErrorList = length (snd currentError)
        isCurrentError = order > 0 && lenErrorList > 0 && (fst currentError) == path 
        isFirstNumberError = order == 0 && lenErrorList > 0

-- Retorna a matriz de regiões e a região sem o preenchimento da posição dada
cleanFillNumber :: GenMatrix Int -> [Position] -> Position -> (GenMatrix Int, [Position])
cleanFillNumber mat region position = (newMatrix, newRegion)
    where
        newMatrix = changeElement mat position 0
        regionInfo = head region -- Primeira tupla da região
        newRegion = changeElementList region 0 (increaseSecond regionInfo)

-- Tenta um novo preenchimento após um anterior pois está seguindo um caminho errado (wrongPath)
tryAgainFillNumber :: GenMatrix Int -> GenMatrix String -> Int -> [Position] -> [Position] 
                    -> Position -> [Int] -> [Possibility] 
                    -> (GenMatrix Int, [Position],  [Int], Position)
tryAgainFillNumber mat matRegions number possiblePositions region lastPosition currentPath possibilities
    | succeeded = (returnMat, returnReg, newPath, pos)
    | otherwise = (mat, region, currentPath, lastPosition) -- Não pôde mudar o caminho atual. Inalterado
    where
        (newMatrix, newRegion) = cleanFillNumber mat region lastPosition 
        (succeeded, pos) = tryFillNumber newMatrix matRegions number possiblePositions region
        (returnMat, returnReg) = fillNumber newMatrix number pos newRegion
        newPath = (init currentPath) ++ [fromMaybe (-1) (elemIndex (number, pos) possibilities)]
        

-- Conseguiu preencher. Agora, chamar a forAllWrongPaths para checar se está seguindo um caminho falha,
-- E caso sim, tenta se livrar desse caso chamando tryAgainFillNumber
checkWrongPaths :: GenMatrix Int -> GenMatrix String -> [Int] -> GenMatrix Int 
                -> Position -> [Position] -> [Possibility] -> Int -> [Position]
                -> (GenMatrix Int, [Position],  [Int], Position)
checkWrongPaths mat matRegions path wrongPaths lastPosition possiblePositions possibilities number region
    | hasAltered = tryAgainFillNumber mat matRegions number newPossiblePositions region lastPosition path possibilities
    | otherwise = (mat, region, path, lastPosition) -- Inalterado
   where
    (newPossiblePositions, hasAltered) =  forAllWrongPaths path (getListFromMatrix wrongPaths) possiblePositions possibilities False


-- Altera errorList de região baseado numa posição que deu erro para o caminho atual
changeErrorList :: [RegionError] -> Position -> [Int] -> [RegionError]
changeErrorList errorList lastPos path =
    if ((length positionErrors) > 0) then 
        if (pathError == path) then
            changeElementList errorList order (path, positionErrors ++ [lastPos])
        else
            changeElementList errorList order (path, [lastPos])
    else 
        changeElementList errorList order (path, [lastPos])
    where
        order = length path -- Referente ao indíce do número na lista de erros
        regionError = errorList!!order -- (path, [position])
        pathError = fst regionError
        positionErrors = snd regionError

-- Falhou, chamar backtracking de novo para o mesmo número com a matriz e caminhos limpos 
-- E a errorList atualizada com a falha.
tryAgainSameNumber :: GenMatrix Int -> GenMatrix String -> [Int] -> [Position] 
                        -> [Position] -> [Possibility] -> [Int] -> [RegionError] 
                        -> GenMatrix Int -> (Position, Int) -> (Bool, GenMatrix Int, [Position], [Int])
tryAgainSameNumber mat matRegions possibleNumbers possiblePositions region possibilities path errorList wrongPaths lastPos = 
    backtrackingTryFillNumber newMatrix matRegions possibleNumbers newPossiblePositions newRegion possibilities newPath newErrorList wrongPaths
    where
        (newMatrix, newRegion) = cleanFillNumber mat region (fst lastPos)
        newPath = init path -- Remove ultimo do caminho
        newErrorList = changeErrorList errorList (fst lastPos) newPath
        newPossiblePositions = addElementList possiblePositions (snd lastPos) (fst lastPos)


-- Continuação do backtracking: após o preenchimento dar certo, eu tento um próximo.
-- Caso falhe, eu tento de novo para o mesmo número
continueBackTrackingTryFillNumber :: GenMatrix Int -> GenMatrix String -> [Int] -> [Position] 
                        -> [Position] -> [Possibility] -> [Int] -> [RegionError] 
                        -> GenMatrix Int -> (Position, Int) -> (Bool, GenMatrix Int, [Position], [Int])
continueBackTrackingTryFillNumber mat matRegions (head:tail) possiblePositions region possibilities path errorList wrongPaths lastPos =
    if (succeeded) then
        (True, newMat, newRegion, newPath)
    else
        tryAgainSameNumber mat matRegions (head:tail) possiblePositions region possibilities path errorList wrongPaths lastPos
    where
        (succeeded, newMat, newRegion, newPath) = fillWholeRegion mat matRegions tail possiblePositions region possibilities path errorList wrongPaths

-- Inicio do backtracking interno à região
backtrackingTryFillNumber :: GenMatrix Int -> GenMatrix String -> [Int] -> [Position] 
                        -> [Position] -> [Possibility] -> [Int] -> [RegionError] 
                        -> GenMatrix Int -> (Bool, GenMatrix Int, [Position], [Int])
backtrackingTryFillNumber mat matRegions (head:tail) possiblePositions region possibilities path errorList wrongPaths =
    if (succeeded) then 
        continueBackTrackingTryFillNumber returnMat matRegions (head:tail) possiblePosNext returnReg possibilities newPath errorList wrongPaths lastPos
    else
        (False, mat, region, path)
    where
        newPossiblePositions = removeErrorPositions path errorList possiblePositions
        (succeeded, pos) = tryFillNumber mat matRegions head newPossiblePositions region
        (newMat, newRegion) = fillNumber mat head pos region

        currentPath = path ++ [fromMaybe (-1) (elemIndex (head, pos) possibilities)] 
        (returnMat, returnReg, newPath, newPosition) = checkWrongPaths newMat matRegions currentPath wrongPaths pos newPossiblePositions possibilities head newRegion

        lastPos = (newPosition, fromMaybe (-1) (elemIndex newPosition possiblePositions) )
        possiblePosNext = removeItemsFromList possiblePositions [newPosition]


-- Enquanto existir números para preencher, vai para o backtracking.
-- Se não há, verifica se todas as posições foram preenchidas ou se o caminho final é um que já falhou.
fillWholeRegion :: GenMatrix Int -> GenMatrix String -> [Int] -> [Position] 
                        -> [Position] -> [Possibility] -> [Int] -> [RegionError] 
                        -> GenMatrix Int -> (Bool, GenMatrix Int, [Position], [Int])
fillWholeRegion mat matRegions possibleNumbers possiblePositions region possibilities path errorList wrongPaths
    | (((length possibleNumbers) == 0) && ((getSecond (region!!0)) /= 0)) = 
        (False, mat, region, path)
    | (((length possibleNumbers) == 0) && ((getSecond (region!!0) == 0))) = 
        ((not (elem path (getListFromMatrix wrongPaths))), mat, region, path)
    | otherwise = 
        backtrackingTryFillNumber mat matRegions possibleNumbers possiblePositions region possibilities path errorList wrongPaths


-- Inicia a solução de uma região, retornando se falhou ou não (com os valores alterados ou inalterados)
solveByRegion :: GenMatrix Int -> GenMatrix String -> GenMatrix Position -> [[Int]] -> 
                GenMatrix Int -> [Int] -> [Position] 
                -> (Bool, GenMatrix Int, [[Int]])
solveByRegion mat matRegions regions regionsPaths wrongPaths possibleNumbers possiblePositions
    | succeeded = (True, newMat, newRegPaths)
    | otherwise = (False, mat, regionsPaths)
    where
        region = head (getListFromMatrix regions)
        possibilities = (getPossibilitiesList possibleNumbers possiblePositions)
        errorList = replicate (length possibleNumbers) ([], [])
        (succeeded, newMat, newReg, newPath) = fillWholeRegion mat matRegions possibleNumbers possiblePositions region possibilities [] errorList wrongPaths
        newRegPaths = regionsPaths ++ [newPath]