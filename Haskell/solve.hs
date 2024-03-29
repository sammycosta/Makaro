module Solve(solve, 
        backtracking, continueBacktracking, tryAgainSameRegion,
        changePuzzleErrorList, cleanRegion, makeWrongPathList) where

import SolveRegion
import Matrix
import PositionUtils
import Validations
import Data.List
import Data.Maybe (fromMaybe)
import CertainSolutions

type PuzzleError = ([[Int]], [[Int]]) -- (regionPath "pai", paths errados pra região)

-- Cria uma lista de caminhos errados na região específica a partir da lista total de puzzleError
makeWrongPathList :: [[Int]] -> [PuzzleError] -> (GenMatrix Int)
makeWrongPathList regionsPaths regionErrorList
    | isCurrentError || isFirstRegionError = Matrix(snd currentError)
    | otherwise = Matrix([]) -- Volta vazia. Não há caminhos que causaram falha nesse ponto da árvore
    where
        order = length regionsPaths -- Referente ao indíce da região na lista de erros
        currentError = regionErrorList!!order -- Tupla PuzzleError.
        lenErrorList = length (snd currentError)
        isCurrentError = order > 0 && lenErrorList > 0 && ((fst currentError) == regionsPaths)
        isFirstRegionError = order == 0 && lenErrorList > 0

 -- Coloca 0 em todas as posições dadas na matriz
cleanRegion :: GenMatrix Int -> [Position] -> GenMatrix Int
cleanRegion mat [] = mat
cleanRegion mat (head:tail) = cleanRegion newMat tail
    where
        newMat = changeElement mat head 0

-- Atualiza a lista de erros totais baseado em um caminho que causou uma falha em uma região específica
changePuzzleErrorList :: [PuzzleError] ->  [[Int]] -> [Int] -> [PuzzleError]
changePuzzleErrorList puzzleErrorList regionsPaths failedPath =
    if ((length pathErrors) > 0) then
        if (regionPathError == regionsPaths) then
            changeElementList puzzleErrorList order (regionsPaths, pathErrors ++ [failedPath])
        else
            changeElementList puzzleErrorList order (regionsPaths, [failedPath])
    else
        changeElementList puzzleErrorList order (regionsPaths, [failedPath])
    where
        order = length regionsPaths -- Referente ao indíce da região na lista de erros
        puzzleError = puzzleErrorList!!order
        regionPathError = fst puzzleError
        pathErrors = snd puzzleError

-- Falhou, chamar backtracking de novo para a mesma região com a matriz e caminhos limpos 
-- E a puzzleErrorList atualizada com a falha.
tryAgainSameRegion :: GenMatrix Int -> GenMatrix String -> GenMatrix Position -> [[Int]] 
        -> [PuzzleError] -> [Position] -> (Bool, GenMatrix Int, [PuzzleError])
tryAgainSameRegion mat matRegions regions regionsPaths puzzleErrorList possiblePositions
    | ((length failedPath) > 0) = backtracking newMat matRegions regions newRegionsPath newPuzzleErrorList
    | otherwise = (False, mat, newPuzzleErrorList)
    where
        failedPath = last regionsPaths
        newRegionsPath = init regionsPaths -- Pop
        newPuzzleErrorList = changePuzzleErrorList puzzleErrorList newRegionsPath failedPath
        newMat = cleanRegion mat possiblePositions

-- Continuação do backtracking: após o preenchimento dar certo, eu tento um próximo.
-- Caso falhe, eu tento de novo para a mesma região
continueBacktracking :: GenMatrix Int -> GenMatrix String -> GenMatrix Position -> [[Int]] -> [PuzzleError]
        -> [Position] -> (Bool, GenMatrix Int, [PuzzleError])
continueBacktracking mat matRegions regions regionsPaths puzzleErrorList possiblePositions =
        if succeeded then
            (True, newMat, newPuzzleErrorList)
        else
            tryAgainSameRegion newMat matRegions regions regionsPaths puzzleErrorList possiblePositions
        where
            newRegions = Matrix(tail (getListFromMatrix regions))
            (auxMat, auxReg) = certainties (mat, newRegions) -- Roda certezas em cima do resultado
            (succeeded, newMat, newPuzzleErrorList) = backtracking auxMat matRegions auxReg regionsPaths puzzleErrorList

-- Inicio do backtracking de regiões
backtracking:: GenMatrix Int -> GenMatrix String -> GenMatrix Position -> [[Int]] -> [PuzzleError]
    -> (Bool, GenMatrix Int, [PuzzleError])
backtracking mat matRegions regions regionsPaths puzzleErrorList
    | length (getListFromMatrix regions) == 0 = (True, mat, puzzleErrorList)
    | succeeded = continueBacktracking newMat matRegions regions newRegPaths puzzleErrorList possiblePositions
    | otherwise = (False, mat, puzzleErrorList)
    where
        wrongPaths = makeWrongPathList regionsPaths puzzleErrorList
        (possibleNumbers, possiblePositions) = missingNumbers (getRow regions 0) mat
        (succeeded, newMat, newRegPaths) = solveByRegion mat matRegions regions regionsPaths wrongPaths possibleNumbers possiblePositions


-- Inicia a solução do puzzle
solve :: GenMatrix Int -> GenMatrix String -> GenMatrix Position -> (Bool, GenMatrix Int)
solve mat matRegions regions = (succeeded, newMat)
    where
        puzzleErrorList = replicate (length (getListFromMatrix regions)) ([], []) 
        (succeeded, newMat, e) = backtracking mat matRegions regions [] puzzleErrorList