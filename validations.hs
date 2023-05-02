module Validations (isAdjacent, isValid, 
        getAimed, getAroundList, getBigger, verifyBiggerAroundArrow,
        validByArrow, isArrow, getAroundArrowsList, validByAroundArrows, validNumberPositionByArrows) where

import Matrix
import ReadPuzzle
import PositionUtils

-- Dada uma posição de seta, retorna a posição que ela aponta.
getAimed :: GenMatrix String -> Position -> Position
getAimed matRegions arrowPosition
    | (getElement matRegions arrowPosition) == "L" = decreaseSecond arrowPosition
    | (getElement matRegions arrowPosition) == "R" = increaseSecond arrowPosition
    | (getElement matRegions arrowPosition) == "U" = decreaseFirst arrowPosition
    | (getElement matRegions arrowPosition) == "D" = increaseFirst arrowPosition
    | otherwise = error "Posição de seta não contém seta"

-- Dada uma posição de seta, retorna a lista de números ao redor que não é o apontado
getAroundList :: GenMatrix Int -> Position -> Position -> [Int]
getAroundList mat arrowPosition aimedPosition = 
        [getElement mat (increaseFirst arrowPosition) | getFirst arrowPosition < (n - 1) && (increaseFirst arrowPosition) /= aimedPosition] ++
        [getElement mat (decreaseFirst arrowPosition) | getFirst arrowPosition > 0 && (decreaseFirst arrowPosition) /= aimedPosition] ++ 
        [getElement mat (increaseSecond arrowPosition) | getSecond arrowPosition < (n - 1) && (increaseSecond arrowPosition) /= aimedPosition] ++
        [getElement mat (decreaseSecond arrowPosition) | getSecond arrowPosition > 0 && (decreaseSecond arrowPosition) /= aimedPosition]
    where
        n = getRowsNumber mat

-- Verificar qual é o maior número da lista. Começar passando o primeiro elemento.
getBigger :: Int -> [Int] -> Int
getBigger number [] = number 
getBigger number (head:tail) 
    | number > head = getBigger number tail
    | otherwise = getBigger head tail

-- Dada a lista de números ao redor da seta, retornar o maior. 
verifyBiggerAroundArrow :: [Int] -> Int
verifyBiggerAroundArrow [] = error "Lista vazia" -- verificar essa lógica
verifyBiggerAroundArrow (head:tail) = getBigger head tail

 -- Avalia se o número pode ser colocado na posição em função da seta
validByArrow :: GenMatrix String -> GenMatrix Int -> Position -> Position -> Int -> Bool
validByArrow matRegions mat arrowPosition numberPosition number
    | aimedPosition == numberPosition = number > (verifyBiggerAroundArrow aroundList)
    | (getElement mat aimedPosition) > 0 = number < (getElement mat aimedPosition)
    | otherwise = True
    where
        aimedPosition = getAimed matRegions arrowPosition
        aroundList = getAroundList mat arrowPosition aimedPosition

-- Avalia se posição tem uma seta.
isArrow :: GenMatrix String -> Position -> Bool
isArrow matRegions position = (c == "L" || c == "R" || c == "U" || c == "D")
    where
        c = getElement matRegions position

-- Dada uma posição, retorna uma lista com as posições de setas ao redor dela.
getAroundArrowsList :: GenMatrix String -> Position -> [Position]
getAroundArrowsList matRegions position = 
    [increaseFirst position | (getFirst position) < (n-1) && isArrow matRegions (increaseFirst position)] ++
    [decreaseFirst position | (getFirst position) > 0 && isArrow matRegions (decreaseFirst position)] ++
    [increaseSecond position | (getSecond position) < (n-1) && isArrow matRegions (increaseSecond position)] ++
    [decreaseSecond position | (getSecond position) > 0 && isArrow matRegions (decreaseSecond position)]
    where
        n = getRowsNumber matRegions

-- NAO TESTADA chama a função validbyArrow para todas as setas ao redor da posição
validByAroundArrows :: GenMatrix Int -> GenMatrix String -> Int -> Position -> [Position] -> Bool
validByAroundArrows _ _ _ _ [] = True
validByAroundArrows mat matRegions number numberPosition (head:tail)
    | validByArrow matRegions mat head numberPosition number = validByAroundArrows mat matRegions number numberPosition tail
    | otherwise = False

-- NAO TESTADA Dada um número e uma posição, retorna se é valido colocar ele nela em função das setas.
validNumberPositionByArrows :: GenMatrix Int -> GenMatrix String -> Int -> Position -> Bool
validNumberPositionByArrows mat matRegions number position = 
    validByAroundArrows mat matRegions number position aroundArrows
    where
        aroundArrows = getAroundArrowsList matRegions position

-- Retorna True se houver um adjacente igual nessa opção, False se não.
isAdjacent :: GenMatrix Int -> Int -> Position -> Bool
isAdjacent mat number position =
    (getFirst position < (n-1) && getElement mat (increaseFirst position) == number) ||
    (getFirst position > 0 && getElement mat (decreaseFirst position) == number) ||
    (getSecond position < (n-1) && getElement mat (increaseSecond position) == number) ||
    (getSecond position > 0 && getElement mat (decreaseSecond position) == number)
    where
        n = getRowsNumber mat

-- NAO TESTADA É valida para todas as regras.
isValid :: GenMatrix Int -> GenMatrix String -> Int -> Position -> Bool
isValid mat matRegions number position =
    (not (isAdjacent mat number position)) && (validNumberPositionByArrows mat matRegions number position)