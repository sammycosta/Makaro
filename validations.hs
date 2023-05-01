module Validations() where

import Matrix
import CertainSolutions
import ReadPuzzle

-- pensar nos otherwise/else

-- NAO TESTADA botar otherwise Dada uma posição de seta, retorna a posição que ela aponta.
getAimed :: GenMatrix String -> Position -> Position
getAimed regionsMatrix arrowPosition
    | getElement (regionsMatrix arrowPosition) == "L" = decreaseSecond arrowPosition
    | getElement (regionsMatrix arrowPosition) == "R" = increaseSecond arrowPosition
    | getElement (regionsMatrix arrowPosition) == "U" = decreaseFirst arrowPosition
    | getElement (regionsMatrix arrowPosition) == "D" = increaseFirst arrowPosition

-- NAO TESTADA Dada uma posição de seta, retorna a lista de números ao redor que não é o apontado
getAroundList :: GenMatrix int -> Position -> Position -> [Int]
getAroundList mat arrowPosition aimedPosition = aroundList
    where
        n = getRowsNumber

    aroundList = 
        [getElement mat (increaseFirst arrowPosition) | getFirst arrowPosition < (n - 1) && (increaseFirst arrowPosition) /= aimedPosition] ++
        [getElement mat (decreaseFirst arrowPosition) | getFirst arrowPosition > 0 && (decreaseFirst arrowPosition) /= aimedPosition ] ++ 
        [getElement mat (increaseSecond arrowPosition) | getSecond arrowPosition < (n - 1) && (increaseSecond arrowPosition) /= aimedPosition] ++
        [getElement mat (decreaseSecond arrowPosition) | getSecond arrowPosition > 0 && (decreaseSecond arrowPosition) /= aimedPosition ]

-- NAO TESTADA Verificar qual é o maior número da lista. Começar passando o primeiro elemento.
getBigger :: Int -> [Int] -> Int
getBigger number [] = number 
getBigger number (head:tail) 
    | number > head = getBigger number tail
    | otherwise = getBigger head tail

-- NAO TESTADA Dada a lista de números ao redor da seta, retornar o maior. 
verifyBiggerAroundArrow :: [Int] -> Int
verifyBiggerAroundArrow [] = error "Lista vazia"
verifyBiggerAroundArrow (head:tail) = getBigger head tail

 -- NAO TESTADA Avalia se o número pode ser colocado na posição em função da seta
validByArrow :: GenMatrix String -> GenMatrix Int -> Position -> Position -> Int -> Bool
validByArrow matRegions mat arrowPosition numberPosition number
    | aimedPosition == numberPosition = number > (verifyBiggerAroundArrow aroundList)
    | (getElement mat aimedPosition) > 0 = number < (getElement mat aimedPosition)
    | otherwise = True
    where
        aimedPosition = getAimed matRegions arrowPosition
        aroundList = getAroundList mat arrowPosition aimedPosition

-- isArrow :: GenMatrix String -> 