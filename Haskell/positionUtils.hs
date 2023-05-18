module PositionUtils (getFirst, getSecond, increaseFirst, increaseSecond, decreaseFirst, decreaseSecond) where

import Matrix

-- Primeiro elemento de uma posição
getFirst :: Position -> Int
getFirst (a, _) = a

-- Segundo elemento de uma posição
getSecond :: Position -> Int
getSecond (_, b) = b

-- Retorna a posição com o primeiro elemento incrementado em 1
increaseFirst :: Position -> Position
increaseFirst (a, b) = (a+1, b)

-- Retorna a posição com o segundo elemento incrementado em 1
increaseSecond :: Position -> Position
increaseSecond (a, b) = (a, b+1)

-- Retorna a posição com o primeiro elemento decrementado em 1
decreaseFirst :: Position -> Position
decreaseFirst (a, b) = (a-1, b)

-- Retorna a posição com o segundo elemento decrementado em 1
decreaseSecond :: Position -> Position
decreaseSecond (a, b) = (a, b-1)