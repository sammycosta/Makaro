module CertainSolutions (missingNumbers, iterateList, 
                        fillMissingOne, fillMissingTwo) where


import Data.List
import Matrix
import PositionUtils
import Validations

-- Função auxiliar de missingNumbers que itera sobre a lista de números possíveis e sobre a lista de regiões,
-- retornando os números ainda não utilizados em uma região e as posições não preenchidas
iterateList :: [Int] -> [Position] -> [Position] -> GenMatrix Int -> ([Int], [Position])
iterateList possibilities empty_pos list mat =
    if (length list) > 0 then
        if (number) /= 0 then
            iterateList (possibilities \\[number]) empty_pos (deleteFirst list) mat
        else
            iterateList possibilities (empty_pos++[(list!!0)]) (deleteFirst list) mat
    else
        (possibilities, empty_pos)
    where
        number = (getElement mat (list!!0))

-- Retorna os números que ainda não foram utilizados em uma região e as posições não preenchidas
missingNumbers :: [Position] -> GenMatrix Int -> ([Int], [Position])
missingNumbers region_list mat =
    iterateList [1..((getFirst (region_list!!0)))] [] (deleteFirst region_list) mat

-- Função auxiliar de fillMissingOne que realiza a verificação do número de posições não preenchidas na região 
-- e realiza seu preenchimento caso haja apenas uma
fillMat1 :: Int -> ([Int], [Position]) -> GenMatrix Int -> GenMatrix Position -> (GenMatrix Int, GenMatrix Position)
fillMat1 i (missing, positions) mat regions = 
    if (getSecond (getElement regions (i, 0))) == 1 then 
        ((changeElement mat (positions!!0) (missing!!0)), (changeElement regions  (i, 0) (decreaseSecond (getElement regions (i, 0)))))
    else
        (mat, regions)

-- Chama a função fillMat1 para todas as regiões, de forma a preencher todasas regiões com apenas uma posição 
-- não preenchida (só há uma opção para o número, nesse caso)
fillMissingOne :: Int -> (GenMatrix Int, GenMatrix Position) -> (GenMatrix Int, GenMatrix Position)
fillMissingOne i (mat, regions) = 
    if i+1 < ((getRowsNumber regions)) then
        fillMissingOne (i+1) missing_num
    else
        missing_num
    where
        missing_num = (fillMat1 i (missingNumbers (getRow regions i) mat) mat regions)

-- Função chamada por verifications que recebe as posições e os valores e realiza a atualização na matriz
changeMatrices :: Int -> Int -> Int -> Int -> Int -> ([Int], [Position]) -> GenMatrix Int -> GenMatrix Position -> (GenMatrix Int, GenMatrix Position)
changeMatrices p1 m1 p2 m2 i (missing, positions) mat regions = 
    ((changeElement (changeElement mat (positions!!p1) (missing!!m1)) (positions!!p2) (missing!!m2)), 
        (changeElement regions  (i, 0) (decreaseSecond(decreaseSecond (getElement regions (i, 0))))))

-- Função auxiliar de fillMat2 que verifica se é possível determinar qual posição deve receber qual valor, 
-- e caso isso seja possível, chama changeMatrices para fazer as atualizações dos valores
verifications :: Int -> ([Int], [Position]) -> GenMatrix Int -> GenMatrix Position -> (GenMatrix Int, GenMatrix Position)
verifications i (missing, positions) mat regions
    | ((isAdjacent mat (missing!!0) (positions!!0)) || (isAdjacent mat (missing!!1) (positions!!1)))= 
        (changeMatrices 0 1 1 0 i (missing, positions) mat regions)
    | ((isAdjacent mat (missing!!1) (positions!!0)) || (isAdjacent mat (missing!!0) (positions!!1))) =
        (changeMatrices 1 1 0 0 i (missing, positions) mat regions)
    | otherwise = (mat, regions)

-- Verifica a região passada e chama verifications para tentar preencher as posições vazias caso haja apenas duas
fillMat2 :: Int -> GenMatrix Int -> GenMatrix Position -> (GenMatrix Int, GenMatrix Position)
fillMat2 i mat regions = 
    if (getSecond (getElement regions (i, 0))) == 2 then 
       (verifications i (missingNumbers (getRow regions i) mat) mat regions)
    else
        (mat, regions)

-- Chama a função fillMat2 para todas as regiões, de forma a preencher as regiões com apenas duas posições vazias 
-- nas quais é possível determinar com certeza os valores de cada uma por causa de seus vizinhos ortogonais
fillMissingTwo :: Int -> (GenMatrix Int, GenMatrix Position) -> (GenMatrix Int, GenMatrix Position)
fillMissingTwo i (mat, regions) = 
    if i+1 < ((getRowsNumber regions)) then
        fillMissingTwo (i+1) (fillMat2 i mat regions)
    else
       (fillMat2 i mat regions)
    