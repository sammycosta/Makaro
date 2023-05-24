(defpackage :SolveRegion
    (:use :common-lisp)
    (:export)
    )

; positionNumber : int [myposition] [[int, myposition]] -> [[int, myposition]]
; Junta um número com várias posições, criando uma lista de possibilidades
(defun positionNumber (num positions possibilities)
    (if (null positions)
        possibilities
        (let ((newPossibilities (append possibilities (list (list num (car positions))))))
            (positionNumber num (cdr positions) newPossibilities)
        )
    )
)

; numberPosition : [int] [myposition] [[int, myposition]] -> [[int, myposition]]
; Chamada positionNumber para todos os números na lista
(defun numberPosition (numbers possiblePositions possibilities)
    (if (null numbers)
        possibilities
        (let ((newPossibilities (positionNumber (car numbers) possiblePositions possibilities)))
            (numberPosition (cdr numbers) possiblePositions newPossibilities)
        )
    )
)

; getPossibilitiesList : [int] [myposition] -> [[int, myposition]]
; Faz a lista de possibilidades a partir de uma lista de números e uma de posições
(defun getPossibilitiesList (possibleNumbers possiblePositions)
    (numberPosition possibleNumbers possiblePositions '())))
)

; VERIFICAR DEPOIS SE REGIÃO VAI SER LISTA DE MYPOSITION OU LIST DE [INT, INT]
; FAZER ISVALID DO VALIDATIONS
; tryFillNumber : [[int]] [[string]] int [[myposition]] [myposition] -> [bool, myposition]
; Tenta preencher um número em todas as posições até conseguir
(defun tryFillNumber (mat matRegions num positions region)
    (if (null positions)
        (list nil '(-1 -1))
        (if (Validations:isValid mat matRegions num (car positions))
            (list t (car positions))
            (tryFillNumber mat matRegions num (cdr positions) region)
        )
)

; fillNumber : [[int]] int myposition [myposition] -> [[int]]
; Dada uma posição e número, retorna a matriz e a região com o número colocado na posição;
(defun fillNumber (mat num pos region)
    (let* ((newMatrix (Matrix:changeElement mat pos num))
            (regionInfo (car region)) ; Primeira tupla da região
            (newRegion (Matrix:changeElementList region 0 (positionUtils:decreaseSecond regionInfo))))
        (list newMatrix newRegion)
    )
)

; filter : (int -> bool) [int] -> [int]
; não testado; mas é pra ser equivalente a do professor
(defun filter (f l)
    (cond ((null l) nil)
          ((funcall f (car l)) (cons (car l) (filter f (cdr l))))
          (t (filter f (cdr l)))))

; removeItemsFromList : [int] [int] -> [int]
; Remove itens da lista principal se existirem na auxiliar. Retorna a lista modificada
; se possível testar, e pro caso onde na major nao tem nada da aux
(defun removeItemsFromList (major aux)
    (if (null aux)
        major
        (let ((newMajorList (filter (lambda (x) (not (equal x (car aux)))) major)))
            removeItemsFromList newMajorList (cdr aux)
        )
    )
)

; isPrefixOf : [int] [int] -> bool
; Retorna se a primeira lista é prefixo da segunda (não testada)
(defun isPrefixOf (l1 l2)
    (cond ((null l1) t)
          ((null l2) nil)
          ((equal (car l1) (car l2)) (isPrefixOf (cdr l1) (cdr l2)))
          (t nil)))

; forAllWrongPaths : [Int] [[Int]] [myposition] [int, myposition] bool -> [[myposition], bool]
; Retorna a lista de posições possíveis alterada e se alterou ela, caso:
; O caminho que estou agora está seguindo um caminho que já deu errado antes.
(defun forAllWrongPaths (currentPath wrongPaths possiblePositions possibilities hasAltered)
    (if (null wrongPaths)
        (list possiblepPositions hasAltered)
        (let ((positionToRemove (cadr (nth possibilities (- (length currentPath) 1))))
              (newPossiblePositions (removeItemsFromList possiblePositions (list positionToRemove))))
            (if (isPrefixOf currentPath (car wrongPaths))
                (forAllWrongPaths currentPath (cdr wrongPaths) newPossiblePositions possibilities t)
                (forAllWrongPaths currentPath (cdr wrongPaths) possiblePositions possibilities hasAltered)
            )
        )
    )
)


; INICIO BACKTRACKING ----------------------------------------------------------

; removeErrorPositions : [int] [[[int], [myposition]]] [myposition] -> [myposition]
; Dado um caminho, verificar se não há posições falhas a serem removidas nesse ponto da árvore.
; (a partir da errorList). Retornar a lista de posições alterada (ou não).
(defun removeErrorPositions (path errorList possiblePositions)
    (let * ((order (length path)) ; Referente ao indíce do número na lista de erros
            (currentError (nth errorList order)) ; Tupla [[int], [myposition]] (regionError)
            (lenErrorList (length (cadr currentError)))
            (isCurrentError (and (> order 0) (> lenErrorList 0) (equal (car currentError) path)))
            (isFirstNumberError (and (= order 0) (> lenErrorList 0))))
        (if (or isCurrentError isFirstNumberError)
            (removeItemsFromList possiblePositions (cadr currentError))
            possiblepositions
        )
    )
)

; cleanFillNumber : [[int]] [myposition] myposition -> [[[int]], [myposition]]
; Retorna a matriz de regiões e a região sem o preenchimento da posição dada
(defun cleanFillNumber (mat region pos)
    (let* ((newMatrix (Matrix:changeElement mat pos 0))
            (regionInfo (car region)) ; Primeira tupla da região
            (newRegion (Matrix:changeElementList region 0 (positionUtils:increaseSecond regionInfo))))
        (list newMatrix newRegion)
    )
)


; tryAgainFillNumber :: [[int] [[string]]] int [myposition] [myposition] myposition [int] [[int, myposition]] 
                        ; -> [[[int]], [myposition], [int], myposition]
; Tenta um novo preenchimento após um anterior pois está seguindo um caminho errado (wrongPath)
(defun tryAgainFillNumber (mat matRegions num possiblePositions regiion lastPosition currentPath possibilities)
    (let * (
        (cleaned (cleanFillNumber mat region lastPosition))
        (newMatrix (car cleaned))
        (newRegion (cadr cleaned))
        (tryFillResult (tryFillNumber newMatrix matRegions num possiblePositions region))
        (succeeded (car tryFillResult))
        (pos (cadr tryFillResult))
        (fillResult (fillNumber newMatrix num pos newRegion))
        (returnMat (car fillResult))
        (returnReg (cadr fillResult))
        (newPath ())) ;; FAZER
        
        (if succeeded
            (list returnMat returnReg newPath pos)
            (list mat region currentPath lastPosition)
        )
    )

)
