(defpackage :SolveRegion
    (:use :common-lisp)
    (:export :solveByRegion
            :replicate))

(in-package :SolveRegion)

;; AUXILIARES CRIADAS NA VERSAO LISP

;; filter : (int -> bool) [int] -> [int]
;; não testado; mas é pra ser equivalente a do professor
(defun filter (f l)
    (cond ((null l) nil)
          ((funcall f (car l)) (cons (car l) (filter f (cdr l))))
          (t (filter f (cdr l)))))

;; isPrefixOf : [int] [int] -> bool
;; Retorna se a primeira lista é prefixo da segunda (não testada)
(defun isPrefixOf (l1 l2)
    (cond ((null l1) t)
          ((null l2) nil)
          ((equal (car l1) (car l2)) (isPrefixOf (cdr l1) (cdr l2)))
          (t nil)))

;; replicate : int t -> [t]
;; Retorna uma lista com n elementos iguais a value
(defun replicate (n value)
    (if (= n 0)
        '()
        (cons value (replicate (- n 1) value))
    )
)

;; FIM AUXILIARES

;; positionNumber : int [myposition] [[int, myposition]] -> [[int, myposition]]
;; Junta um número com várias posições, criando uma lista de possibilidades
(defun positionNumber (num positions possibilities)
    (if (null positions)
        possibilities
        (let ((newPossibilities (append possibilities (list (list num (car positions))))))
            (positionNumber num (cdr positions) newPossibilities)
        )
    )
)

;; numberPosition : [int] [myposition] [[int, myposition]] -> [[int, myposition]]
;; Chamada positionNumber para todos os números na lista
(defun numberPosition (numbers possiblePositions possibilities)
    (if (null numbers)
        possibilities
        (let ((newPossibilities (positionNumber (car numbers) possiblePositions possibilities)))
            (numberPosition (cdr numbers) possiblePositions newPossibilities)
        )
    )
)

;; getPossibilitiesList : [int] [myposition] -> [[int, myposition]]
;; Faz a lista de possibilidades a partir de uma lista de números e uma de posições
(defun getPossibilitiesList (possibleNumbers possiblePositions)
    (numberPosition possibleNumbers possiblePositions '())
)

;; VERIFICAR DEPOIS SE REGIÃO VAI SER LISTA DE MYPOSITION OU LIST DE [INT, INT]
;; FAZER ISVALID DO VALIDATIONS
;; tryFillNumber : [[int]] [[string]] int [[myposition]] [myposition] -> [bool, myposition]
;; Tenta preencher um número em todas as posições até conseguir
(defun tryFillNumber (mat matRegions num positions region)
    (if (null positions)
        (list nil '(-1 -1))
        (if (Validations:isValid mat matRegions num (car positions))
            (list t (car positions))
            (tryFillNumber mat matRegions num (cdr positions) region)
        )
    )
)

;; fillNumber : [[int]] int myposition [myposition] -> [[int]]
;; Dada uma posição e número, retorna a matriz e a região com o número colocado na posição;
(defun fillNumber (mat num pos region)
    (let* ((newMatrix (Matrix:changeElement mat pos num))
            (regionInfo (car region)) ; Primeira tupla da região
            (newRegion (Matrix:changeElementList region 0 (PosUtils:decreaseSecond regionInfo))))
        (list newMatrix newRegion)
    )
)

;; removeItemsFromList : [int] [int] -> [int]
;; Remove itens da lista principal se existirem na auxiliar. Retorna a lista modificada
;; se possível testar, e pro caso onde na major nao tem nada da aux
(defun removeItemsFromList (major aux)
    (if (null aux)
        major
        (let ((newMajorList (filter (lambda (x) (not (equal x (car aux)))) major)))
            (removeItemsFromList newMajorList (cdr aux))
        )
    )
)

;; forAllWrongPaths : [Int] [[Int]] [myposition] [int, myposition] bool -> [[myposition], bool]
;; Retorna a lista de posições possíveis alterada e se alterou ela, caso:
;; O caminho que estou agora está seguindo um caminho que já deu errado antes.
(defun forAllWrongPaths (currentPath wrongPaths possiblePositions possibilities hasAltered)
    (if (null wrongPaths)
        (list possiblePositions hasAltered)
        (let ((positionToRemove (cadr (nth (- (length currentPath) 1) possibilities)))
              (newPossiblePositions (removeItemsFromList possiblePositions (list positionToRemove))))
            (if (isPrefixOf currentPath (car wrongPaths))
                (forAllWrongPaths currentPath (cdr wrongPaths) newPossiblePositions possibilities t)
                (forAllWrongPaths currentPath (cdr wrongPaths) possiblePositions possibilities hasAltered)
            )
        )
    )
)


;; INICIO BACKTRACKING ----------------------------------------------------------

;; removeErrorPositions : [int] [[[int], [myposition]]] [myposition] -> [myposition]
;; Dado um caminho, verificar se não há posições falhas a serem removidas nesse ponto da árvore.
;; (a partir da errorList). Retornar a lista de posições alterada (ou não).
(defun removeErrorPositions (path errorList possiblePositions)
    (let* ((order (length path)) ; Referente ao indíce do número na lista de erros
            (currentError (nth order errorList)) ; Tupla [[int], [myposition]] (regionError)
            (lenErrorList (length (cadr currentError)))
            (isCurrentError (and (> order 0) (> lenErrorList 0) (equal (car currentError) path)))
            (isFirstNumberError (and (= order 0) (> lenErrorList 0))))
        (if (or isCurrentError isFirstNumberError)
            (removeItemsFromList possiblePositions (cadr currentError))
            possiblePositions
        )
    )
)

;; cleanFillNumber : [[int]] [myposition] myposition -> [[[int]], [myposition]]
;; Retorna a matriz de regiões e a região sem o preenchimento da posição dada
(defun cleanFillNumber (mat region pos)
    (let* ((newMatrix (Matrix:changeElement mat pos 0))
            (regionInfo (car region)) ; Primeira tupla da região
            (newRegion (Matrix:changeElementList region 0 (PosUtils:increaseSecond regionInfo))))
        (list newMatrix newRegion)
    )
)


;; tryAgainFillNumber :: [[int] [[string]]] int [myposition] [myposition] myposition [int] [[int, myposition]] 
                        ; -> [[[int]], [myposition], [int], myposition]
;; Tenta um novo preenchimento após um anterior pois está seguindo um caminho errado (wrongPath)
(defun tryAgainFillNumber (mat matRegions num possiblePositions region lastPosition currentPath possibilities)
    (let* (
        (cleaned (cleanFillNumber mat region lastPosition))
        (newMatrix (car cleaned))
        (newRegion (cadr cleaned))
        (tryFillResult (tryFillNumber newMatrix matRegions num possiblePositions region))
        (succeeded (car tryFillResult))
        (pos (cadr tryFillResult)))

        (if succeeded
            (let* (
                (fillResult (fillNumber newMatrix num pos newRegion))
                (returnMat (car fillResult))
                (returnReg (cadr fillResult))
                (newPath (append (butlast currentPath) (list (position (list num pos) possibilities)))))
                (list returnMat returnReg newPath pos)
            )
            (list mat region currentPath lastPosition)
        )
    )
)

;; checkWrongPaths : [[int]] [[string]] [int] [[int]] myposition [myposition] [[int, myposition]] int [myposition] -> 
                    ; [[[int]] [myposition] [int] myposition]
;; Conseguiu preencher. Agora, chamar a forAllWrongPaths para checar se está seguindo um caminho falha, 
;; E caso sim, tenta se livrar desse caso chamando tryAgainFillNumber
(defun checkWrongPaths (mat matRegions path wrongPaths lastPosition possiblePositions possibilities num region)
    (let* (
        (forAllResult (forAllWrongPaths path wrongPaths possiblePositions possibilities nil))
        (newPossiblePositions (car forAllResult))
        (hasAltered (cadr forAllResult)))

        (if hasAltered
            (tryAgainFillNumber mat matRegions num newPossiblePositions region lastPosition path possibilities)
            (list mat region path lastPosition) ; Inalterado
        )
    )
)

;; changeErrorList : [[int], [myposition]] myposition [int] -> [[int], [myposition]]
;; Altera errorList de região baseado numa posição que deu erro para o caminho atual
(defun changeErrorList (errorList lastPos path)
    (let* (
        (order (length path))
        (regionError (nth order errorList))
        (pathError (car regionError))
        (positionErrors (cadr regionError)))

        (if (> (length positionErrors) 0)
            (if (equal pathError path)
                (Matrix:changeElementList errorList order (list path (append positionErrors (list lastPos))))
                (Matrix:changeElementList errorList order (list path (list lastPos)))
            )
            (Matrix:changeElementList errorList order (list path (list lastPos)))
        )
    )
)

;; tryAgainSameNumber 
;; Falhou, chamar backtracking de novo para o mesmo número com a matriz e caminhos limpos 
;; E a errorList atualizada com a falha.
(defun tryAgainSameNumber (mat matRegions possibleNumbers possiblePositions region possibilities path errorList wrongPaths lastPos)
    (let* (
        (cleaned (cleanFillNumber mat region (car lastPos)))
        (newMatrix (car cleaned))
        (newRegion (cadr cleaned))
        (newPath (butlast path))
        (newErrorList (changeErrorList errorList (car lastPos) newPath))
        (newPossiblePositions (Matrix:addElementList possiblePositions (cadr lastPos) (car lastPos))))

        (backtrackingTryFillNumber newMatrix matRegions possibleNumbers newPossiblePositions newRegion possibilities newPath newErrorList wrongPaths)
    )
)

;; Continuação do backtracking: após o preenchimento dar certo, eu tento um próximo.
;; Caso falhe, eu tento de novo para o mesmo número
(defun continueBackTrackingTryFillNumber (mat matRegions numbers possiblePositions region possibilities path errorList wrongPaths lastPos)
    (let* (
        (fillResult (fillWholeRegion mat matRegions (cdr numbers) possiblePositions region possibilities path errorList wrongPaths))
        (succeeded (car fillResult))
        (newMat (cadr fillResult))
        (newRegion (caddr fillResult))
        (newPath (cadddr fillResult)))

        (if succeeded
            (list t newMat newRegion newPath)
            (tryAgainSameNumber mat matRegions numbers possiblePositions region possibilities path errorList wrongPaths lastPos)
        )  
    )
)

;; Inicio do backtracking interno à região
(defun backtrackingTryFillNumber (mat matRegions numbers possiblePositions region possibilities path errorList wrongPaths)
    (let* (
        (head (car numbers))
        (tail (cdr numbers))
        (newPossiblePositions (removeErrorPositions path errorList possiblePositions))
        (tryFillResult (tryFillNumber mat matRegions head newPossiblePositions region))
        (succeeded (car tryFillResult))
        (pos (cadr tryFillResult)))

        (if succeeded 
            (let* (
                (fillResult (fillNumber mat head pos region))
                (newMat (car fillResult))
                (newRegion (cadr fillResult))
                (currentPath (append path (list (position (list head pos) possibilities))))
                (checkResult (checkWrongPaths newMat matRegions currentPath wrongPaths pos newPossiblePositions possibilities head newRegion))
                (returnMat (car checkResult))
                (returnReg (cadr checkResult))
                (newPath (caddr checkResult))
                (newPosition (cadddr checkResult))
                (lastPos (list newPosition (position newPosition possiblePositions)))
                (possiblePosNext (removeItemsFromList possiblePositions (list newPosition))))

                (continueBackTrackingTryFillNumber returnMat matRegions numbers possiblePosNext returnReg possibilities newPath errorList wrongPaths lastPos)
            )

            (list nil mat region path)
        )    
    )
)


;; Enquanto existir números para preencher, vai para o backtracking.
;; Se não há, verifica se todas as posições foram preenchidas ou se o caminho final é um que já falhou.
(defun fillWholeRegion (mat matRegions possibleNumbers possiblePositions region possibilities path errorList wrongPaths)
    (cond 
        ((and (= (length possibleNumbers) 0) (not (= (PosUtils:getSecond (car region)) 0)))
        (list nil mat region path))

        ((and (= (length possibleNumbers) 0) (= (PosUtils:getSecond (car region)) 0))
        (list (not (member path wrongPaths)) mat region path))

        (t
        (backtrackingTryFillNumber mat matRegions possibleNumbers possiblePositions region possibilities path errorList wrongPaths))
    )
)


(defun solveByRegion (mat matRegions regions regionsPaths wrongPaths possibleNumbers possiblePositions)
    (let* (
        (region (car regions))
        (possibilities (getPossibilitiesList possibleNumbers possiblePositions))
        (errorList (replicate (length possibleNumbers) (list '() '())))
        (fillResult (fillWholeRegion mat matRegions possibleNumbers possiblePositions region possibilities '() errorList wrongPaths))
        (succeeded (car fillResult))
        (newMat (cadr fillResult))
        (newReg (caddr fillResult))
        (newPath (cadddr fillResult))
        (newRegPaths (append regionsPaths (list newPath))))

        (if succeeded 
            (list t newMat newRegPaths)
            (list nil mat regionsPaths)
        )
    )
)