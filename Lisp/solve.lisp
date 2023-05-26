(defpackage :Solve
    (:use :common-lisp)
    (:export :solve))

(in-package :Solve)

(require "SolveRegion" "./solveRegion.lisp")


;; Cria uma lista de caminhos errados na região específica a partir da lista total de puzzleError
(defun makeWrongPathList (regionsPaths regionErrorList)
    (let* (
        (order (length regionsPaths))
        (currentError (nth order regionErrorList))
        (lenErrorList (length (cadr currentError)))
        (isCurrentError (and (> order 0) (> lenErrorList 0) (equal (car currentError) regionsPaths)))
        (isFirstRegionError (and (= order 0) (> lenErrorList 0))))

        (if (or currentError isFirstRegionError)
            (cadr currentError)
            '()
        )
    )
)

;; Coloca 0 em todas as posições dadas na matriz
(defun cleanRegion (mat positions)
    (if (null positions)
        mat
        (let ((newMat (Matrix:changeElement mat (car positions) 0)))
            (cleanRegion newMat (cdr positions)))
    )
)

;; Atualiza a lista de erros totais baseado em um caminho que causou uma falha em uma região específica
(defun changePuzzleErrorList (puzzleErrorList regionsPaths failedPath)
    (let* (
        (order (length regionsPaths))
        (puzzleError (nth order puzzleErrorList))
        (regionPathError (car puzzleError))
        (pathErrors (cadr puzzleError)))

        (if (> (length pathErrors 0))
            (if (equal regionPathError regionsPaths)
                (Matrix:changeElementList puzzleErrorList order (list regionsPaths (append pathErrors (list failedPath))))
                (Matrix:changeElementList puzzleErrorList order (list regionsPaths (list failedPath)))
            )
            (Matrix:changeElementList puzzleErrorList order (list regionsPaths (list failedPath)))
        ))
)

;; Falhou, chamar backtracking de novo para a mesma região com a matriz e caminhos limpos 
;; E a puzzleErrorList atualizada com a falha.
(defun tryAgainSameRegion (mat matRegions regions regionsPaths puzzleErrorList possiblePositions)
    (let* (
        (failedPath (last regionsPaths))
        (newRegionsPath (butlast regionsPaths))
        (newPuzzleErrorList (changePuzzleErrorList puzzleErrorList newRegionsPath failedPath))
        (newMat (cleanRegion mat possiblePositions)))

        (if (> (length failedPath) 0)
            (backtracking newMat matRegions regions newRegionsPath newPuzzleErrorList)
            (list nil mat newPuzzleErrorList)
        )
    )
)

;; Continuação do backtracking: após o preenchimento dar certo, eu tento um próximo.
;; Caso falhe, eu tento de novo para a mesma região
(defun continueBacktracking (mat matRegions regions regionsPaths puzzleErrorList possiblePositions)
    (print "continuou")
    (let* (
        (newRegions (cdr regions))
        ;; Atualizar pra usar certezas dps
        (backtrackingResult (backtracking mat matRegions newRegions regionsPaths puzzleErrorList))
        (succeeded (car backtrackingResult))
        (newMat (cadr backtrackingResult))
        (newPuzzleErrorList (caddr backtrackingResult)))

        (if succeeded
            (list t newMat newPuzzleErrorList)
            (tryAgainSameRegion newMat matRegions regions regionsPaths puzzleErrorList possiblePositions)
        )
    )
)

;; Inicio do backtracking de regiões
(defun backtracking (mat matRegions regions regionsPaths puzzleErrorList)
    (let* (
        (wrongPaths (makeWrongPathList regionsPaths puzzleErrorList))
        (missingNumbersResult (CertainSolutions:missingNumbers (car regions) mat))
        (possibleNumbers (car missingNumbersResult))
        (possiblePositions (cadr missingNumbersResult))
        (solveRegionResult (SolveRegion:solveByRegion mat matRegions regions regionsPaths wrongPaths possibleNumbers possiblePositions))
        (succeeded (car solveRegionResult))
        (newMat (cadr solveRegionResult))
        (newRegPaths (caddr solveRegionResult)))

        (cond 
            ((equal (length regions) 0)
            (list t mat puzzleErrorList))

            (succeeded
            (continueBacktracking newMat matRegions regions newRegPaths puzzleErrorList possiblePositions))

            (t
            (list nil mat puzzleErrorList))   
        )
    )
)

(defun solve (mat matRegions regions)
    (let* (
        (puzzleErrorList (SolveRegion:replicate (length regions) (list '() '())))
        (backtrackingResult (backtracking mat matRegions regions '() puzzleErrorList))
        (succeeded (car backtrackingResult))
        (newMat (cadr backtrackingResult)))
        (list succeeded newMat)
    )
)