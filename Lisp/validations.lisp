(defpackage :Validations
    (:use :common-lisp)
    (:export :getAimed
             :getAroundList
             :validByArrow
             :verifyBiggerAroundArrow
             :getAroundArrowsList
             :isValid
             ))

(in-package :Validations)

(require "Matrix" "./matrix.lisp")
(require "PosUtils" "./positionUtils.lisp")
(require "ReadPuzzle" "./readpuzzle.lisp")

;; Dada uma posição de seta, retorna a posição que ela aponta.
(defun getAimed (matRegions arrowPosition)
    (cond
        ((string= (Matrix:getElement matRegions arrowPosition) "L")
                (PosUtils:decreaseSecond arrowPosition)
        )
        ((string= (Matrix:getElement matRegions arrowPosition) "R")
                (PosUtils:increaseSecond arrowPosition)
        )
        ((string= (Matrix:getElement matRegions arrowPosition) "U")
                (PosUtils:decreaseFirst arrowPosition)
        )
        ((string= (Matrix:getElement matRegions arrowPosition) "D")
                (PosUtils:increaseFirst arrowPosition)
        )
        (t
                (error "Posição de seta não contém seta")
        )
    )
)

;; Dada uma posição de seta, retorna a lista de números ao redor que não é o apontado
;; (defun getAroundList (mat arrowPosition aimedPosition)
;;     (let ((n (Matrix:getRowsNumber mat))
;;         (offsets '((1 0) (-1 0) (0 1) (0 -1))))
;;         (mapcan #'(lambda (offset)
;;                     (let* ((arrowFirst (PosUtils:getFirst arrowPosition))
;;                         (arrowSecond (PosUtils:getSecond arrowPosition))
;;                         (offsetFirst (car offset))
;;                         (offsetSecond (cadr offset))
;;                         (print offsetFirst)
;;                         (position (Matrix:makePosition
;;                                     (+ arrowFirst offsetFirst)
;;                                     (+ arrowSecond offsetSecond))))
;;                     (when (and (<= 0 (PosUtils:getFirst position) (- n 1))
;;                                 (<= 0 (PosUtils:getSecond position) (- n 1))
;;                                 (not (equalp position aimedPosition)))
;;                         (list (Matrix:getElement mat position)))))
;;                 offsets)
;;     )
;; )   

;; Dada uma posição de seta, retorna a lista de números ao redor que não é o apontado
(defun getAroundList (mat arrowPosition aimedPosition)
    (let ((n (Matrix:getRowsNumber mat)))
        (append
            (when (and (< (PosUtils:getFirst arrowPosition) (- n 1)) 
                    (not (equalp (PosUtils:increaseFirst arrowPosition) aimedPosition)))
                (list (Matrix:getElement mat (PosUtils:increaseFirst arrowPosition))))
            (when (and (> (PosUtils:getFirst arrowPosition) 0) 
                    (not (equalp (PosUtils:decreaseFirst arrowPosition) aimedPosition)))
                (list (Matrix:getElement mat (PosUtils:decreaseFirst arrowPosition))))
            (when (and (< (PosUtils:getSecond arrowPosition) (- n 1)) 
                    (not (equalp (PosUtils:increaseSecond arrowPosition) aimedPosition)))
                (list (Matrix:getElement mat (PosUtils:increaseSecond arrowPosition))))
            (when (and (> (PosUtils:getSecond arrowPosition) 0) 
                    (not (equalp (PosUtils:decreaseSecond arrowPosition) aimedPosition)))
                (list (Matrix:getElement mat (PosUtils:decreaseSecond arrowPosition))))
        )
    )
)

;; Verificar qual é o maior número da lista. Começar passando o primeiro elemento.
(defun getBigger (num list)
    (if (null list)
        num
        (if (> num (car list))
            (getBigger num (cdr list))
            (getBigger (car list) (cdr list))
        )
    )
)

;; Dada a lista de números ao redor da seta, retornar o maior.
(defun verifyBiggerAroundArrow (list)
    (if (null list)
        (error "Lista vazia")
        (getBigger (car list) (cdr list))
    )
)

;; Avalia se o número pode ser colocado na posição em função da seta
(defun validByArrow (matRegions mat arrowPosition numberPosition number)
    (let* ((aimedPosition (getAimed matRegions arrowPosition))
           (aroundList (getAroundList mat arrowPosition aimedPosition)) 
            )
        (cond 
            ((equalp aimedPosition numberPosition)
                (> number (verifyBiggerAroundArrow aroundList))
            )
            ((> (Matrix:getElement mat aimedPosition) 0)
                (< number (Matrix:getElement mat aimedPosition))
            )
            (t
                T
            )
        )
    )
)

;; Avalia se posição tem uma seta.
(defun isArrow (matRegions position) 
    (let ((c (Matrix:getElement matRegions position)))
        (or (string= c "L") (string= c "R") (string= c "U") (string= c "D") )
    )
)

;; Dada uma posição, retorna uma lista com as posições de setas ao redor dela.
(defun getAroundArrowsList (matRegions position)
    (let ((n (Matrix:getRowsNumber matRegions)))
        (append
            (when (and (< (PosUtils:getFirst position) (- n 1)) 
                    (isArrow matRegions (PosUtils:increaseFirst position)))
            (list (PosUtils:increaseFirst position)))
            (when (and (> (PosUtils:getFirst position) 0) 
                    (isArrow matRegions (PosUtils:decreaseFirst position)))
            (list (PosUtils:decreaseFirst position)))
            (when (and (< (PosUtils:getSecond position) (- n 1)) 
                    (isArrow matRegions (PosUtils:increaseSecond position)))
            (list (PosUtils:increaseSecond position)))
            (when (and (> (PosUtils:getSecond position) 0) 
                    (isArrow matRegions (PosUtils:decreaseSecond position)))
            (list (PosUtils:decreaseSecond position)))
        )
    )
)

;; chama a função validbyArrow para todas as setas ao redor da posição
(defun validByAroundArrows (mat matRegions number numberPosition list)
    (if (null list)
        T
        (if (validByArrow matRegions mat (car list) numberPosition number)
            (validByAroundArrows mat matRegions number numberPosition (cdr list))
            Nil
        )
    )
)

;; Dada um número e uma posição, retorna se é valido colocar ele nela em função das setas.
(defun validNumberPositionByArrows (mat matRegions number position)
    (validByAroundArrows mat matRegions number position 
        (getAroundArrowsList matRegions position))
)

;; Retorna True se houver um adjacente igual nessa opção, False se não.
(defun isAdjacent (mat number position) 
    (let ((n (Matrix:getRowsNumber mat))) 
        (or 
            (and (< (PosUtils:getFirst position) (- n 1)) 
                (= (Matrix:getElement mat (PosUtils:increaseFirst position)) number))
            (and (> (PosUtils:getFirst position) 0) 
                (= (Matrix:getElement mat (PosUtils:decreaseFirst position)) number))
            (and (< (PosUtils:getSecond position) (- n 1)) 
                (= (Matrix:getElement mat (PosUtils:increaseSecond position)) number))
            (and (> (PosUtils:getSecond position) 0) 
                (= (Matrix:getElement mat (PosUtils:decreaseSecond position)) number))
        )
    )
)

;; É valida para todas as regras.
(defun isValid (mat matRegions number position)
    (and
        (not (isAdjacent mat number position))
        (validNumberPositionByArrows mat matRegions number position) 
    )
)
