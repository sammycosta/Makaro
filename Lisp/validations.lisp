(defpackage :Validations
    (:use :common-lisp)
    (:export :getAimed
             :getAroundList
             :validByArrow
             :verifyBiggerAroundArrow
             
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
(defun getAroundList (mat arrowPosition aimedPosition)
    (let ((n (Matrix:getRowsNumber mat))
        (offsets '((1 0) (-1 0) (0 1) (0 -1))))
        (mapcan #'(lambda (offset)
                    (let* ((arrowFirst (PosUtils:getFirst arrowPosition))
                        (arrowSecond (PosUtils:getSecond arrowPosition))
                        (offsetFirst (car offset))
                        (offsetSecond (cadr offset))
                        (print offsetFirst)
                        (position (Matrix:makePosition
                                    (+ arrowFirst offsetFirst)
                                    (+ arrowSecond offsetSecond))))
                    (when (and (<= 0 (PosUtils:getFirst position) (- n 1))
                                (<= 0 (PosUtils:getSecond position) (- n 1))
                                (not (equalp position aimedPosition)))
                        (list (Matrix:getElement mat position)))))
                offsets)
    )
)   

;; Verificar qual é o maior número da lista. Começar passando o primeiro elemento.
(defun getBigger (num list)
    (if (null list)
        number
        (if (> number (car list))
            (getBigger number tail)
            (getBigger (car list tail))
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
