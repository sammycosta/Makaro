(defpackage :Matrix
    (:use :common-lisp)
    (:export :getRow
             :getRowsNumber
             :getColumnsNumber
             :getElement
             :changeElementList
             :changeElement
             :makePosition
             :addElementList
             :addElement
             :printMatrix
             :positionRow
             :positionCol
                ))

(in-package :Matrix)

;; Struct que indica uma posição
(defstruct myposition
    row
    col
)

;; Função que chama a criação de uma struct myposition
(defun makePosition (row col)
    (make-myposition
        :row row
        :col col
    )
)

;; Função que acessa o elemento row da struct myposition e o retorna
(defun positionRow (pos)
  (myposition-row pos))

;; Função que acessa o elemento col da struct myposition e o retorna
(defun positionCol (pos)
  (myposition-col pos))

;; Retorna uma linha da matriz (uma lista)
(defun getRow (matrix row)
    (if (null matrix)
        (error "Empty")
        (if (or (< row 0) (>= row (list-length matrix))) 
            (error "Index out of bounds")
            (nth row matrix)
        )
    )
)

;; Retorna o número de linhas da matriz
(defun getRowsNumber (matrix)
    (if (null matrix)
        0
        (list-length matrix)
    )
)

;; Retorna o número de colunas de uma linha específica da matriz
(defun getColumnsNumber (matrix row)
    (if (null matrix)
        0
        (if (or (< row 0) (>= row (list-length matrix))) 
            (error "Index out of bounds")
            (list-length (nth row matrix)) 
        )
    )
)

;; Retorna o elemento em uma posição (myposition) da matriz
(defun getElement (matrix pos)
    (nth (myposition-col pos) (nth (myposition-row pos) matrix))
)

;; Função auxiliar que muda o elemento em uma posição da lista
(defun changeElementList (list i value)
    (if (null list)
        (error "Empty")
        (if (= i 0)
            (cons value (cdr list))
            (cons (car list) (changeElementList (cdr list) (- i 1) value))
        )
    )
)

;; Muda o elemento em uma posição da matriz (lista de listas) e retorna a nova matriz
(defun changeElement (matrix pos value)
    (if (null matrix)
        (error "Empty")
        (cond 
            ((or (< (myposition-row pos) 0) 
                 (< (myposition-col pos) 0) 
                 (>= (myposition-row pos) (list-length matrix))
                 (>= (myposition-col pos) (list-length (car matrix)))) 
               (error "Index out of bounds"))
            ((= (myposition-row pos) 0)
                (cons (changeElementList (nth (myposition-row pos) matrix) (myposition-col pos) value) 
                    (cdr matrix)))
            (t 
                (let ((new_pos nil))
                (setq new_pos (make-myposition
                        :row (- (myposition-row pos) 1)
                        :col (myposition-col pos)
                    ))
                (cons (car matrix) (changeElement (cdr matrix) new_pos value)))) 
        )
    )
)

;; Função auxiliar que adiciona um elemento em uma posição da lista
(defun addElementList (list i value)
    (if (null list)
        (cons value ())
        (if (= i 0)
            (cons value list)
            (cons (car list) (addElementList (cdr list) (- i 1) value))
        )
    )
)

;; Adiciona um elemento em uma posição da matriz (lista de listas) e retorna a nova matriz
(defun addElement (matrix pos value)
    (if (null matrix)
        (cons (cons value ()) () )
        (cond 
            ((or (< (myposition-row pos) 0) 
                 (< (myposition-col pos) 0) 
                 (> (myposition-row pos) (list-length matrix))
                ) 
               (error "Index out of bounds"))
            ((= (myposition-row pos) (list-length matrix))
                (concatenate 'list matrix (cons (addElementList () (myposition-col pos) value) () ))
            )
            ((= (myposition-row pos) 0)
                (cons (addElementList (nth (myposition-row pos) matrix) (myposition-col pos) value) 
                    (cdr matrix))
            )
            (t 
                (let ((new_pos nil))
                (setq new_pos (make-myposition
                        :row (- (myposition-row pos) 1)
                        :col (myposition-col pos)
                    ))
                (cons (car matrix) (addElement (cdr matrix) new_pos value)))
            ) 
        )
    )
)

;; Função que printa a matriz (lista de listas) na tela
(defun printMatrix (matrix)
  (dolist (row matrix)
    (format t "~{~a~^ ~}~%" row)))
