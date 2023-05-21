(defpackage :Matrix
    (:use :common-lisp)
    (:export :getRow
             :getRowsNumber
             :getColumnsNumber
             :getElement
             :changeElement
                ))

(in-package :Matrix)

(defstruct Position
    row
    col
)

(defun getRow (matrix row)
    (if (null matrix)
        (error "Empty")
        (if (or (< row 0) (>= row (list-length matrix))) 
            (error "Index out of bounds")
            (nth row matrix)
        )
    )
)

(defun getRowsNumber (matrix)
    (if (null matrix)
        (error "Empty")
        (list-length matrix)
    )
)

(defun getColumnsNumber (matrix row)
    (if (null matrix)
        0
        (if (or (< row 0) (>= row (list-length matrix))) 
            (error "Index out of bounds")
            (list-length (nth row matrix)) 
        )
    )
)

(defun getElement (matrix row col)
    (nth col (nth row matrix))
)

(defun changeElementList (list i value)
    (if (null list)
        (error "Empty")
        (if (= i 0)
            (cons value (cdr list))
            (cons value (changeElementList (cdr list) (- i 1) value))
        )
    )
)

(defun changeElement (matrix pos value)
    (if (null matrix)
        (error "Empty")
        (cond 
            ((or (< (position-row pos) 0) (< (position-col pos) 0) (>= (position-row pos) (list-length matrix) (>= (position-col pos) (list-length (car matrix))))) 
               (error "Index out of bounds"))
            ((= (position-row pos) 0)
                (cons (changeElementList (nth (position-row pos) matrix) (position-col pos) value)))
            (t 
                (let new_pos
                    (make-position
                        :row (- (position-row pos) 1)
                        :col (position-row col)
                    )
                (cons (car matrix) (changeElement (cdr matrix) new_pos value)))) 
        )
    )
)
