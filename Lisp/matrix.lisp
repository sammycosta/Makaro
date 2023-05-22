(defpackage :Matrix
    (:use :common-lisp)
    (:export :getRow
             :getRowsNumber
             :getColumnsNumber
             :getElement
             :changeElement
             :makePosition
                ))

(in-package :Matrix)

(defstruct myposition
    row
    col
)

(defun makePosition (row col)
    (make-myposition
        :row row
        :col col
    )
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
            ((or (< (myposition-row pos) 0) (< (myposition-col pos) 0) (>= (myposition-row pos) (list-length matrix) (>= (myposition-col pos) (list-length (car matrix))))) 
               (error "Index out of bounds"))
            ((= (myposition-row pos) 0)
                (cons (changeElementList (nth (myposition-row pos) matrix) (myposition-col pos) value) (cdr matrix)))
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
