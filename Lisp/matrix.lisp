(defpackage :Matrix
    (:use :common-lisp)
    (:export :getRow
             :getRowsNumber
             :getColumnsNumber
             :getElement
             :changeElement
             :makePosition
             :addElement
             :printMatrix
             :positionRow
             :positionCol
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

(defun positionRow (pos)
  (myposition-row pos))

(defun positionCol (pos)
  (myposition-col pos))

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
        0
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

(defun getElement (matrix pos)
    (nth (myposition-col pos) (nth (myposition-row pos) matrix))
)

(defun changeElementList (list i value)
    (if (null list)
        (error "Empty")
        (if (= i 0)
            (cons value (cdr list))
            (cons (car list) (changeElementList (cdr list) (- i 1) value))
        )
    )
)

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

(defun addElementList (list i value)
    (if (null list)
        (cons value ())
        (if (= i 0)
            (cons value list)
            (cons (car list) (addElementList (cdr list) (- i 1) value))
        )
    )
)

(defun addElement (matrix pos value)
    (if (null matrix)
        (cons (cons value ()) () )
        (cond 
            ((or (< (myposition-row pos) 0) 
                 (< (myposition-col pos) 0) 
                 (> (myposition-row pos) (list-length matrix))
                ;;  (and (< (myposition-row pos) (list-length matrix)) 
                ;;     (> (myposition-col pos) (list-length (nth (myposition-col pos) matrix))))
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

(defun printMatrix (matrix)
  (dolist (row matrix)
    (format t "~{~a~^ ~}~%" row)))
