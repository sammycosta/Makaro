(defpackage :ReadPuzzle
    (:use :common-lisp)
    (:export :getListFromStr
             :createMatrix
             :listToInt
             :createIntMatrix
             :isLetter
             :regionNumber))

(in-package :ReadPuzzle)


(defun getListFromStr (string)
    (split-sequence:split-sequence #\SPACE string)
)

(defun createMatrix (list)
    (if (null list)
        ()
        (cons (getListFromStr (car list)) (createMatrix (cdr list)))
    )
)

(defun listToInt (list)
    (if (null list)
        ()
        (cons (parse-integer (car list)) (listToInt (cdr list)))
    )
)

(defun createIntMatrix (list)
    (if (null list)
        ()
        (cons (listToInt (getListFromStr (car list))) (createIntMatrix (cdr list)))
    )
)

;; --------
(defun isLetter (str)
    (or (string= str "R") (string= str "L") (string= str "D") (string= str "U") (string= str "X"))
)

(defun regionNumber (str)
    (- (parse-integer str) 1)
)
