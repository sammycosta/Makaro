(defpackage :ReadPuzzle
    (:use :common-lisp)
    (:export :getListFromStr
             :createMatrix
             :listToInt
             :createIntMatrix
             :isLetter
             :regionNumber))

(in-package :ReadPuzzle)

(defun split-string (string &optional (delimiter #\Space))
  "Divide uma string em uma lista de substrings com base no separador especificado."
  (if (string= string "")
      '()
      (let ((start 0)
            (end (length string))
            (result '()))
        (loop while (< start end)
              do (let ((next-delimiter (position delimiter string :start start)))
                   (push (subseq string start next-delimiter) result)
                   (if next-delimiter
                       (setq start (1+ next-delimiter))
                       (setq start end))))
        (nreverse result))))

(defun getListFromStr (string)
  "Retorna uma lista de palavras a partir de uma string."
  (split-string string #\Space))

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
