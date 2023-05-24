(defpackage :ReadPuzzle
    (:use :common-lisp)
    (:export :getListFromStr
             :createMatrix
             :listToInt
             :createIntMatrix
             :isLetter
             :regionNumber
             ))

(in-package :ReadPuzzle)

(require "Matrix" "./matrix.lisp")
(require "PosUtils" "./positionUtils.lisp")


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

(defun newRegion (c regions)
    (if (> (parse-integer c) (Matrix:getRowsNumber regions))
        (let ((pos nil) (element nil))
                (setq pos (Matrix:makePosition
                        (Matrix:getRowsNumber regions)
                        0)
                )
                (setq element (Matrix:makePosition 0 0)
                )
             (Matrix:addElement regions pos element)
        )
        regions
    )
)

(defun increaseSize (c regions)
    (let ((pos nil) (element nil))
                (setq pos (Matrix:makePosition
                        (parse-integer c)
                        0
                ))
                (setq element 
                    (PosUtils:increaseFirst (Matrix:getElement regions pos))   
                )
             (Matrix:changeElement regions pos element)
    )
)

(defun increaseUnfilled (c mat_cert regions pos)
     (if (= (Matrix:getElement mat_cert pos) 0 )
        (let ((new_pos nil) (element nil))
                (setq new_pos (Matrix:makePosition
                        (regionNumber c)
                        0)
                )
                (setq element 
                    (PosUtils:increaseSecond (Matrix:getElement regions new_pos))
                )
             (Matrix:changeElement regions new_pos element)
        )
        regions
    )
)

(defun addToRegion (c regions pos)
    (let ((new_pos nil))
                (setq new_pos (Matrix:makePosition
                        (parse-integer c)
                        (Matrix:getColumnsNumber regions (parse-integer c))
                ))
             (Matrix:addElement regions new_pos pos)
    )
)

(defun treatCharacter (c mat_cert regions pos)
     (if (not (isLetter c))
        (let ((new_pos nil) (element nil))
                (setq new_pos (Matrix:makePosition
                        (regionNumber c)
                        0)
                )
                (setq element 
                    (PosUtils:increaseSecond (Matrix:getElement regions new_pos))
                )
             (Matrix:changeElement regions new_pos element)
        )
        regions
    )
)
