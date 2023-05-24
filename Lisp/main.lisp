;; (require 'uiop)
; (load "~/quicklisp/setup.lisp")
; (ql:quickload "split-sequence")

(require "ReadPuzzle" "./readpuzzle.lisp")
(require "Matrix" "./matrix.lisp")
(require "PosUtils" "./positionUtils.lisp")
;; (ql:quickload "alexandria")
;; (require 'alexandria)

(defun readFile (file_name)
  (with-open-file (stream file_name :direction :input)
    (loop for linha = (read-line stream nil)
          while linha
          collect linha)))

(defun main()
 (let ((size 0)
        (contents_list '())
        (regionsMatrix '())
        (certaintyMatrix '())
        (regions '())
        (initial_pos (Matrix:makePosition 0 0)))
    (setq contents_list (readfile "../puzzle_01.txt"))
    (setq size (parse-integer (car contents_list)))
    (print size)
    (print (subseq contents_list 1 (+ 1 size)))
    (print (subseq (nth 1 contents_list) 0 1))
    (print (ReadPuzzle:getListFromStr (nth 1 contents_list)))

    (setq regionsMatrix (ReadPuzzle:createMatrix (subseq contents_list 1 (+ 1 size))))
    (print regionsMatrix)

    (setq certaintyMatrix (ReadPuzzle:createIntMatrix (subseq contents_list (+ 1 size))))
    (print certaintyMatrix)

    ;; (print (ReadPuzzle:isLetter (nth 1 (nth 0 regionsMatrix))))

    ;; (print (Matrix:getRow certaintyMatrix 0))
    ;; (print (Matrix:getRowsNumber regionsMatrix))
    ;; (print (Matrix:getColumnsNumber regionsMatrix 1))
    ;; (print (Matrix:getElement certaintyMatrix 0 0))
    ;; (print Matrix )
    (let ((c certaintyMatrix)
        (pos (Matrix:makePosition 0 0)))
        ;; (print (Matrix:changeElement c pos 88))
        (print (Matrix:getElement c pos))
        (Matrix:printMatrix (Matrix:addElement c pos 88))
    )

    (print initial_pos)
    (setq regions (ReadPuzzle:findRegions regionsMatrix certaintyMatrix '(()) initial_pos))
    (print regions)

 )
)

(main)