;; (require 'uiop)
; (load "~/quicklisp/setup.lisp")
; (ql:quickload "split-sequence")

(require "ReadPuzzle" "./readpuzzle.lisp")
(require "Matrix" "./matrix.lisp")
(require "PosUtils" "./positionUtils.lisp")
(require "Validations" "./validations.lisp")
(require "CertainSolutions" "./certainSolutions.lisp")
(require "SolveRegion" "./solveRegion.lisp")
(require "Solve" "./solve.lisp")
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
        (initial_pos (Matrix:makePosition 0 0))
        (finalResult '()))
    (setq contents_list (readfile "../puzzle_14.txt"))
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
    (let* ((c certaintyMatrix)
        (pos (Matrix:makePosition 6 7))
        (pos2 (Matrix:makePosition 0 4))
        (pos3 (Matrix:makePosition 6 7))
        (n 7)
        (a (loop for i from 1 to n collect i))
        (lista_teste (list (list 2 pos) (list 1 pos2) (list 7 pos)
        (list 3 pos2))))
        ;; (print (Matrix:changeElement c pos 88))
        ;; (print (Matrix:getElement c pos))
        ;; (Matrix:printMatrix (Matrix:addElement c pos 88))
        ;; (print (Validations:validByArrow regionsMatrix certaintyMatrix pos pos2 5))
        ;; (print (Validations:getAroundList certaintyMatrix pos (Validations:getAimed regionsMatrix pos)))
        ;; (print (Validations:isValid certaintyMatrix regionsMatrix 1 pos2))
        ;; (print a)
        (print (position (list 7 pos3) lista_teste :test #'equalp))
    )

    
    ;; (print (Matrix:addElement '() initial_pos 88))
    ;; (format t "~%")
    
    ;; ;; (print (Matrix:getElement regionsMatrix initial_pos))
    ;; (setq regions (ReadPuzzle:findRegions regionsMatrix certaintyMatrix '() initial_pos))
    ;; ;; (Matrix:printMatrix certaintyMatrix)

    

    (setq finalResult (Solve:solve certaintyMatrix regionsMatrix regions))
    (print "saiu")
    (format t "~%")
    (Matrix:printMatrix (cadr finalResult))
    

    ;; (Matrix:printMatrix (car (CertainSolutions:fillMissingOne 0 (list certaintyMatrix regions))))
    ;; (format t "~%")
    ;; (Matrix:printMatrix (car (CertainSolutions:fillMissingTwo 0 (CertainSolutions:fillMissingOne 0 (list certaintyMatrix regions)))))
    ;; (format t "~%")
    ;; (Matrix:printMatrix (car (CertainSolutions:certainties (list certaintyMatrix regions))))
    ;; (print regions)

 )
)

(main)