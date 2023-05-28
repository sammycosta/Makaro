(require "PosUtils" "./positionUtils.lisp")
(require "ReadPuzzle" "./readpuzzle.lisp")
(require "Validations" "./validations.lisp")
(require "CertainSolutions" "./certainSolutions.lisp")
(require "Solve" "./solve.lisp")

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
        (auxResult '())
        (finalResult '()))

    (setq contents_list (readfile "../puzzle_17.txt"))
    (setq size (parse-integer (car contents_list)))

    ;; Parsing das estruturas necessárias
    (setq regionsMatrix (ReadPuzzle:createMatrix (subseq contents_list 1 (+ 1 size))))
    (setq certaintyMatrix (ReadPuzzle:createIntMatrix (subseq contents_list (+ 1 size))))
    (setq regions (ReadPuzzle:findRegions regionsMatrix certaintyMatrix '() initial_pos))

    ;; Preenche certezas
    (setq auxResult (CertainSolutions:certainties (list certaintyMatrix regions)))
    (setq certaintyMatrix (car auxResult))
    (setq regions (cadr auxResult))

    ;; Roda backtracking
    (setq finalResult (Solve:solve certaintyMatrix regionsMatrix regions))
    (print (car finalResult))
    (format t "~%")
    (Matrix:printMatrix (cadr finalResult))
  )
)

(main)