;; (require 'uiop)

;; (defun lerArquivo (nome_arquivo)
;;   (with-open-file (stream nome_arquivo :direction :input)
;;     (let ((conteudo (read stream)))
;;       (format t "~a~%" conteudo)
;;       conteudo)))

;; (defun readfilee (file_name)
;;   (with-open-file (instream file_name :direction :input)
;;     (when instream 
;;       (let ((string (make-string (file-length instream))))
;;         (read-sequence string instream)
;;         string))))

(defvar size 0)
(defvar contents_list '())

(defun getListFromStr (string)
    (if (null string)
        ()
        (if (not (string= (subseq string 0 1) " "))
            (cons (subseq string 0 1) (getListFromStr (subseq string 1)))
            (getListFromStr (subseq string 1))
        )
    )
)

(defun readFile (file_name)
  (with-open-file (stream file_name :direction :input)
    (loop for linha = (read-line stream nil)
          while linha
          collect linha)))

(defun main()
    ;; (let ((content_list (readfile "../puzzle_01.txt")))
    ;;    ((size (car content)))
    ;;    (print size))
    (setq contents_list (readfile "../puzzle_01.txt"))
    (setq size (parse-integer (car contents_list)))
    (print size)
    (print (subseq contents_list 1 (+ 1 size)))
    (print (subseq (nth 1 contents_list) 0 1))
    ;; (print (getListFromStr (subseq contents_list 1 2)))
    
)
(main)