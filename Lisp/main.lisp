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

(defun readFile (file_name)
  (with-open-file (stream file_name :direction :input)
    (loop for linha = (read-line stream nil)
          while linha
          collect linha)))



(defun main()
    (let ((content_list (readfile "../puzzle_01.txt")))
       ((size (car content)))
       (print size))
    
)
(main)