(defun lerArquivo (nome_arquivo)
  (with-open-file (stream nome_arquivo :direction :input)
    (let ((conteudo (read stream)))
      (format t "~a~%" conteudo)
      conteudo)))



(defun main()
    ;; (set ((conteudo (lerArquivo "../puzzle_01.txt"))))
    ;; (print conteudo)
    (let ((in (open "../puzzle_01.txt")))
        (format t "~a~%" (read-line (setq conteudo (in)))
        (close in)))
    (print conteudo)
    ;; ?????????
)

(main)