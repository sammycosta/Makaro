(defpackage :ReadPuzzle
    (:use :common-lisp)
    (:export :getListFromStr
             :createMatrix
             :listToInt
             :createIntMatrix
             :isLetter
             :regionNumber
             :findRegions
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

;; Cria uma matriz (lista de listas) de Strings a partir de uma lista de strings
(defun createMatrix (list)
    (if (null list)
        ()
        (cons (getListFromStr (car list)) (createMatrix (cdr list)))
    )
)

;; Cria uma lista de Integers a partir de uma lista de Strings
(defun listToInt (list)
    (if (null list)
        ()
        (cons (parse-integer (car list)) (listToInt (cdr list)))
    )
)

;; Cria uma matriz (lista de listas) de Integers a partir de uma lista de strings
(defun createIntMatrix (list)
    (if (null list)
        ()
        (cons (listToInt (getListFromStr (car list))) (createIntMatrix (cdr list)))
    )
)

;; --------

;; Retorna True se for uma das letras utilizadas no mapeamento do jogo
(defun isLetter (str)
    (or (string= str "R") (string= str "L") (string= str "D") (string= str "U") (string= str "X"))
)

;; Retorna o número da região (recebida como String)
(defun regionNumber (str)
    (- (parse-integer str) 1)
)

;; Cria uma nova região caso ela ainda não exista, senão retorna a matriz de regiões inalterada
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

;; Aumenta o tamanho da região (o primeiro elemento da primeira tupla armazenada na linha correspondente à região)
(defun increaseSize (c regions)
    (let ((pos nil) (element nil))
                (setq pos (Matrix:makePosition
                        (regionNumber c)
                        0
                ))
                (setq element 
                    (PosUtils:increaseFirst (Matrix:getElement regions pos))   
                )
             (Matrix:changeElement regions pos element)
    )
)

;; Aumenta o a quantidade de elementos não preenchidos na região
;; (o segundo elemento da primeira tupla armazenada na linha correspondente à região)
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

;; Adiciona uma posição a uma região e retorna a matrix de regiões
(defun addToRegion (c regions pos)
    (let ((new_pos nil))
                (setq new_pos (Matrix:makePosition
                        (regionNumber c)
                        (Matrix:getColumnsNumber regions (regionNumber c))
                ))
             (Matrix:addElement regions new_pos pos)
    )
)

;; Recebe um caractere (da matriz de regiões) e chama as funções acima para adicionar a posição 
;; à região respectiva na matriz de regiões
(defun treatCharacter (c mat_cert regions pos)
    (if (not (isLetter c))
        (addToRegion c 
            (increaseUnfilled c mat_cert (increaseSize c (newRegion c regions)) pos)
            pos
        )
        regions
    )
)

;; Percorre a matriz de regiões e passa os caracteres para a função acima
(defun findRegions (mat_reg mat_cert regions pos)
    (if (and (= (+ 1 (Matrix:positionCol pos)) (Matrix:getRowsNumber mat_reg))
            (= (+ 1 (Matrix:positionRow pos)) (Matrix:getRowsNumber mat_reg)))
        (treatCharacter (Matrix:getElement mat_reg pos) 
            mat_cert regions pos
        )
        (if (and (= (+ 1 (Matrix:positionCol pos)) (Matrix:getRowsNumber mat_reg))
                (/= (+ 1 (Matrix:positionRow pos)) (Matrix:getRowsNumber mat_reg)))
            (let ((new_pos nil))
            
                (setq new_pos (Matrix:makePosition
                        (+ 1 (Matrix:positionRow pos))
                        0)
                )
                (findRegions mat_reg mat_cert 
                    (treatCharacter (Matrix:getElement mat_reg pos) 
                        mat_cert regions pos) new_pos)
            )
            (let ((new_pos nil))
                (setq new_pos (Matrix:makePosition
                        (Matrix:positionRow pos)
                        (+ 1 (Matrix:positionCol pos)))
                )
                    
                (findRegions mat_reg mat_cert 
                    (treatCharacter (Matrix:getElement mat_reg pos) 
                        mat_cert regions pos) new_pos)
            )
        )
    )
)
