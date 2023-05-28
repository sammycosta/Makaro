(defpackage :CertainSolutions
    (:use :common-lisp)
    (:export :missingNumbers
            :certainties))

(in-package :CertainSolutions)

;; Função auxiliar de missingNumbers que itera sobre a lista de números possíveis e sobre a lista de regiões,
;; retornando os números ainda não utilizados em uma região e as posições não preenchidas
(defun iterateList (possibilities empty_pos rec_list mat)
    (if (> (length rec_list) 0)
        (let ((num (Matrix:getElement mat (car rec_list))))
            (if (/= num 0)
                (iterateList (set-difference possibilities (list num)) empty_pos (cdr rec_list) mat)
                (iterateList possibilities (append empty_pos (list (car rec_list))) (cdr rec_list) mat)
            )
        )
        (list possibilities empty_pos)
    )
)

;; Retorna os números que ainda não foram utilizados em uma região e as posições não preenchidas
(defun missingNumbers (region_list mat)
    (iterateList (loop for i from 1 to (PosUtils:getFirst (car region_list)) collect i)
        '() (cdr region_list) mat)
)

;; Função auxiliar de fillMissingOne que realiza a verificação do número de posições não preenchidas na região 
;; e realiza seu preenchimento caso haja apenas uma
(defun fillMat1 (i missing_list mat regions)
    (let ((aux_pos (Matrix:makePosition i 0)))
        (if (= (PosUtils:getSecond (Matrix:getElement regions aux_pos)) 1)
            (list (Matrix:changeElement mat (car (cadr missing_list)) (car (car missing_list))) 
                    (Matrix:changeElement regions aux_pos (PosUtils:decreaseSecond (Matrix:getElement regions aux_pos)))
            )
            (list mat regions)
        )
    )
)

;; Chama a função fillMat1 para todas as regiões, de forma a preencher todasas regiões com apenas uma posição 
;; não preenchida (só há uma opção para o número, nesse caso)
(defun fillMissingOne (i mat_n_reg)
    (let* ( (mat (car mat_n_reg))
            (regions (cadr mat_n_reg))
            (missing_num (fillMat1 i (missingNumbers (Matrix:getRow regions i) mat) mat regions))
          )
        (if (< (+ i 1) (Matrix:getRowsNumber regions))
            (fillMissingOne (+ i 1) missing_num)
            missing_num
        )
    )
)

;; Função chamada por verifications que recebe as posições e os valores e realiza a atualização na matriz
(defun  changeMatrices (p1 m1 p2 m2 i missing_list mat regions)
    (let ((missing (car missing_list)) 
          (positions (cadr missing_list))
          (aux_pos (Matrix:makePosition i 0))
         )
         (list 
            (Matrix:changeElement (Matrix:changeElement mat (nth p1 positions) (nth m1 missing)) (nth p2 positions) (nth m2 missing))
            (Matrix:changeElement regions aux_pos 
                (PosUtils:decreaseSecond (PosUtils:decreaseSecond (Matrix:getElement regions aux_pos))))
         )
    )
)

;; Função auxiliar de fillMat2 que verifica se é possível determinar qual posição deve receber qual valor, 
;; e caso isso seja possível, chama changeMatrices para fazer as atualizações dos valores
(defun verifications (i missing_list mat regions)
    (let ((missing (car missing_list)) 
          (positions (cadr missing_list))
         )
         (cond 
            ((or (Validations:isAdjacent mat (car missing) (car positions))
                (Validations:isAdjacent mat (cadr missing) (cadr positions)))
                (changeMatrices 0 1 1 0 i missing_list mat regions)
            )
            ((or (Validations:isAdjacent mat (cadr missing) (car positions)) 
                (Validations:isAdjacent mat (car missing) (cadr positions)))
                (changeMatrices 1 1 0 0 i missing_list mat regions)
            )
            (t
                (list mat regions)
            )
         )
    )
)

;; Verifica a região passada e chama verifications para tentar preencher as posições vazias caso haja apenas duas
(defun fillMat2 (i mat regions)
    (let ((aux_pos (Matrix:makePosition i 0)))
        (if (= (PosUtils:getSecond (Matrix:getElement regions aux_pos)) 2)
            (verifications i (missingNumbers (Matrix:getRow regions i) mat) mat regions)
            (list mat regions)
        )
    )
)

;; Chama a função fillMat2 para todas as regiões, de forma a preencher as regiões com apenas duas posições vazias 
;; nas quais é possível determinar com certeza os valores de cada uma por causa de seus vizinhos ortogonais
(defun fillMissingTwo (i mat_n_reg)
    (if (< (+ i 1) (Matrix:getRowsNumber (cadr mat_n_reg)))
        (fillMissingTwo (+ i 1) (fillMat2 i (car mat_n_reg) (cadr mat_n_reg)))
        (fillMat2 i (car mat_n_reg) (cadr mat_n_reg))
    )
)

;; Chama fillMissingTwo e fillMissingOne e retorna o resultado 
(defun certainties (mat_n_reg)
    (let ((regions (cadr mat_n_reg)))
        (if (null regions)
            mat_n_reg
            (fillMissingTwo 0 (fillMissingOne 0 mat_n_reg))
        )
    )
)