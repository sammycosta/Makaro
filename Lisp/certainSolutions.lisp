(defpackage :CertainSolutions
    (:use :common-lisp)
    (:export :fillMissingOne
            :missingNumbers
            :iterateList
            ))

(in-package :CertainSolutions)

(require "Matrix" "./matrix.lisp")
(require "PosUtils" "./positionUtils.lisp")
(require "Validations" "./validations.lisp")



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