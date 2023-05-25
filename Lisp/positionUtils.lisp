(defpackage :PosUtils
    (:use :common-lisp)
    (:export :getFirst
             :getSecond
             :increaseFirst
             :increaseSecond
             :decreaseFirst
             :decreaseSecond
                ))

(in-package :PosUtils)

(load "./matrix.lisp")

(defun getFirst (position)
    (Matrix:positionRow position)
)

(defun getSecond (position)
    (Matrix:positionCol position)
)

(defun increaseFirst (position)
    (let ((pos nil))
                (setq pos (Matrix:makePosition
                        (+ 1 (Matrix:positionRow position))
                        (Matrix:positionCol position))
                )
                pos
    )
)

(defun decreaseFirst (position)
    (let ((pos nil))
                (setq pos (Matrix:makePosition
                        (- (Matrix:positionRow position) 1)
                        (Matrix:positionCol position))
                )
                pos
    )
)

(defun increaseSecond (position)
    (let ((pos nil))
                (setq pos (Matrix:makePosition
                        (Matrix:positionRow position)
                        (+ 1 (Matrix:positionCol position)))
                )
                pos
    )
)

(defun decreaseSecond (position)
    (let ((pos nil))
                (setq pos (Matrix:makePosition
                        (Matrix:positionRow position)
                        (- (Matrix:positionCol position) 1))
                )
                pos
    )
)
