(load "game-of-life.lisp")

(defun print-life (life)
    (loop for row from 0 to (1- (life-rows life)) do
        (loop for col from 0 to (1- (life-cols life)) do
            (princ (if (equalp LIVE (life-get-cell life row col)) "*" " ")))
        (terpri)))

(print-life (life-from-lists (list (list LIVE LIVE) (list DEAD LIVE))))
