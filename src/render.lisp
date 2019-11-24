(load "game-of-life.lisp")

(defun print-life (life)
    (loop for row from 0 to (1- (life-rows life)) do
        (loop for col from 0 to (1- (life-cols life)) do
            (princ (if (equalp LIVE (life-get-cell life row col)) "*" " ")))
        (terpri)))

(defun run-life (life transitions)
    (if (= 0 transitions)
        life
        (progn
            (princ "----")
            (terpri)
            (print-life life)
            (sleep 1)
            (run-life life (1- transitions)))))
    
(run-life (life-from-lists (list (list LIVE LIVE) (list DEAD LIVE)))
          10)
