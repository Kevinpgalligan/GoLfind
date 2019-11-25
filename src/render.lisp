(load "game-of-life.lisp")

(defun print-life (life)
    (loop for row from 0 to (1- (life-rows life)) do
        (loop for col from 0 to (1- (life-cols life)) do
            (princ (if (equalp LIVE (life-get-cell life row col)) "X" " ")))
        (terpri)))

(defun run-life (life transitions)
    (if (= 0 transitions)
        life
        (progn
            (princ "----")
            (terpri)
            (print-life life)
            (run-life (life-next-state life) (1- transitions)))))
    
(run-life (life-from-lists (list (list DEAD LIVE LIVE LIVE LIVE LIVE DEAD)
                                 (list LIVE DEAD DEAD DEAD DEAD DEAD LIVE)
                                 (list LIVE DEAD LIVE DEAD LIVE DEAD LIVE)
                                 (list LIVE DEAD DEAD DEAD DEAD DEAD LIVE)
                                 (list LIVE DEAD DEAD DEAD DEAD DEAD LIVE)
                                 (list DEAD LIVE LIVE LIVE LIVE LIVE DEAD)))
          50)
