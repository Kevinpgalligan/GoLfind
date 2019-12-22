(in-package mona-lisa-gol)

(defsketch run-life
    ((life (random-life 50 100))
     (pixels-per-cell 6)
     (width (* 200 pixels-per-cell))
     (height (* 100 pixels-per-cell)))
  (setf life (life-next-state life))
  (background +white+)
  (dotimes (row 100)
    (dotimes (col 200)
      (with-pen (make-pen :fill (if (equalp DEAD (life-get-cell life row col))
                                    +white+
                                    +black+))
        (rect (* col pixels-per-cell)
              (* row pixels-per-cell)
              pixels-per-cell
              pixels-per-cell)))))

#|
(run-life (life-from-lists (list (list DEAD LIVE LIVE LIVE LIVE LIVE DEAD)
                                 (list LIVE DEAD DEAD DEAD DEAD DEAD LIVE)
                                 (list LIVE DEAD LIVE DEAD LIVE DEAD LIVE)
                                 (list LIVE DEAD DEAD DEAD DEAD DEAD LIVE)
                                 (list LIVE DEAD DEAD DEAD DEAD DEAD LIVE)
                                 (list DEAD LIVE LIVE LIVE LIVE LIVE DEAD)))
          50)
|#
