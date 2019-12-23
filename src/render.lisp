(in-package mona-lisa-gol)

(defsketch run-life
    ((initial-life (random-life 50 100))
     ;; must override initial-life when passing life
     ;; to the sketch.
     ;; directly setting the default life to random
     ;; doesn't allow it to be overridden, for some
     ;; reason.
     (life initial-life)
     (pixels-per-cell 50)
     (width (* (life-cols life) pixels-per-cell))
     (height (* (life-rows life) pixels-per-cell))
     (frames-per-state 50)
     (i 0))
  (incf i)
  (when (= 0 (rem i frames-per-state))
    (setf life (life-next-state life)))
  (background +white+)
  (dotimes (row (life-rows life))
    (dotimes (col (life-cols life))
      (with-pen (make-pen :fill (if (equalp DEAD (life-get-cell life row col))
                                    +white+
                                    +black+))
        (rect (* col pixels-per-cell)
              (* row pixels-per-cell)
              pixels-per-cell
              pixels-per-cell)))))

(defparameter +smiley+
  (life-from-lists
   (list
    (list DEAD LIVE LIVE LIVE LIVE LIVE DEAD)
    (list LIVE DEAD DEAD DEAD DEAD DEAD LIVE)
    (list LIVE DEAD LIVE DEAD LIVE DEAD LIVE)
    (list LIVE DEAD DEAD DEAD DEAD DEAD LIVE)
    (list LIVE DEAD LIVE LIVE LIVE DEAD LIVE)
    (list LIVE DEAD DEAD DEAD DEAD DEAD LIVE)
    (list DEAD LIVE LIVE LIVE LIVE LIVE DEAD))))
