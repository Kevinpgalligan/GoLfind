(in-package mona-lisa-gol)

(require 'sketch)
(use-package 'sketch)

(defmacro run-life (life &key (pixels-per-cell 1) (frames-per-state 50))
  `(make-instance 'life-animate
    :initial-life ,life
    :initial-pixels-per-cell ,pixels-per-cell
    :initial-frames-per-state ,frames-per-state))

(defsketch life-animate
    ((initial-life (random-life 50 100))
     ;; This is a hack. Can't directly override the parameters,
     ;; for some reason, so have to override a different variable
     ;; and then set the actual variable.
     ;; Not sure if it's an issue with the sketch library or my
     ;; stupidity.
     ;; (same hack applied to pixels-per-cell and frames-per-state).
     (life initial-life)
     (initial-pixels-per-cell 1)
     (pixels-per-cell initial-pixels-per-cell)
     (width (* (life-cols life) pixels-per-cell))
     (height (* (life-rows life) pixels-per-cell))
     (initial-frames-per-state 50)
     (frames-per-state initial-frames-per-state)
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

