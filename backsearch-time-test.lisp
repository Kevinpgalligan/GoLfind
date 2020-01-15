;;; The vgplot library used for plotting here is just an interface
;;; to gnuplot, so gnuplot must be installed first.

(ql:quickload 'vgplot)

(defparameter num-cells-vs-time
  (loop for cols in (list 2 5 8 11 14 17 20) collect
        (let ((start-time (get-internal-real-time)))
          (find-life-parent-state (random-life 20 cols))
          (list (* 20 cols) (- (get-internal-real-time) start-time)))))

(progn 
  (vgplot:plot (mapcar #'first num-cells-vs-time)
               (mapcar #'(lambda (pair) (/ (second pair) 1000))
                       num-cells-vs-time)
               "r-;")
  (vgplot:xlabel "cells")
  (vgplot:ylabel "seconds")
  (vgplot:title "Backsearch time vs number of cells"))
