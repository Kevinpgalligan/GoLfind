; 1. get cell
; 2. create game
; 3. render (once)
; 4. next generation
; 5. render & update, try with sample
; 6. load/save string representation

(defconstant LIVE 'LIVE)
(defconstant DEAD 'DEAD)

(defstruct life grid)

;; Assumes square grid.
(defun life-size (life) (car (array-dimensions (life-grid life))))

;; Accepts an NxN list of lists.
(defun life-from-lists (init)
    (make-life :grid (make-array
                        (list (list-length init)
                              (list-length (car init)))
                        :initial-contents init)))

(defun life-get-cell (life row col)
    (let ((grid (life-grid life))
          (size (life-size life)))
        (aref grid (mod row size) (mod col size))))

(print (life-grid (life-from-lists (list (list LIVE LIVE) (list DEAD LIVE)))))

#|
(let ((life (life-from-lists (list (list LIVE LIVE) (list DEAD LIVE)))))
    (mapcar (lambda (rowcol)
            (apply (lambda (row col) (life-get-cell life row col))
                   rowcol))
        '(((- 1) (- 1))
          ((- 1) 0)
          (0 0)
          (1 0)
          (1 1)
          (2 2)
          (2 1)
          (2 0))))
|# 
