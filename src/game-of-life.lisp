; 1. render (once)
; 2. next generation
; 3. render & update, try with sample
; 4. load/save string representation
; 5. some way to compare grid, that's what's needed for GA
; 6. basic GA
; 7. apply GA to the target problem
; ?. at some point, need tool to convert images; rescale, make pixelated black+white, save to string
; ?. allow non NxN grid

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

(print (let ((life (life-from-lists (list (list LIVE LIVE) (list DEAD LIVE)))))
    (mapcar (lambda (rowcol)
                (apply (lambda (row col) (life-get-cell life row col))
                       rowcol))
        (list
            (list (- 1) (- 1))
            (list (- 1) 0)
            (list 0 0)
            (list 1 0)
            (list 1 1)
            (list 2 2)
            (list 2 1)
            (list 2 0)))))
