; 1. seriously consider representation of grid & decide on final one.
; 2. next state
; 3. render & update, try with sample
; 4. load/save string representation
; 5. some way to compare grid, that's what's needed for GA
; 6. basic GA
; 7. apply GA to the target problem
; ?. at some point, need tool to convert images; rescale, make pixelated black+white, save to string
; ?. allow non NxN grid

(defconstant LIVE t)
(defconstant DEAD nil)

(defstruct life grid rows cols)

;; Accepts an NxN list of lists.
(defun life-from-lists (init)
    (let ((rows (list-length init))
          (cols (list-length (car init))))
        (make-life :grid (make-array (list rows cols) :initial-contents init)
                   :rows rows
                   :cols cols)))

(defun life-get-cell (life row col)
    (aref (life-grid life)
          (mod row (life-rows life))
          (mod col (life-cols life))))

#|
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
|#
