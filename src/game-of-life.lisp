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

; TODO... implement
(defun life-surrounding-cells (life row col) nil)

(defun life-next-state (life)
    (let ((next-grid
           (make-array (list (life-rows life) (life-cols life))
                       :element-type boolean ; TODO... "boolean" is undefined, for some reason?
                       :initial-element DEAD)))
        (loop for i from 0 to (1- (life-rows)) do
            (loop for j from 0 to (1- (life-cols)) do
                (let ((current (life-get-cell i j))
                      (neighours
                       ; TODO... filter this for LIVE cells, then count
                       (life-surrounding-cells life i j)))
                    (if (or (and (equalp current LIVE)
                                 (find neighbours '(2 3)))
                            (and (equalp current DEAD)
                                 (= neighbours 3)))
                        (setf (aref next-grid i j) LIVE)
                        nil))))
        (destructuring-bind (rows cols) (array-dimensions next-grid)
            (make-life next-grid rows cols))))
