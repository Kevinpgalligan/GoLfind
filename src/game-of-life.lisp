; 0. structure as package, get quicklisp + dev env working (PCL).
; 1. display graphics.
; 2. load b/w image, convert to life, run it.
; 3. should be able to save gif.
; 4. GA.

(defconstant LIVE t)
(defconstant DEAD nil)

(defstruct life grid rows cols)

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

(defconstant NEIGHBOUR-OFFSETS
    (list (list (- 1) (- 1))
          (list (- 1) 0)
          (list (- 1) 1)
          (list 0 (- 1))
          (list 0 1)
          (list 1 (- 1))
          (list 1 0)
          (list 1 1)))

;; Buggy for small grids, counts self as a neighbour.
(defun life-live-neighbours (life row col)
    (let ((coords (list row col)))
        (remove DEAD
            (mapcar
                (lambda (coords)
                    (destructuring-bind (row col) coords
                        (life-get-cell life row col)))
                (loop for offsets in NEIGHBOUR-OFFSETS collect
                    (mapcar #'+ offsets coords))))))

(defun life-next-state (life)
    (let ((next-grid
           (make-array (list (life-rows life) (life-cols life))
                       :element-type 'boolean
                       :initial-element DEAD)))
        (loop for i from 0 to (1- (life-rows life)) do
            (loop for j from 0 to (1- (life-cols life)) do
                (let ((current (life-get-cell life i j))
                      (neighbours (list-length (life-live-neighbours life i j))))
                    (if (or (and (equalp current LIVE)
                                 (find neighbours '(2 3)))
                            (and (equalp current DEAD)
                                 (= neighbours 3)))
                        (setf (aref next-grid i j) LIVE)
                        nil))))
        (destructuring-bind (rows cols) (array-dimensions next-grid)
            (make-life :grid next-grid
                       :rows rows
                       :cols cols))))

(defun compare-lives (l1 l2)
    (let* ((diff
            (mapcar #'equalp
                                (2d-array-to-flat-list (life-grid l1))
                                (2d-array-to-flat-list (life-grid l2))))
           (diff-size (list-length diff))
           (matching (list-length (remove nil diff)))
           (non-matching (- diff-size matching)))
        (values matching non-matching)))

(defun 2d-array-to-flat-list (array)
    (flatten
        (loop for i below (array-dimension array 0) collect
            (loop for j below (array-dimension array 1) collect
                (aref array i j)))))

(defun flatten (xs)
    (cond ((null xs) nil)
          (t (append (car xs) (flatten (cdr xs))))))
