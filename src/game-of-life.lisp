(in-package :mona-lisa-gol)

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

(defparameter +neighbour-offsets+
  (list (list (- 1) (- 1))
        (list (- 1) 0)
        (list (- 1) 1)
        (list 0 (- 1))
        (list 0 1)
        (list 1 (- 1))
        (list 1 0)
        (list 1 1)))

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

(defun life-live-neighbours (life row col)
  (remove DEAD
          (mapcar (lambda (coords) (apply #'life-get-cell life coords))
                  (life-neighbour-coords life row col))))

(defun life-neighbour-coords (life row col)
  (remove-duplicates
   (loop for offset in +neighbour-offsets+ collect
         (list (mod (+ row (car offset)) (life-rows life))
               (mod (+ col (cadr offset)) (life-cols life))))
   :test #'equalp))

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

(defun random-life (rows cols)
  (life-from-lists
   (loop for _ from 1 upto rows collect
         (loop for _ from 1 upto cols collect
               (if (zerop (random 2))
                   DEAD
                   LIVE)))))

(defun load-png-as-life (path)
  (life-from-lists
   (let* ((pixels (png-read:image-data (png-read:read-png-file path)))
          (cols (array-dimension pixels 0))
          (rows (array-dimension pixels 1)))
     (loop for row from 0 upto (1- rows) collect
           (loop for col from 0 upto (1- cols) collect
                 ;; Don't bother to check the other channels, this
                 ;; is only supposed to work for black & white images.
                 (if (< (aref pixels col row 0) 255) LIVE DEAD))))))

(defparameter +smiley+
  (life-from-lists
   (list
    (list DEAD LIVE LIVE LIVE LIVE LIVE LIVE LIVE DEAD)
    (list LIVE DEAD DEAD DEAD DEAD DEAD DEAD DEAD LIVE)
    (list LIVE DEAD DEAD LIVE DEAD LIVE DEAD DEAD LIVE)
    (list LIVE DEAD LIVE DEAD DEAD DEAD LIVE DEAD LIVE)
    (list LIVE DEAD DEAD LIVE LIVE LIVE DEAD DEAD LIVE)
    (list LIVE DEAD DEAD DEAD DEAD DEAD DEAD DEAD LIVE)
    (list DEAD LIVE LIVE LIVE LIVE LIVE LIVE LIVE DEAD))))
