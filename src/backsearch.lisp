(in-package mona-lisa-gol)

(require 'cl-sat)
(import '(cl-sat:solve))

(defun find-life-parent-state (life)
  (multiple-value-bind (solution satisfiable)
      (solve (life->sat life) :minisat)
    solution))
    #|
    (values (if satisfiable (solution->life solution life) nil)
            satisfiable)))
|#

(defun life->sat (life)
  (let ((cols (life-cols life)))
    `(and ,@(loop for coords in (alexandria:map-product #'list
                                                        (alexandria:iota (life-rows life))
                                                        (alexandria:iota cols))
                  collect (multiple-value-bind (row col) (apply #'values coords)
                            (let ((cell (coords->symbol cols row col))
                                  (neighbours
                                    (mapcar (lambda (coords) (apply #'coords->symbol cols coords))
                                            (life-neighbour-coords life row col))))
                              (previous-cell-clause cell neighbours (life-get-cell life row col))))))))

(defun coords->symbol (cols row col)
  (make-symbol (write-to-string (+ (* cols row) col))))

(defun previous-cell-clause (cell neighbours cell-state)
  "Returns clause with possible previous states of cell + its neighbours."
  ;; if LIVE...
  ;; was LIVE and had 2/3 LIVE neighbours
  ;; or was DEAD and had 2 LIVE neighbours
  ;; if DEAD...
  ;; the negation of the above.
    (let ((clause `(or (and ,cell
                          (or ,@(clauses-with-n-live 2 neighbours)
                              ,@(clauses-with-n-live 3 neighbours)))
                     (and ,(bool-not cell)
                          (or ,@(clauses-with-n-live 2 neighbours))))))
      (if (equalp LIVE cell-state)
          clause
          `(not ,clause))))

(defun bool-not (symbol)
  (make-symbol (concatenate 'string "!" (string symbol))))

(defun clauses-with-n-live (n neighbours)
  "Generates all clauses where n of the variables denote live cells."
  (let* ((num-vars (length neighbours)))
    (if (> n num-vars)
        ;; Impossible to satisfy, just return an impossible clause.
        (list `(and a !a))
        (mapcar (lambda (flags) (clause-with-negation neighbours flags))
                (all-possible-flags num-vars n)))))

(defun clause-with-negation (neighbours flags)
  `(and ,@(mapcar (lambda (v flag) (if flag v (bool-not v))) neighbours flags)))

(defun all-possible-flags (n num-live)
  (cond ((> num-live n)
         (list))
        ((= n 0)
         (list (list)))
        ((= num-live 0)
         (mapcar (lambda (flags) (cons nil flags))
                 (all-possible-flags (1- n) 0)))
        (t
         (concatenate 'list
                      (mapcar (lambda (flags) (cons nil flags))
                              (all-possible-flags (1- n) num-live))
                      (mapcar (lambda (flags) (cons t flags))
                              (all-possible-flags (1- n) (1- num-live)))))))

(defun solution->life (solution original-life)
  ;; todo: take symbols, parse as integers, convert index to row/col, set cells
  (random-life 3 3))

