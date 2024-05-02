(in-package :golfind)

(require 'cl-sat)
(require 'cl-sat.minisat)
(import '(cl-sat:solve
          cl-sat:var))

(defun life-backsearch (life n)
  (let ((previous-states-found 0))
    (loop for i from 1 upto n do
          (multiple-value-bind (parent exists) (find-life-parent-state life)
            (if exists
                (progn
                  (setf life parent)
                  (incf previous-states-found))
                (return))))
    (values life previous-states-found)))

(defun find-life-parent-state (life)
  (multiple-value-bind (solution satisfiable)
      (cl-sat:solve (life->sat life) :minisat)
    (values (if satisfiable (solution->life solution life) nil)
            satisfiable)))

(defun life->sat (life)
  (let ((cols (life-cols life)))
    `(and ,@(loop for coords in (alexandria:map-product #'list
                                                        (alexandria:iota (life-rows life))
                                                        (alexandria:iota cols))
                  collect (multiple-value-bind (row col) (apply #'values coords)
                            (let ((cell (coords->var cols row col))
                                  (neighbours
                                    (mapcar (lambda (coords) (apply #'coords->var cols coords))
                                            (life-neighbour-coords life row col))))
                              (previous-cell-clause cell neighbours (life-get-cell life row col))))))))

(defun life->solution (life)
  (let ((bindings (list)))
    (dotimes (i (life-rows life))
      (dotimes (j (life-cols life))
        (push (list (coords->var (life-cols life) i j)
                    ;; At the time of writing, DEAD and LIVE are nil/t.
                    ;; So we could just use the cell value as the binding
                    ;; directly. But it might change, this is more flexible.
                    (if (equalp DEAD (life-get-cell life i j)) nil t))
              bindings)))
    bindings))

(defun eval-sat-solution (sat solution)
  (eval `(let ,solution ,sat)))

(defun coords->var (cols row col)
  (cl-sat:var (+ (* cols row) col)))

(defun previous-cell-clause (cell neighbours cell-state)
  "Returns clause with possible previous states of cell + its neighbours."
  ;; if LIVE...
  ;; was LIVE and had 2/3 LIVE neighbours
  ;; or was DEAD and had 2 LIVE neighbours
  ;; if DEAD...
  ;; was LIVE and did NOT have 2/3 LIVE neighbours
  ;; or was DEAD and did NOT have 2 LIVE neighbours
  (if (equalp LIVE cell-state)
      `(or (and ,cell
                (or ,@(clauses-with-n-live 2 neighbours)
                    ,@(clauses-with-n-live 3 neighbours)))
           (and ,(bool-not cell)
                (or ,@(clauses-with-n-live 3 neighbours))))
      `(or (and ,cell
                (not (or ,@(clauses-with-n-live 2 neighbours)
                         ,@(clauses-with-n-live 3 neighbours))))
           (and ,(bool-not cell)
                (not (or ,@(clauses-with-n-live 3 neighbours)))))))

(defun bool-not (symbol)
  `(not ,symbol))

(defun clauses-with-n-live (n neighbours)
  "Generates all clauses where n of the variables denote live cells."
  (let ((num-vars (length neighbours)))
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
  (life-from-lists
   (loop for i from 0 upto (1- (life-rows original-life)) collect
         (loop for j from 0 upto (1- (life-cols original-life)) collect
               (if (member (coords->var (life-cols original-life) i j)
                           solution)
                   LIVE
                   DEAD)))))

