(in-package evolving-faces)

(defstruct individual life fitness)
(defstruct result best population fitness-datapoints)
(defstruct fitness-datapoint p0 p50 p100)

(defun evolve (target &key result (pop-size 100) (states-to-eval 100))
  (if (null result)
      (pop-to-result (random-population target pop-size states-to-eval) (list))
      (pop-to-result (evolve-population (result-population result) target pop-size states-to-eval)
                     (result-fitness-datapoints result))))

(defun pop-to-result (population current-fitness-datapoints)
  (make-result
   :best (first (last population))
   :population population
   :fitness-datapoints (append current-fitness-datapoints
                               (list (get-fitness-datapoint population)))))

(defun evolve-population (population target pop-size states-to-eval)
  (let* ((elites (floor pop-size 5))
         (crossovers (floor pop-size 2))
         (mutants (- pop-size elites crossovers)))
    (order-individuals
     (append
      (last population elites)
      (loop repeat crossovers
            collect
            (let ((crossover-life
                    (apply #'crossover
                           (mapcar #'individual-life
                                   (list (select-random population pop-size)
                                         (select-random population pop-size))))))
              (make-individual :life crossover-life
                               :fitness (evaluate-fitness crossover-life target states-to-eval))))
      (loop repeat mutants
            collect
            (let ((mutated-life (mutate (individual-life (select-random population pop-size)))))
              (make-individual :life mutated-life
                               :fitness (evaluate-fitness mutated-life target states-to-eval))))))))

(defun select-random (population pop-size)
  ;; Rank-based probability.
  (let* ((proportion-sum (/ (* pop-size (1+ pop-size)) 2))
         (choice (random proportion-sum)))
    (loop for n from 1 upto pop-size
          for individual in population
          for sum = 1 then (+ sum n)
          do (when (> sum choice)
               (return individual)))))

(defun crossover (l1 l2)
  (let* ((rows (life-rows l1))
         (cols (life-cols l1))
         (new-grid (alexandria:copy-array (life-grid l1))))
    (loop for i from 0 upto (1- rows) do
          (loop for j from 0 upto (1- cols) do
                (when (and (not (equalp (life-get-cell l1 i j) (life-get-cell l2 i j)))
                           (= (random 2) 0))
                  ;; If the two lives are not the same in this cell, make a random
                  ;; choice between them.
                  (setf (aref new-grid i j) (life-get-cell l2 i j)))))
    (make-life :grid new-grid :rows rows :cols cols)))

(defun mutate (life)
  (let* ((rows (life-rows life))
         (cols (life-cols life))
         (new-grid (alexandria:copy-array (life-grid life))))
    (loop for i from 0 upto (1- rows) do
          (loop for j from 0 upto (1- cols) do
                (when (= 0 (random 4)) ; 25% mutation rate
                  (setf (aref new-grid i j)
                        (if (equalp DEAD (life-get-cell life i j))
                            LIVE
                            DEAD)))))
    (make-life :grid new-grid :rows rows :cols cols)))

(defun get-fitness-datapoint (population)
  (make-fitness-datapoint
   :p0 (individual-fitness (first population)) 
   :p50 (individual-fitness (middle-element population))
   :p100 (individual-fitness (first (last population)))))

(defun middle-element (list)
  (let ((length (list-length list)))
    (if (zerop length)
        nil
        (nth (floor length 2) list))))

(defun random-population (target pop-size states-to-eval)
  (order-individuals
   (loop for _ from 1 upto pop-size collect
         (random-individual target states-to-eval))))

(defun order-individuals (individuals)
  ;; Sort from highest fitness score (worst fitness) to lowest.
  (sort individuals #'> :key #'individual-fitness))

(defun random-individual (target states-to-eval)
  (let ((life (random-life target)))
    (make-individual :life life
                     :fitness (evaluate-fitness life target states-to-eval))))

(defun random-life (target)
  (life-from-lists
   (loop for _ from 1 upto (life-rows target) collect
         (loop for _ from 1 upto (life-cols target) collect
               (if (zerop (random 2))
                   DEAD
                   LIVE)))))

(defun evaluate-fitness (life target states-to-eval)
  (loop for state from 1 upto states-to-eval
        for curr-life = life then (life-next-state curr-life)
        sum (score-life curr-life target state states-to-eval)))

(defun score-life (life target state states-to-eval)
  (multiple-value-bind (matching non-matching) (compare-lives life target)
    (if (< state (/ states-to-eval 10)) ; ugly magic number
        ;; Initially punish for being like the target.
        matching
        ;; After a while, punish for being unlike the target, with more
        ;; weighting towards the end state.
        (* state non-matching))))
