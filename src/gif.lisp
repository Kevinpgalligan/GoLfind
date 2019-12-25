(in-package mona-lisa-gol)

(require 'skippy)
(import '(skippy:make-color-table
          skippy:make-data-stream
          skippy:ensure-color
          skippy:add-image
          skippy:make-image
          skippy:output-data-stream
          skippy:make-image-data))

(defun life-to-gif (life
                    gif-path
                    &key
                      (num-states 10)
                      (frames-per-state 5)
                      (frames-for-last-state 20)
                      (pixels-per-cell 2))
  (let* ((height (* pixels-per-cell (life-rows life)))
         (weight (* pixels-per-cell (life-cols life)))
         (color-table (make-color-table))
         (data-stream (make-data-stream :height height
                                        :width weight
                                        :loopingp t
                                        :color-table color-table))
         (white (ensure-color #xFFFFFF color-table))
         (black (ensure-color #x000000 color-table)))
    (dotimes (state num-states)
      (add-image (make-image :height height
                             :width weight
                             :data-stream data-stream
                             :image-data (life-to-image-data life
                                                             height
                                                             weight
                                                             pixels-per-cell
                                                             white
                                                             black)
                             :delay-time (if (= state (1- num-states))
                                             frames-for-last-state
                                             frames-per-state))
                 data-stream))
    (output-data-stream data-stream gif-path)))

(defun life-to-image-data (life height weight pixels-per-cell dead-color live-color)
  (let ((data (make-image-data weight height)))
    (dotimes (row (life-rows life))
      (dotimes (col (life-cols life))
        (dotimes (pixel-index pixels-per-cell)
          (setf (aref data (+ (* row pixels-per-cell (life-cols life))
                              (* col pixels-per-cell)
                              pixel-index))
                (if (equalp DEAD (life-get-cell life row col))
                    dead-color
                    live-color)))))))
