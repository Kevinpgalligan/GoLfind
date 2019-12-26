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
                      (num-states 25)
                      (frames-per-state 20)
                      (frames-for-last-state 50)
                      (pixels-per-cell 2))
  (let* ((height (* pixels-per-cell (life-rows life)))
         (width (* pixels-per-cell (life-cols life)))
         (color-table (make-color-table))
         (data-stream (make-data-stream :height height
                                        :width width
                                        :loopingp t
                                        :color-table color-table))
         (white (ensure-color #xFFFFFF color-table))
         (black (ensure-color #x000000 color-table)))
    (dotimes (state num-states)
      (progn
        (add-image (make-image :height height
                               :width width
                               :data-stream data-stream
                               :image-data (life-to-image-data life
                                                               height
                                                               width
                                                               pixels-per-cell
                                                               white
                                                               black)
                               :delay-time (if (= state (1- num-states))
                                               frames-for-last-state
                                               frames-per-state))
                   data-stream)
        (setf life (life-next-state life))))
    (output-data-stream data-stream gif-path)))

(defun life-to-image-data (life height width pixels-per-cell dead-color live-color)
  (let* ((data (make-image-data width height))
         (pixels-per-line (* pixels-per-cell (life-cols life)))
         (pixels-per-row (* pixels-per-cell pixels-per-line)))
    (progn
      (dotimes (row (life-rows life))
        (dotimes (col (life-cols life))
          (let ((color (if (equalp DEAD (life-get-cell life row col))
                           dead-color
                           live-color)))
            (dotimes (row-pixel pixels-per-cell)
              (dotimes (col-pixel pixels-per-cell)
                (setf (aref data (+ (* row pixels-per-row)
                                    (* row-pixel pixels-per-line)
                                    (* col pixels-per-cell)
                                    col-pixel))
                      color))))))
      data))))
