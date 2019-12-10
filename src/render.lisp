(in-package mona-lisa-gol)

(defsketch run-life ()
  (background +white+))

#|
(run-life (life-from-lists (list (list DEAD LIVE LIVE LIVE LIVE LIVE DEAD)
                                 (list LIVE DEAD DEAD DEAD DEAD DEAD LIVE)
                                 (list LIVE DEAD LIVE DEAD LIVE DEAD LIVE)
                                 (list LIVE DEAD DEAD DEAD DEAD DEAD LIVE)
                                 (list LIVE DEAD DEAD DEAD DEAD DEAD LIVE)
                                 (list DEAD LIVE LIVE LIVE LIVE LIVE DEAD)))
          50)
|#
