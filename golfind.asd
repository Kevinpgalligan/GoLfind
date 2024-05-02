(defpackage :golfind-asd
  (:use :cl :asdf))

(in-package :golfind-asd)

(defsystem golfind
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:sketch :alexandria :png-read :skippy :cl-sat.minisat)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "game-of-life")
               (:file "backsearch")
               (:file "gif")
               (:file "animate")))
