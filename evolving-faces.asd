(defpackage :evolving-faces-asd
  (:use :cl :asdf))

(in-package :evolving-faces-asd)

(defsystem evolving-faces
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:sketch)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "game-of-life")
               (:file "render")
               (:file "genetic")))
