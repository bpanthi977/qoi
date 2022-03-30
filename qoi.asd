;;;; qoi.asd

(asdf:defsystem #:qoi
  :description "Library for encoding/decoding QOI (Quite OK Image Format) files"
  :author "Bibek Panthi <bpanthi977@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "qoi")))
