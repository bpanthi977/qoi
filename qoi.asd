;;;; qoi.asd

(asdf:defsystem #:qoi
  :description "Describe qoi here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "qoi")))
