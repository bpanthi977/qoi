;;;; package.lisp

(defpackage #:qoi
  (:use #:cl)
  (:local-nicknames (#:u #:alexandria))
  (:export #:decode
           #:encode))
