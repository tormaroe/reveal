;;;; package.lisp

(defpackage #:data
  (:use #:cl))

(defpackage #:reveal
  (:use #:cl #:cl-who)
  (:import-from #:hunchentoot #:easy-acceptor
                              #:define-easy-handler)
  (:export #:start
           #:stop))
