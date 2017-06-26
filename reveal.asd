;;;; reveal.asd

(asdf:defsystem #:reveal
  :description "Provides introspection into a running Common Lisp image through a web interface"
  :author "Torbjorn Maro <torbjorn.maro@gmail.com>"
  :license "MIT"
  :depends-on (#:hunchentoot
               #:cl-who)
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "reveal")))

