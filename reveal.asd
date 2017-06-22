;;;; reveal.asd

(asdf:defsystem #:reveal
  :description "Describe reveal here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot)
  :serial t
  :components ((:file "package")
               (:file "reveal")))

