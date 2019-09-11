;;;; paren6.asd

(asdf:defsystem #:paren6
  :description "Describe paren6 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:parenscript)
  :components ((:file "package")
               (:file "paren6")))
