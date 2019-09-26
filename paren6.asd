;;;; paren6.asd

(asdf:defsystem #:paren6
  :description "Paren6 is a set of ES6 macros for Parenscript"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license  "Apache License, version 2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:parenscript #:alexandria)
  :components ((:file "package")
               (:file "paren6")))
