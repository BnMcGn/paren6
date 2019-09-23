;;;; test-paren6.asd

(asdf:defsystem #:test-paren6
  :description "Tests for Paren6"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:parenscript
               #:paren6
               #:external-program)
  :serial t
  :components ((:module :test
                        :serial t
                        :components ((:file "test-paren6")))))


