;;;; package.lisp

(defpackage #:paren6
  (:use #:cl
        #:parenscript
        #:alexandria)
  (:shadowing-import-from #:parenscript #:switch)
  (:export
   #:export
   #:export-default
   #:import
   #:list6
   #:create6
   #:=>
   #:defclass6
   #:defconstant6
   #:super
   #:import-into
   #:for-of))
