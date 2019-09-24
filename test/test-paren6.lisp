
(in-package :cl-user)
(defpackage test-paren6
  (:use #:cl #:parenscript #:paren6))

(in-package :test-paren6)


(defun tests ()
  (ps
    (defvar chai (require "chai"))
    (defvar expect (@ chai expect))

    (defconstant6 sample-constant t)

    (setf sample-constant nil)

    (describe
     "list6"
     (lambda ()
       (it "Should return an array equal to [1, 2, 3, 4, 5]"
           (lambda ()
             (chain
              (expect (list6 1 :... (list 2 3 4) 5))
              ;; or: to have ordered (members (list 1 2 3 4 5))
              to be (an "array") that deep (equals (list 1 2 3 4 5)))))))

    (describe
     "create6"
     (lambda ()
       (defvar arr (create :a 1 :b 2))
       (defvar a 5)
       (defvar c 10)
       (defvar item (create6
                     a c :... arr (:b 3)
                     (set thing (x) (setf (@ this _thing) x))
                     (get thing () (@ this _thing))
                     (defun action (x) x)))
       (it "Should allow same name setting"
           (lambda ()
             (chain
              (expect (@ item c)) to (equal 10))))
       (it "Should shadow values in the correct order"
           (lambda ()
             (chain
              (expect (+ (@ item a) (@ item b)))
              to (equal 4))))
       (it "Should allow creation of setters and getters"
           (lambda ()
             (setf (@ item thing) "asdf")
             (chain (expect (@ item thing))
                    to (equal "asdf"))))
       (it "Should allow the creation of object methods"
           (lambda ()
             (chain (expect (@ item action)) to be (a "function"))))))

    (describe
     "=>"
     (lambda ()
       (it "Shouldn't have its own 'this'"
           (lambda ()
             (setf (@ this y) 2)
             (chain
              (expect (funcall (=> x (+ x (@ this y))) 3))
              to be (equal 5))))))

    (describe
     "defconstant6"
     (lambda ()
       (it "Should set a constant"
           (lambda ()
             (chain (expect sample-constant) to be true)))
       (it "Should not allow changes"
           (lambda ()
             (setf sample-constant nil)
             (chain (expect sample-constant)
                    to be true)))))

    (defclass6 (big-thing)
        (defun constructor (size)
          (setf (@ this size) (chain big-thing (estimate size))))
      (defstatic estimate (quantity)
        (* 2 quantity))
      (defun boast () (chain big-thing (estimate (@ this size)))))

    (defclass6 (reasonable-thing big-thing)
        (defun constructor (size)
          (super (1- size)))
      (defun boast ()
        (+ (chain super (boast)) "???"))
      (set legs (count)
           (setf (@ this _legs) count))
      (get legs ()
           (@ this _legs)))

    (defvar thing (new (reasonable-thing 10)))

    (describe
     "defclass6"
     (lambda ()
       (it "Should call superclass constructor"
           (lambda ()
             (chain (expect (@ thing size)) to (equal 18))))
       (it "Should call superclass method"
           (lambda ()
             (chain (expect (chain thing (boast))) to (equal "36???"))))
       (it "Should have getter/setter support"
           (lambda ()
             (setf (@ thing legs) 6)
             (chain (expect (@ thing legs)) to (equal 6))))))

    (describe
     "import"
     (lambda ()
       (import (a (b x) c) "./module.js")
       (it "Should bind same names from module"
           (lambda ()
             (chain (expect a) to (equal 1))))
       (it "Should bind to alternative names when asked"
           (lambda ()
             (chain (expect x) to (equal 2))))
       (it "Should use new names that were specified by export"
           (lambda ()
             (chain (expect c) to (equal 3))))
       (import ((:default y)) "./module.js")
       (it "Should supply the default export on request"
           (lambda ()
             (chain (expect y) to have (property "a" 1))))
       (import ((:all z)) "./module.js")
       (it "Should supply the whole export module on request"
           (lambda ()
             (chain (expect z) to have (property "a" 1))))))

    (describe
     "export-default"
     (lambda ()
       (it "Should set module.exports to the requested value"
           (lambda ()
             (export-default 3)
             (chain (expect (@ module exports)) to (equal 3))))))

    (describe
     "export"
     (lambda ()
       (it "Should set module.exports to the requested import"
           (lambda ()
             (export (a b c) :from "./module.js")
             (chain (expect (@ module exports a)) to (equal 1))))
       (it "Should export an object"
           (lambda ()
             (export nil :source (create a 10 b 20))
             (chain (expect (@ module exports b)) to (equal 20))))
       (it "Should add a variable to module.exports"
           (lambda ()
             (defvar c 30)
             (export (c))
             (chain (expect (@ module exports)) to be (an "object") with (property "c" 30))))))
    ))


(defun write-test-module ()
  (with-open-file (s (asdf:system-relative-pathname 'test-paren6 "test/module.js")
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string
     (ps
       (defvar a 1)
       (defvar b 2)
       (defvar d 3)
       (export (a b (d c))))
     s)))

(defun run-tests ()
  (write-test-module)
  (with-open-file (s (asdf:system-relative-pathname 'test-paren6 "test/tests.js")
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string (tests) s))
  (uiop:with-current-directory ((asdf:system-source-directory 'test-paren6))
    (princ
     (with-output-to-string (out)
       (external-program:run
        "mocha" nil :output out :error :output)))))

(run-tests)

#|
(defun run-tests ()
  (uiop:with-current-directory ((asdf:system-relative-pathname 'test-paren6 "t/"))
    (with-input-from-string (in (print (tests)))
      (print
       (with-output-to-string (out)
         (external-program:run
          "node" (list "-r" "mocha" "-r" "chai") :input in :output out :error :output))))))
|#
