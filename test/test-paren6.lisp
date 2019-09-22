
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
        (format nil "~a???" (chain super (boast)))))

    (chain console (log big-thing))

    (defvar thing (new (reasonable-thing 10)))

    (describe
     "defclass6"
     (lambda ()
       (it "Should call superclass constructor"
           (lambda ()
             (chain (expect (@ thing size)) to (equal 18))))))
    ))

  #|

list6
  create6
  defclass6
  =>
  export
  import
  export-default
  const6

|#

(defun run-tests ()
  (with-open-file (s (asdf:system-relative-pathname 'test-paren6 "test/tests.js")
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string (print (tests)) s))
  (uiop:with-current-directory ((asdf:system-source-directory 'test-paren6))
    (print
     (with-output-to-string (out)
       (external-program:run
        "test/node_modules/mocha/bin/mocha" nil :output out :error :output)))))

#|
(defun run-tests ()
  (uiop:with-current-directory ((asdf:system-relative-pathname 'test-paren6 "t/"))
    (with-input-from-string (in (print (tests)))
      (print
       (with-output-to-string (out)
         (external-program:run
          "node" (list "-r" "mocha" "-r" "chai") :input in :output out :error :output))))))
|#
