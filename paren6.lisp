;;;; paren6.lisp

;; Add some of the ES6 functionality to parenscript

(in-package #:paren6)


(defpsmacro defconstant6 (name value)
  "As per ES6, the variable cannot be redeclared or redefined, but its contents may be mutable."
  `(chain -object
          (define-property
              (if (eql (typeof global) "object") global window)
              ,(symbol-to-js-string name)
            (create :value ,value :enumerable t :writable false :configurable false))))

#|
Constant declaration

ES6 introduced the const keyword, which cannot be redeclared or reassigned, but is not immutable.

// ES6
const CONST_IDENTIFIER = 0; // constants are uppercase by convention

    MDN Reference: const

|#

(defpsmacro => (params &body body)
  "The parenscript equivalent of the ES6 arrow operator. => is different from lambda in two ways. It doesn't have its own copy of 'this' and when invoked with a single parameter, that parameter doesn't need to be enclosed in a list."
  (let ((params (if (listp params) params (list params))))
    `(chain (lambda ,params ,@body) (bind this))))


(defun getters-and-setters (target funcs)
  "Generates code from funcs to add getters and setters to the object specified by target. Used by the create6 and defclass6 macros."
  (let ((stor (make-hash-table))
        (results nil))
    (dolist (func funcs)
      (unless (gethash (second func) stor)
        (setf (gethash (second func) stor) (list nil nil)))
      (setf (elt (gethash (second func) stor)
                 (cond
                   ((string-equal 'get (car func)) 0)
                   ((string-equal 'set (car func)) 1)
                   (t (error "Set/get spec must begin with set or get"))))
            func))
    (maphash-values
     (lambda (funcs)
       (let ((get (car funcs))
             (set (second funcs)))
         (push
          `(chain
            -object
            (define-property
                ,target ,(symbol-to-js-string (second (or get set)))
              (create
               ,@(when get
                   `(get (lambda ,@(cddr get))))
               ,@(when set
                   `(set (lambda ,@(cddr set)))))))
          results)))
     stor)
    results))


(defpsmacro create6 (&rest items-pairlists-and-dotted)
  "Due to the differences in lisp vs. javascript syntax, the ES6 shorthand for same name properties is somewhat counterintuitive. Create6 implements it by assuming that any symbol found at the top level of the macro is meant to refer to a variable of the same name. Non same name pairs must be placed in parentheses.

Given this ES6:

    var obj = {a, b, c: 1, d: 2}

the equivalent create6 macro reads:

    (create6 a b (c 1 d 2))

Multiple pairs can be placed in a single list.

Create6 also supports spread syntax in its top level. An object following the :... keyword in the body of the macro will have its fields copied into the newly created object.

    (let
       ((a 1)
        (b 3)
        (d 5))
     (create6 a b :... (create c 2 d 4) d))

results in

    {a: 1, b: 3, c: 2, d: 5}

Create6 allows the insertion of setters, getters and ordinary functions in place. Placed in the top level of the macro, they take this form:

    (<functype> <name> (<lambda list>) <body>)

where functype is one of get, set or defun. On the odd chance that you wish to start a toplevel list with one of these symbols and not have it turned into a function, use a keyword.

    (create6 a b (defun c (...) ...) d (:get e))

results in

    {a: a,
     b: b,
     c: function (...) {...},
     d: d,
     get: e} "
  (let ((stor (gensym))
        (spreadp nil)
        (accum (list))
        (get-and-set nil))
    (dolist (itm items-pairlists-and-dotted)
      (if spreadp
          (if (eq itm :...)
              (error "Can't have two :... keywords in a row")
              (progn
                (setf spreadp nil)
                (push `(merge ,stor ,itm) accum)))
          (if (listp itm)
              (if (keywordp (car itm))
                  (push `(merge ,stor (create ,@itm)) accum)
                  (cond
                    ((string-equal (car itm) 'defun)
                     (push `(setf (@ ,stor ,(second itm))
                                  (lambda ,@(cddr itm))) accum))
                    ((string-equal (car itm) 'get)
                     (push itm get-and-set))
                    ((string-equal (car itm) 'set)
                     (push itm get-and-set))
                    (t (push `(merge ,stor (create ,@itm)) accum))))
              (if (eq itm :...)
                  (setf spreadp t)
                  (push `(setf (@ ,stor ,itm) ,itm) accum)))))
    `(let ((,stor (create)))
       (labels ((merge (target obj)
                  (chain #:|Object|
                         (#:keys obj)
                         (#:for-each (lambda (key) (setf (getprop target key)
                                                         (getprop obj key)))))))
         ,@(nreverse accum))
       ,@(when get-and-set
           (getters-and-setters stor get-and-set))
       ,stor)))


(defpsmacro list6 (&rest items-and-lists)
  "List6 creates lists much like the regular list macro, but adds the `:...` spread syntax operator, allowing other lists to be spread into the created list.

    (let ((arr (list 1 2 3)))
      (create6 4 :... arr 5 6 :... arr))

results in

    [4, 1, 2, 3, 5, 6, 1, 2, 3]''"
  (let ((accum nil)
        (curr nil)
        (spreadp nil))
    (dolist (itm items-and-lists)
      (if (eq :... itm)
          (if spreadp
              (error ":... can't follow :...")
              (progn
                (when curr
                  (push (cons 'list (nreverse curr)) accum)
                  (setf curr nil))
                (setf spreadp t)))
          (if spreadp
              (progn
                (push itm accum)
                (setf spreadp nil))
              (push itm curr))))
    (when curr
      (push (cons 'list (nreverse curr)) accum))
    `(apply (@ -array prototype concat) ,@(nreverse accum))))

;;;Defclass6 stuff

(setf (gethash 'chain2 parenscript::*macro-toplevel*) (gethash 'chain parenscript::*macro-toplevel*))

(defun super-wrap (code superclass has-super)
  ;;Only add access to super class constructor if there is a parent class
  (when (some (lambda (x)
                (and (symbolp x)
                     (starts-with-subseq (string 'super.) (string x))))
              (flatten code))
    (warn
     "Defclass6 doesn't handle super calls of the form super.x - please rewrite as (chain super (x ...))"))
  (if has-super
      `(macrolet ((super (&rest params)
                    `(chain2 ,,`',superclass (call this ,@params)))
                  (chain (&body body)
                    (if (string-equal (car body) 'super)
                        (if (listp (second body))
                            (destructuring-bind (_ (method &rest params) &rest more) body
                              (declare (ignore _))
                              `(chain2 ,,`',superclass prototype ,method
                                       (call this ,@params) ,@more))
                            ;; Hope this is what is wanted... could also be ,superclass prototype
                            `(chain2 ,,`',superclass ,@(cdr body)))
                        `(chain2 ,@body))))
         ,code)
      code))

(defun proc-defun (classname code)
  (let ((methodname (second code))
        (params (third code))
        (body (cdddr code)))
    `(setf (@ ,classname prototype ,methodname)
          (lambda ,params ,@body))))

(defun proc-static (classname code)
  (let ((methodname (second code))
        (params (third code))
        (body (cdddr code)))
    `(setf (@ ,classname ,methodname)
           (lambda ,params ,@body))))

(defpsmacro defclass6 ((name &optional extends) &body body)
  "Defclass6 is used to define ES6 style classes. It takes the following form:

    (defclass6 (classname parent)
      (defun constructor () ...)
      (defun method () ...)
      (defstatic static-method () ...)
      (get item () ...)
      (set item (value) ...))

The parent class is optional. If it is provided, then (super) is defined inside of the constructor and results in a call to the parent constructor. Bound superclass methods are available under (chain super (methodname ...)). Note that super.methodname style calls will not work.

As in ES6, the method named 'constructor' is recognized as the constructor. Static methods, getters and setters are also available as per the form above. "
  (let ((constructor nil)
        (methods nil)
        (extends-sym (gensym "extends"))
        (get-and-set nil))
    (dolist (itm body)
      (cond
        ((string-equal (second itm) 'constructor)
         (if constructor
             (error "Class can't have more than one constructor")
             (setf constructor itm)))
        ((string-equal (car itm) 'defun)
         (push (super-wrap (proc-defun name itm) extends-sym extends) methods))
        ((string-equal (car itm) 'defstatic)
         (push (super-wrap (proc-static name itm) extends-sym extends) methods))
        ((string-equal (car itm) 'get)
         (push itm get-and-set))
        ((string-equal (car itm) 'set)
         (push itm get-and-set))
        (t (error "Defclass6 only allows defun, defstatic, get and set calls in its body"))))
    (when constructor
      (unless (< 2 (length constructor))
        (error "Constructor needs a parameter list")))
    `(progn
       (defvar ,extends-sym ,extends)
       ,(if constructor
            (super-wrap `(defun ,name ,(third constructor) ,@(cdddr constructor)) extends-sym extends)
            `(defun ,name ()))
       ,@(when extends
           (list `(setf (@ ,name prototype) (chain -object (create (@ ,extends prototype))))
                 `(setf (@ ,name prototype constructor) ,name)))
       ,@methods
       ,@(when get-and-set
           (getters-and-setters `(@ ,name prototype) get-and-set)))))


(defpsmacro export ((&rest symbol-list) &key from source)
  "The export macro registers items in the module.exports object so that the current Javascript file can be imported by other files.

The first parameter, a list of symbols, is the set of names to be added to the export list. It will be taken from the environment if no :from or :source parameter is specified. If the symbol list is empty, the entire :from or :source object will have its keys exported. If no :from or :source is specified, then the symbol list can not be empty.

Use the :from keyword to export from another module or submodule. The :source keyword is used to export an object or portions of an object in the current namespace.

Note that paren6 uses CommonJS exports internally. Because CommonJS doesn't have a dedicated slot for default exports, mixing calls to export and export-default within the same module will cause overwriting."
  (let ((obj (gensym))
        (objsource (cond
                     (from `(require ,from))
                     (source source))))
    (when (and from source)
      (error "Only one of :from or :source can be used per export invocation."))
    (when (and from (not (stringp from)))
      (error "Exported module name in :from must be a string"))
    (unless (or symbol-list from source)
      (error
       "Either a list of symbols must be supplied or a :from or :source parameter must be present."))
    `(let ((,obj ,objsource))
       (unless (eql "object" (typeof (@ module exports)))
         (setf (@ module exports) (create)))
       ,@(if symbol-list
             (mapcar (lambda (sym)
                       (let* ((outsym (if (listp sym) (second sym) sym))
                              (insym (if (listp sym) (car sym) sym))
                              (insrc (if objsource `(@ ,obj ,insym) insym)))
                         `(setf (@ module exports ,outsym) ,insrc)))
                     symbol-list)
             `((chain #:|Object|
                      (#:keys ,obj)
                      (#:for-each (lambda (key)
                                    (setf (getprop (@ module exports) key)
                                          (getprop ,obj key))))))))))

(defpsmacro export-default (item &key from)
  "The export-default macro replaces the contents of module.exports with the specified item. If the :from parameter names a module, item will be taken from that module. If item is NIL, the whole module will be exported.

Note that export-default will overwrite the results of any earlier calls to export or export-default."
  (let ((obj (gensym)))
    (when (and from (not (stringp from)))
      (error "Exported module name in :from must be a string"))
    (if from
        (if item
            `(let ((,obj (require ,from)))
               (setf (@ module exports) (@ ,obj ,item)))
            `(setf (@ module exports) (require ,from)))
        (if item
            `(setf (@ module exports) ,item)
            (error "Nothing to export")))))

(defpsmacro import ((&rest names) module)
  "Import from a javascript file or library. The second parameter, module, is a string that specifies the source. The first parameter is a list of names to be bound to things from the incoming module.

Import expects that any symbol in the names list can be found in the import. The item will be bound to the same name in the current environment. If you wish to bind something to an alternate name, place the name in a list, followed by the alternate name.

For example:

    (import (a (b x)) \"./my-module.js\")

will bind the item 'a' from my-module.js to 'a' in the present module, and will bind 'b' to 'x'.

You may also import the default export:

    (import ((:default -my-module)) \"./my-module.js\")

or import the entire module into an object:

    (import ((:all -my-module)) \"./my-module.js\")
"
  (let ((modstor (gensym "modstor")))
    `(let ((,modstor (require ,module)))
       ,@(mapcar
          (lambda (name)
            (cond
              ((atom name) `(var ,name (@ ,modstor ,name)))
              ((eq (car name) :all) `(var ,(second name) ,modstor))
              ((eq (car name) :default)
               `(var ,(second name)
                     (if (and ,modstor (@ ,modstor __es-module))
                         (@ ,modstor default)
                         ,modstor)))
              (t `(var ,(second name) (@ ,modstor ,(car name))))))
          names))))


#|
Promises/Callbacks

Promises represent the completion of an asynchronous function. They can be used as an alternative to chaining functions.

// ES5 callback
function doSecond() {
    console.log('Do second.');
}

function doFirst(callback) {
    setTimeout(function() {
        console.log('Do first.');

        callback();
    }, 500);
}

doFirst(doSecond);

// ES6 Promise
let doSecond = () => {
    console.log('Do second.');
}

let doFirst = new Promise((resolve, reject) => {
    setTimeout(() => {
        console.log('Do first.');
        
        resolve();
    }, 500);
});
  
doFirst.then(doSecond);

An example below using XMLHttpRequest, for demonstrative purposes only (Fetch API would be the proper modern API to use).

// ES5 callback
function makeRequest(method, url, callback) {
    var request = new XMLHttpRequest();

    request.open(method, url);
    request.onload = function() {
        callback(null, request.response);
    };
    request.onerror = function() {
        callback(request.response);
    };
    request.send();
}

makeRequest('GET', 'https://url.json', function (err, data) {
        if (err) { 
            throw new Error(err);
        } else {
            console.log(data);
        }
    }
);

// ES6 Promise
function makeRequest(method, url) {
    return new Promise((resolve, reject) => {
        let request = new XMLHttpRequest();

        request.open(method, url);
        request.onload = resolve;
        request.onerror = reject;
        request.send();
    });
}

makeRequest('GET', 'https://url.json')
.then(event => {
    console.log(event.target.response);
})
.catch(err => {
    throw new Error(err);
});


|#
