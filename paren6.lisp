;;;; paren6.lisp

;; Add some of the ES6 functionality to parenscript

(in-package #:paren6)


#|
Variable declaration

ES6 introduced the let keyword, which allows for block-scoped variables which cannot be hoisted or redeclared.

// ES5
var x = 0;

// ES6
let x = 0;

    MDN Reference: let


|#

;;Defconstant?

(defpsmacro const6 (name value)
  "As per ES6, the variable cannot be redeclared or redefined, but its contents may be mutable.
This version may not work in block scope."
  `(chain -object
          (define-property
              (if (eql (typeof global) "object") global window)
              ,name
            (create :value ,value :enumerable t :writable false :configurable false))))

#|
Constant declaration

ES6 introduced the const keyword, which cannot be redeclared or reassigned, but is not immutable.

// ES6
const CONST_IDENTIFIER = 0; // constants are uppercase by convention

    MDN Reference: const

|#

(defpsmacro => (params &body body)
  (let ((params (if (listp params) params (list params))))
    `(chain (lambda ,params ,@body) (bind this))))

#|
Arrow functions

The arrow function expression syntax is a shorter way of creating a function expression. Arrow functions do not have their own this, do not have prototypes, cannot be used for constructors, and should not be used as object methods.


Template literals
Concatenation/string interpolation

;; Parenscript has (format)? Don't need. Also should be done at parenscript level.

    MDN Reference: Expression interpolation

Multi-line strings

;; Parenscript has native support.


Implicit returns

;; Paren has already


|#

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

    {a: 1, b: 3, c: 2, d: 5} "
  (let ((stor (gensym))
        (spreadp nil)
        (accum (list)))
    (dolist (itm items-pairlists-and-dotted)
      (if spreadp
          (if (eq itm :...)
              (error "Can't have two :... keywords in a row")
              (progn
                (setf spreadp nil)
                (push `(merge ,stor ,itm) accum)))
          (if (listp itm)
              (push `(merge ,stor (create ,@itm)) accum)
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
       ,stor)))

#|
Key/property shorthand

ES6 introduces a shorter notation for assigning properties to variables of the same name.

// ES5
var obj = { 
    a: a, 
    b: b
}

// ES6
let obj = { 
    a, 
    b
}

    MDN Reference: Property definitions

Method definition shorthand

The function keyword can be omitted when assigning methods on an object.

// ES5
var obj = {
    a: function(c, d) {},
    b: function(e, f) {}
};

// ES6
let obj = {
    a(c, d) {},
    b(e, f) {}
}

obj.a(); // call method a

    MDN Reference: Method definitions

|#

;; Do we have a with-keys macro? Might want a read only version.
;; With-slots will do the job. Ps version works with JS objects.



#|


Destructuring

Use curly brackets to assign properties of an object to their own variable.

var obj = { a: 1, b: 2, c: 3 };

// ES5
var a = obj.a;
var b = obj.b;
var c = obj.c;

// ES6
let {a, b, c} = obj;

    MDN Reference: Object initializer

|#

;; Use dolist in place of for-of. We also have for-in.

#|
Array iteration (looping)

// ES6
for (let i of arr) {
    console.log(i);
}

;; Parenscript already supports default parameters
Default parameters

|#

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

;; For function arguments: we have apply. Otherwise, this might need to be implemented
;; at the parenscript level.

#|
Spread syntax


Spread syntax can be used for function arguments.

// ES6
let arr1 = [1, 2, 3];
let func = (a, b, c) => a + b + c;

console.log(func(...arr1);); // 6

|#

(defun super-wrap (code superclass)
  ;;Only add access to super class constructor if there is a parent class
  (when (some (lambda (x)
                (starts-with-subseq (string x) (string 'super.)))
              (flatten code))
    (warn "Defclass6 doesn't handle super calls of the form super.xxx. Please rewrite as (chain super (xxx ...))"))
  (if superclass
      `(macrolet ((super (&rest params)
                    `(chain ,,superclass (call this ,@params)))
                  (chain (&body body)
                    (if (string-equal (car body) 'super)
                        (if (listp (second body))
                            (destructuring-bind (_ (method &rest params) &rest more) body
                              `(chain (@ ,superclass prototype ,method (call this ,@params) ,@more)))
                            ;; Hope this is what is wanted... could also be ,superclass prototype
                            `(chain ,superclass ,@(cdr body)))
                        `(chain ,@body))))
         ,@code)
      code))

(defun proc-method (classname code)
  (let ((methodname (second code))
        (params (third code))
        (body (cdddr code)))
    `(setf (@ ,classname prototype ,methodname)
          (lambda ,params ,body))))

(defun proc-static (classname code)
  (let ((methodname (second code))
        (params (third code))
        (body (cdddr code)))
    `(setf (@ ,classname ,methodname)
           (lambda ,params ,body))))

;;FIXME: add setter/getter and super support
;;FIXME: check over "extends" expression support.

(defpsmacro defclass6 ((name &optional extends) &body body)
  (let ((constructor nil)
        (methods nil))
    (dolist (itm body)
      (cond
        ((string-equal (second itm) 'constructor)
         (if constructor
             (error "Class can't have more than one constructor")
             (setf constructor itm)))
        ((string-equal (car itm) 'defmethod)
         (push (super-wrap (proc-method name itm) extends) methods))
        ((string-equal (car itm) 'defstatic)
         (push (super-wrap (proc-static name itm) extends) methods))
        (t (error "Defclass6 only allows defmethod or defstatic calls in its body"))))
    (when constructor
      (unless (< 2 (length constructor))
        (error "Constructor needs a parameter list")))
    ;;FIXME: onceonly on name? or ensure symbol
    `(progn
       ,(if constructor
            (super-wrap `(defun ,name ,(third constructor) ,(cdddr constructor)) extends)
            `(defun ,name ()))
       ,@methods
       ,@(when extends
           (list `(setf (@ ,name prototype) (chain -object (create (@ ,extends prototype))))
                 `(setf (@ ,name prototype constructor) ,name))))))

#|


Classes/constructor functions

|#

;;import-as import-from export

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

Import expects that any symbol in the names list can be found in the import. The item will be bound to the same name in the current environment. If you wish to bind something "
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
