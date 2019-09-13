# Paren6
### Ben McGunigle <bnmcgn@gmail.com>

Paren6 is a set of ES6 macros for Parenscript.

Many modern Javascript examples are given in ES6 code. While almost 
everything that ES6 does can be replicated in standard ES5, it often takes 
considerable boilerplate to get it done. This library is designed to ease 
the task of transcribing ES6 code into Parenscript, either by providing 
ES6-like macros or by documenting existing features of Parenscript that 
fill the desired function.

Paren6, like Parenscript, outputs ES5 compatible code.

## License

Specify license here



## Variable declaration

ES6 introduced the let keyword, which allows for block-scoped variables which cannot be hoisted or redeclared.

// ES5
var x = 0;

// ES6
let x = 0;




## Constant declaration

ES6 introduced the const keyword, which cannot be redeclared or reassigned, but is not immutable.

;;Defconstant?

(defpsmacro const6 (name value)
  "As per ES6, the variable cannot be redeclared or redefined, but its contents may be mutable.
This version may not work in block scope."
  `(chain -object
          (define-property
              (if (eql (typeof global) "object") global window)
              ,name
            (create :value ,value :enumerable t :writable false :configurable false))))

// ES6
const CONST_IDENTIFIER = 0; // constants are uppercase by convention



## Arrow functions

The arrow function expression syntax is a shorter way of creating a function expression. Arrow functions do not have their own this, do not have prototypes, cannot be used for constructors, and should not be used as object methods.


(defpsmacro => (params &body body)
  (let ((params (if (listp params) params (list params))))
    `(chain (lambda ,params ,@body) (bind this))))

## Template literals
Concatenation/string interpolation

;; Parenscript has (format)? Don't need. Also should be done at parenscript level.

    MDN Reference: Expression interpolation

## Multi-line strings

;; Parenscript has native support.


## Implicit returns

;; Paren has already



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

## Key/property shorthand

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


## Method definition shorthand

The function keyword can be omitted when assigning methods on an object.


// ES6
let obj = {
    a(c, d) {},
    b(e, f) {}
}



## Destructuring

Use curly brackets to assign properties of an object to their own variable.

var obj = { a: 1, b: 2, c: 3 };

// ES6
let {a, b, c} = obj;


;; Use dolist in place of for-of. We also have for-in.

Array iteration (looping)

// ES6
for (let i of arr) {
    console.log(i);
}

## Default parameters

;; Parenscript already supports default parameters



"List6 creates lists much like the regular list macro, but adds the `:...` spread syntax operator, allowing other lists to be spread into the created list.

    (let ((arr (list 1 2 3)))
      (create6 4 :... arr 5 6 :... arr))

results in

    [4, 1, 2, 3, 5, 6, 1, 2, 3]''"

;; For function arguments: we have apply. Otherwise, this might need to be implemented
;; at the parenscript level.

## Spread syntax


Spread syntax can be used for function arguments.

// ES6
let arr1 = [1, 2, 3];
let func = (a, b, c) => a + b + c;

console.log(func(...arr1);); // 6 


## Classes/constructor functions

;;FIXME: add setter/getter and super support
;;FIXME: check over "extends" expression support.

(defpsmacro defclass6 ((name &optional extends) &body body)



## Modules - export/import

(defpsmacro export ((&rest symbol-list) &key from source)

(defpsmacro export-default (item &key from)

(defpsmacro import ((&rest names) module)



Modules can be created to export and import code between files.

<!-- index.html -->
<script src="export.js"></script>
<script type="module" src="import.js"></script>

// export.js
let func = a => a + a;
let obj = {};
let x = 0;

export { func, obj, x };

// import.js
import { func, obj, x } from './export.js';

console.log(func(3), obj, x);

    MDN Reference: export
    MDN Reference: import

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
