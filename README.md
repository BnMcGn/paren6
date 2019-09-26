# Paren6
### Ben McGunigle <bnmcgn@gmail.com>

Paren6 is a set of ES6 macros for Parenscript.

Many modern Javascript examples are given in ES6 code. While almost 
everything that ES6 does can be replicated in standard ES5, considerable 
boilerplate is often needed to get it done. This library is designed to ease 
the task of transcribing ES6 code into Parenscript, either by providing 
ES6-like macros or by documenting existing features of Parenscript that 
fill the desired function.

Paren6, like Parenscript, outputs ES5 compatible code.

## License

Apache License, version 2.0

# Documentation

## Variable declaration

Paren6 does not have a direct analogue to ES6 let.

Parenscript's lisp-style let takes the following form:

    (let ((var-a "value")
          (var-b 3))
      ... <code that uses the variables>)
      
## Constant declaration - defconstant6

As per ES6, the variable cannot be redeclared or redefined, but its contents may be mutable.

In parenscript:

    (defconstant6 *constant* 0)
    
In ES6:

    const CONSTANT = 0;
    

## Arrow functions

The parenscript equivalent of the ES6 arrow operator. => is different from lambda in two ways. It doesn't have its own copy of 'this' and when invoked with a single parameter, that parameter doesn't need to be enclosed in parentheses.

In contrast to ES6 arrow functions, the paren6 version follows lisp syntax rules, placing the => symbol first.

Parenscript:

    (=> x (expt x 2))
    
ES6:
    
    x => x**2
    
## String interpolation

Not implemented

## Multi-line strings

Parenscript has native support for multi-line strings.

## Implicit returns

Parenscript, as a lisp, supports implicit returns by default.

## Spread syntax

Spread syntax, where implemented in paren6, is indicated with the `:...` keyword.

## Shorthand in object definitions - create6

The create6 macro implements matching shorthand to that of ES6. Due to basic differences between lisp and javascript syntax, the form of the shorthand diverges somewhat from ES6. This is most pronounced in same name support. Create6 implements it by assuming that any symbol found at the top level of the macro is meant to refer to a variable of the same name. Non same name pairs must be placed in parentheses.

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
     get: e} 

## Spread syntax in lists - list6

List6 creates lists much like the regular list macro, but adds the `:...` spread syntax operator, allowing other lists to be spread into the created list.

    (let ((arr (list 1 2 3)))
      (create6 4 :... arr 5 6 :... arr))

results in

    [4, 1, 2, 3, 5, 6, 1, 2, 3]
    
## Spread syntax in function calls

Not implemented - although apply can be used to similar effect.

## Destructuring

ES6 supports destructuring of objects in this manner:

    var obj = { a: 1, b: 2, c: 3 };
    let {a, b, c} = obj;
    
Similar results can be achieved with parenscript's with-slots macro:

    (with-slots (a b c) obj
      ...)
      
The variables will only be bound within the scope of the macro. Also, any changes to them will be transmitted to the slots in the object.

Parenscript also includes a destructuring-bind macro, which is worth considering if you wish to destructure lists.

## Iteration

For-of is not implemented

Part of its functionality is covered by native lisp tools like dolist and loop.

## Default parameters

Parenscript supports lisp-style default parameters:

    (defun func (x &optional (y 0))
       ...)
       
The parameter `y` will be set to 0 unless the user supplies another value.

## Classes - defclass6

Defclass6 is used to define ES6 style classes. It takes the following form:

    (defclass6 (classname parent)
      (defun constructor () ...)
      (defun method () ...)
      (defstatic static-method () ...)
      (get item () ...)
      (set item (value) ...))

The parent class is optional. If it is provided, then (super ...) is defined inside of the constructor and results in a call to the parent constructor. Bound superclass methods are available under (chain super (methodname ...)). Note that super.methodname style calls will not work.

As in ES6, the method named 'constructor' is recognized as the constructor. Static methods, getters and setters are also available as per the form above. 


## Modules - export, export-default, import

### export

The export macro registers items in the module.exports object so that the current Javascript file can be imported by other files.

The first parameter, a list of symbols, is the set of names to be added to the export list. It will be taken from the environment if no :from or :source parameter is specified. If the symbol list is empty, the entire :from or :source object will have its keys exported. If no :from or :source is specified, then the symbol list can not be empty.

Use the :from keyword to export from another module or submodule. The :source keyword is used to export an object or portions of an object in the current namespace.

Examples:

    (export (a b c) :from "./module.js")
    
    (export (a b c) :source an-object)

Note that paren6 uses CommonJS exports internally. Because CommonJS doesn't have a dedicated slot for default exports, mixing calls to export and export-default within the same module will cause overwriting.

### export-default

The export-default macro replaces the contents of module.exports with the specified item. If the :from parameter names a module, item will be taken from that module. If item is NIL, the whole module will be exported.

### import

Import from a javascript file or library. The second parameter, module, is a string that specifies the source. The first parameter is a list of names to be bound to things from the incoming module.

Import expects that any symbol in the names list can be found in the import. The item will be bound to the same name in the current environment. If you wish to bind something to an alternate name, place the name in parentheses, followed by the alternate name.

For example:

    (import (a (b x)) "./my-module.js")

will bind the item 'a' from my-module.js to 'a' in the present module, and will bind 'b' to 'x'.

You may also import the default export:

    (import ((:default -my-module)) "./my-module.js")

or import the entire module into an object:

    (import ((:all -my-module)) "./my-module.js")



## Promises/Callbacks

Not implemented

# Testing

Paren6 uses node, mocha and chai in its tests. If you wish to run them:

    >npm install --global mocha
    >npm install --global chai
    
Ensure that the mocha executable is in your search path.
