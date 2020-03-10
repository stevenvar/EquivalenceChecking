# EquivalenceChecking
Prototype for equivalence checking of two ML programs 


This checks if two programs are syntactically the same after inlining functions which bodies look different in both programs.
Using this, we can detect renamings, currying, swapping of parameters, generalisation (adding a parameter), eta-conversion...

Functions absent in either programs are also inlined, which allows checking for extraction of code or manual inlining of functions. 

Some alpha-conversion is done on functions in order not to fail when arguments name are simply changed (e.g following the semantics of the language, fun x -> x and fun y -> y are seen as the same ...)

Removal of "let ... in" construct is also done beforehand in order to avoid name capture, for example :

```ocaml
let f x = x 

let g y = 
   let f x = x * 42 in 
   f 2
```

is changed to 

```ocaml
let f x = x 

let g y =  
   (fun x -> x * 42) 2
```

before inlining any function.


# Compile and run

```
   dune build
```

```
  _build/default/comparing.exe tests/fibo_before.ml tests/fibo_after.ml
```
