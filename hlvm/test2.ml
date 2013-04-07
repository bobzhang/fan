(* Compilation :
   ocamlc -pp "camlp4o pa_hlvm.cmo" -c -dtypes test2.ml -c *)

(** Test programs for HLVM. *)

open Printf
open Hlvm
open Expr

(** Integer Fibonacci benchmark *)
let fib ns : Hlvm.t list = <:phrase<
  let fib (n : int) : int =
    if n > 1 then fib(n-2) + fib(n-1) else n
 >> ::
 List.map
 (fun n -> <:phrase< do printf("\nInteger Fibonacci function: fib(%d)\n", $int:n$)
                      ; fib($int:n$) >>)
 ns

(** Float-based Fibonacci benchmark *)
let ffib ns =
  <:phrase<
    let ffib n =
      if n > 1. then ffib(n - 2.) + ffib(n - 1.) else n >> ::
    List.map
    (fun n -> <:phrase<
     do printf("\nFloating-point Fibonacci function: fib(%f)\n", $flo:n$)
      ; ffib($flo:n$) >>)
    ns

let fill ty =
  <:phrase< let fill (a : $ty$ array, x : $ty$, i : int) : unit =
    if i < length(a)
    then do a[i] <- x
          ; fill(a, x, i+1)
    else () >>

(** Sieve of Eratosthenes. *)
let sieve is : Hlvm.t list = <:prog<
  $fill `Bool$ ;;
  let last (a : bool array, i : int) : int =
    if a[i] then i else last(a, i-1) ;;
  let loop2 (a : bool array, i : int, di : int) : unit =
    if i >= length(a) then ()
    else do a[i] <- false
          ; loop2(a, i+di, di) ;;
  let loop1 (a : bool array, i : int) : unit =
    if i = length(a) then ()
    else do if a[i] then loop2(a, 2*i, i) else ()
          ; loop1(a, i+1) >> @
    List.map
    (fun i -> <:phrase< 
     do let a = alloc($int:i$, false) in
       do printf("\nSieve of Eratosthenes\n")
        ; fill(a, true, 0)
        ; loop1(a, 2)
        ; last(a, length(a)-1) >> )
    is

(** Render the Mandelbrot set with inlined complex arithmetic. *)
let mandelbrot ns = <:prog<
  let pixel (n : int, zr, zi, cr, ci) : unit =
    if n = 65536 then printf(" ")
    else if zr * zr + zi * zi >= 4. then printf(".")
    else pixel(n+1, zr * zr - zi * zi + cr, 2. * zr * zi + ci, cr, ci)  ;;
    
  let row (i : int, j : int, n : int) : unit =
    if i > n then ()
    else do pixel(0, 0., 0., 2. * float(i) / float(n) - 1.5, 2. * float(j) / float(n) - 1.)
          ; row(i+1, j, n) ;;

  let col (j : int, n : int) : unit =
    if j > n then ()
    else do row(0, j, n)
          ; printf("\n")
          ; col(j+1, n) >> @
    List.map
    (fun n -> <:phrase<
       do printf("\nMandelbrot with inline complex arithmetic\n")
        ; col(0, $int:n$) >>)
    ns

(** Test tail call elimination by passing one function as an argument to
    another higher-order function that calls it in tail position, mutually
    recursively. *)
let tco n = <:prog<
  let even (odd : (int) -> int, n : int) : int = odd(n+1) ;;
  let odd (n : int) : int = if n < $int:n$ then even(odd, n+1) else n ;;
  do printf("\nTesting TCO across a higher-order function\n")
   ; even(odd, 0)
>>

(** Test HLVM's struct representation of tuples. *)
let tuples = <:prog<
  let id (s : {float; int}) : {float; int} = s ;;
  let id2 (s : {float; int}) : {float; int} = id(s) ;;
  let rev (s : {int; float}) : {float; int} = id({s.1;s.0}) ;;
  do printf("\nTesting structs (should give (3.4, 2))\n")
   ; rev({2; 3.4})
>>

(** Test the FFI by calling some trig functions from libc. *)
let trig =
  let triple = `Struct[`Float; `Float; `Float] in
  <:prog<
    extern sin : (float) -> float ;;
    extern cos : (float) -> float ;;
    let test (f : (float) -> float) : $triple$ =
      {f(0.1); f(0.2); f(0.3)} ;;
    do printf("\nTesting FFI\n")
     ; test(sin)
     ; test(cos) >>

(** Main program. *)
let () =
  let defs =
    fib [10; 40] @
    ffib [10.0; 40.0] @
    sieve [1000; 10] @
    mandelbrot [1; 77] @
    tco 1000001 @
    tuples @
    trig @
      [] in
  List.iter Hlvm.eval defs;
  Hlvm.save()
