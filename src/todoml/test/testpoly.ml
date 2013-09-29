

type u =
    [`A of (int * int) | `B of (int * bool )]
type v = [`B of (int * bool)]

let f = function
  | #v as x ->
      match x with
        `B(x,_)-> x;;
      
let fg = function
  | `A (x,_) | `B(x,_) -> x;;

let fa (x:v) =
  fg x;;

(*
type ab = [ `A | `B ];;

let f (x : [`A]) = match x with #ab -> 1;;
  *)
  


















