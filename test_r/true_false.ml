

let _ = (true,false);
(* false; *)
let a = fun
  [ true -> true
  | false -> false];
let b = fun
  [ `true -> true
  | `false -> false];


let a = (`true,`false,`True,`False);

exception True of bool ;

type a = [ True of int ];

let a = fun
  [ true ->  true
  | false as x -> x];
let is_constructor =  fun
  [ true -> assert false ];

let _ = assert false;  
(* assert false; *)
(* (* error*)

 *)  
