

let f u v = %cexp@option{
  let! x = u 
  and y = v in
  Some (x + y  )
 }

let x = 3                   
(* let! x = 3 (\* error *\) *)
(* local variables: *)
(* compile-command: "ocamlc -I ../../common   -I ../../cold -I ../../utils -c -pp fan monad.ml" *)
(* end: *)
