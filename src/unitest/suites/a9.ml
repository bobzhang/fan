
let f x = 
  try x with
    `F g  -> raise c
        (* This should be a syntax error *)
        

(* local variables: *)
(* compile-command: "ocamlc -c a9.ml" *)
(* end: *)
