  type u = A
  let cmp_u (x,y)= 
    match (x,y) with
    | (A, A) -> true
    |  _ -> false  (* here warning should be get rid of *)

(* local variables: *)
(* compile-command: "ocamlc -c a15.ml" *)
(* end: *)
