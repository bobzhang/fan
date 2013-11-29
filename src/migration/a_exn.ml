TYPE_CONV_PATH "B_exn"
exception V of int with sexp
(* Sexplib.Exn_magic.register1 (fun v1 -> V v1) "a_exn.ml.V" sexp_of_int *)
let () = Sexplib.Exn_magic.register1 (fun v1 -> V v1) "B_exn.V" sexp_of_int

(* let () = Sexplib.Exn_magic.register1 (fun v1 -> V v1) "A_exn.V" sexp_of_int   *)
(* let () = *)
(*   Sexplib.Exn_magic.register1 (fun v1 -> V v1) "a_exn.ml.V" sexp_of_int *)
    
