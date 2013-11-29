type u = int with sexp


  
type u = int

let _ = fun (_ : u) -> ()
  
let __u_of_sexp__ = let _tp_loc = "a.ml.u" in fun t -> int_of_sexp t
  
let _ = __u_of_sexp__
  
let u_of_sexp sexp =
  try __u_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((_tp_loc, sexp)) ->
      Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp
  
let _ = u_of_sexp
  
let sexp_of_u v = sexp_of_int v
  
let _ = sexp_of_u
  

  

