
module Make(S:T) = struct
  type t  = int with sexp, bin_io 
end
(*
module Make (S : T) =
  struct
    type t = int
    
    let _ = fun (_ : t) -> ()
      
    let __t_of_sexp__ =
      let _tp_loc = "a_module.ml.Make.t" in fun t -> int_of_sexp t
      
    let _ = __t_of_sexp__
      
    let t_of_sexp sexp =
      try __t_of_sexp__ sexp
      with
      | Sexplib.Conv_error.No_variant_match ((_tp_loc, sexp)) ->
          Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp
      
    let _ = t_of_sexp
      
    let sexp_of_t v = sexp_of_int v
      
    let _ = sexp_of_t
      
  end
*)  

    
