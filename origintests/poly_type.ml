

  type u = [ `A of int * bool ];;

(*
  [
  Rtag "A" false
  [
  core_type (origintests/poly_type.ml[3,2+19]..origintests/poly_type.ml[3,2+29])
  Ptyp_tuple
  [
  core_type (origintests/poly_type.ml[3,2+19]..origintests/poly_type.ml[3,2+22])
  Ptyp_constr "int"
  []
  core_type (origintests/poly_type.ml[3,2+25]..origintests/poly_type.ml[3,2+29])
  Ptyp_constr "bool"
  []  ]  ]  ]
 *)


(* {:row_field|$vrn:x of $par:y|} *)
