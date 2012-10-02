(* open Format; *)
open Parsetree;

let mktyp loc d = {ptyp_desc = d; ptyp_loc =  loc};



(*
  
 *)
let mkpoly = fun
  [{ptyp_desc=Ptyp_poly _ _ ; _} as t -> t
  | x  -> {(x) with ptyp_desc = Ptyp_poly [] x}];







