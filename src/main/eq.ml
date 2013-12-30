(** we don't use this utility, it exist here only for testing purpose...
 *)

(*
  %extend@Locf{
  let eq_t (x:)
  }
 *)
module Locf = struct
  include Locf
  let eq_t (_x:Locf.t) (_y:Locf.t) = true 
end


module Tokenf = struct
  include Tokenf
  let eq_ant (_x:Tokenf.ant) (_y:Tokenf.ant) = true
end


open StdFan
  (* for [eq_string] -- we need  a built in library for each plugin  *)
open Astf
;;

%fans{
  keep off;
  derive(Eq);};;


%ocaml{%include{ "../common/astf.mli"};;  };;

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/eq.cmo" *)
(* end: *)
