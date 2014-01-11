
open Astfn
module type S = sig
  val pp_print_loc : Locf.t Formatf.t 
end

module Make(U:S) = struct
  open U
  %fans{keep off;
      derive (Print (* OPrint *) PrintWrapper);
    };;
   
   %ocaml{ %include{ "astfn.ml" };; };;
end;;

include Make(struct
  let pp_print_loc _ _ = ()
end)

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/astfn_print.cmo" *)
(* end: *)
