



(** Dynamic Ast expansion *)
open Astf

%fans{keep off; derive(DynAst); };;
%ocaml{%include{ "../common/astf.ml" };; };;


type dyn

external dyn_tag : 'a t  ->  dyn t = "%identity"

module Pack(X : sig type 'a t  end) = struct

  type pack = (dyn t  * Obj.t)

  exception Pack_error

  let pack tag (v:'a X.t ) = (dyn_tag tag  , Obj.repr v)

  let unpack : 'a t  -> pack -> 'a X.t  = fun
    tag (tag', obj) -> 
      if dyn_tag tag = tag' then (Obj.obj obj : 'a X.t ) else raise Pack_error
        
  let print_tag : Format.formatter -> pack -> unit = fun
     f (tag, _) ->
      Format.pp_print_string f (to_string tag)
end

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/dyn_tag.cmo" *)
(* end: *)
