



(** Dynamic Ast expansion *)
open FAst

{:fans|keep off; derive(DynAst); |};;
{:ocaml|{:include| "common/fAst.mli" |} |};;


type dyn

external dyn_tag : 'a tag  ->  dyn tag = "%identity"
module Pack(X : sig type 'a t  end) = struct
  type pack = (dyn tag  * Obj.t)

  exception Pack_error

  let pack tag (v:'a X.t ) = (dyn_tag tag  , Obj.repr v)

  let unpack : 'a tag  -> pack -> 'a X.t  = fun
    tag (tag', obj) -> 
      if dyn_tag tag = tag' then (Obj.obj obj : 'a X.t ) else raise Pack_error
        
  let print_tag : Format.formatter -> pack -> unit = fun
     f (tag, _) ->
      Format.pp_print_string f (string_of_tag tag)
end
