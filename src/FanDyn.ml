
open Ast;

{:fans|keep off; derive(DynAst); |};
{:ocaml|{:include| "src/Ast.mli" |}; |};


type dyn;
external dyn_tag : tag 'a -> tag dyn = "%identity";
module Pack(X : sig type 'a t ; end) = struct
  type pack = (tag dyn * Obj.t);
  exception Pack_error;
  let pack tag (v:X.t 'a) = (dyn_tag tag, Obj.repr v);
  let unpack : tag 'a -> pack -> X.t 'a = fun
    tag (tag', obj) -> 
      if dyn_tag tag = tag' then (Obj.obj obj : X.t 'a) else raise Pack_error;
  let print_tag : Format.formatter -> pack -> unit = fun
     f (tag, _) ->
      Format.pp_print_string f (string_of_tag tag);
end;
