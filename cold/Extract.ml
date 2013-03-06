open LibUtil
open Types
let _loc = FanLoc.ghost
let rec signature_item (x : Types.signature_item) =
  match x with
  | Sig_value _|Sig_exception _|Sig_module _|Sig_class _|Sig_modtype _
    |Sig_class_type _ -> `Nil _loc
  | Sig_type (id,_,_) when Btype.is_row_name (Ident.name id) -> `Nil _loc
  | Sig_type (id,td,_rs) -> type_declaration id td
and type_declaration id
  { type_params; type_kind; type_private; type_manifest;
    type_newtype_level;_}
  =
  (let name = Ident.name id in
   let params = List.map type_expr type_params in
   let private_flag =
     match type_private with
     | Asttypes.Private  -> `Private _loc
     | Asttypes.Public  -> `Public _loc in
   let manifest = Option.map type_expr type_manifest in
   match (type_kind, manifest) with
   | (Type_abstract ,None ) ->
       `TyDcl (_loc, (`Lid (_loc, name)), params, (`Nil _loc), [])
   | (Type_abstract ,Some x)|(Type_record _,x)|(Type_variant _,x) ->
       failwithf "not" : Ast.typedecl )
and type_expr ({ desc;_} : Types.type_expr) =
  match desc with
  | Tvar _|Tarrow _|Ttuple _|Tconstr _|Tfield _|Tnil |Tlink _|Tobject _
    |Tsubst _|Tvariant _|Tunivar _|Tpoly _|Tpackage _ -> failwith "not"
let signature (sg : Types.signature) = List.map signature_item sg