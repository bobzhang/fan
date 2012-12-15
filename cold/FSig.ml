open Format
open Ast
type vrn =  
  | TyVrn
  | TyVrnEq
  | TyVrnSup
  | TyVrnInf
  | TyVrnInfSup
  | TyAbstr 
type trail_info = (vrn* int) 
type col =  {
  col_label: string;
  col_mutable: bool;
  col_ctyp: ctyp} 
type ty_info = 
  {
  ty_name_expr: expr;
  ty_expr: expr;
  ty_id_expr: expr;
  ty_id_patt: patt;
  ty_id_exprs: expr list;
  ty_id_patts: patt list} 
type record_col = 
  {
  record_label: string;
  record_mutable: bool;
  record_info: ty_info} 
type record_info = record_col list 
type basic_id_transform =
  [ `Pre of string | `Post of string | `Fun of string -> string] 
type rhs_basic_id_transform = [ basic_id_transform | `Exp of string -> expr] 
type full_id_transform =
  [ basic_id_transform | `Idents of ident list -> ident
  | `Ident of ident -> ident | `Last of string -> ident
  | `Obj of string -> string] 
type named_type = (string* ctyp) 
and and_types = named_type list 
and types = [ `Mutual of and_types | `Single of named_type] 
and module_types = types list 
type obj_dest =  
  | Obj of k
  | Str_item 
and k =  
  | Fold
  | Iter
  | Map 
let preserve = ["self"; "self_type"; "unit"; "result"]
module type Config =
  sig
    val mk_variant : string -> ty_info list -> expr
    val mk_tuple : ty_info list -> expr
    val mk_record : record_info -> expr
    val arity : int
    val left_type_variable : basic_id_transform
    val right_type_variable : rhs_basic_id_transform
    val right_type_id : full_id_transform
    val left_type_id : basic_id_transform
    val trail : trail_info -> match_case
    val names : string list
  end
type warning_type =  
  | Abstract of string
  | Qualified of string 
let string_of_warning_type =
  sprintf "Warning: %a\n"
    (fun _  ->
       function
       | Abstract s -> "Abstract: " ^ s
       | Qualified s -> "Qualified: " ^ s)