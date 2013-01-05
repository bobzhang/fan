open Format
open FanAst
type vrn =  
  | TyVrn
  | TyVrnEq
  | TyVrnSup
  | TyVrnInf
  | TyVrnInfSup
  | TyAbstr 
type trail_info = (vrn* int) 
type col =  {
  label: string;
  is_mutable: bool;
  ctyp: ctyp} 
type ty_info = 
  {
  name_expr: expr;
  expr: expr;
  id_expr: expr;
  id_patt: patt;
  id_exprs: expr list;
  id_patts: patt list} 
type record_col =  {
  label: string;
  is_mutable: bool;
  info: ty_info} 
type record_info = record_col list 
type basic_id_transform =
  [ `Pre of string | `Post of string | `Fun of string -> string] 
type rhs_basic_id_transform = [ basic_id_transform | `Exp of string -> expr] 
type full_id_transform =
  [ basic_id_transform | `Idents of ident list -> ident
  | `Ident of ident -> ident | `Last of string -> ident
  | `Obj of string -> string] 
open StdLib
let _ = ()
type named_type = (string* ctyp) 
and and_types = named_type list 
and types = [ `Mutual of and_types | `Single of named_type] 
and module_types = types list 
let rec pp_print_module_types: 'fmt -> module_types -> 'result =
  fun fmt  a0  -> pp_print_list pp_print_types fmt a0
and pp_print_types: 'fmt -> types -> 'result =
  fun fmt  ->
    function
    | `Mutual a0 ->
        Format.fprintf fmt "@[<1>(`Mutual@ %a)@]" pp_print_and_types a0
    | `Single a0 ->
        Format.fprintf fmt "@[<1>(`Single@ %a)@]" pp_print_named_type a0
and pp_print_and_types: 'fmt -> and_types -> 'result =
  fun fmt  a0  -> pp_print_list pp_print_named_type fmt a0
and pp_print_named_type: 'fmt -> named_type -> 'result =
  fun fmt  a0  ->
    (fun fmt  (a0,a1)  ->
       Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_string a0 pp_print_ctyp
         a1) fmt a0
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
    val cons_transform : (string -> string) option
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