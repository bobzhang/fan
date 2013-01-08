open LibUtil
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
  exp0: expr;
  pat0: patt;
  id_expr: expr;
  id_patt: patt;
  id_exprs: expr list;
  id_patts: patt list} 
type vbranch = [ `variant of (string* ctyp list) | `abbrev of ctyp] 
type branch = [ `branch of (string* ctyp list)] 
type record_col =  {
  label: string;
  is_mutable: bool;
  info: ty_info} 
type record_info = record_col list 
type basic_id_transform =
  [ `Pre of string | `Post of string | `Fun of string id] 
type rhs_basic_id_transform = [ basic_id_transform | `Exp of string -> expr] 
type full_id_transform =
  [ basic_id_transform | `Idents of ident list -> ident
  | `Ident of ident -> ident | `Last of string -> ident | `Obj of string id] 
open StdLib
let _ = ()
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
type warning_type =  
  | Abstract of string
  | Qualified of string 
let rec pp_print_named_type: 'fmt -> named_type -> 'result =
  fun fmt  a0  ->
    (fun fmt  (a0,a1)  ->
       Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_string a0 pp_print_ctyp
         a1) fmt a0
and pp_print_and_types: 'fmt -> and_types -> 'result =
  fun fmt  a0  -> pp_print_list pp_print_named_type fmt a0
and pp_print_types: 'fmt -> types -> 'result =
  fun fmt  ->
    function
    | `Mutual a0 ->
        Format.fprintf fmt "@[<1>(`Mutual@ %a)@]" pp_print_and_types a0
    | `Single a0 ->
        Format.fprintf fmt "@[<1>(`Single@ %a)@]" pp_print_named_type a0
and pp_print_module_types: 'fmt -> module_types -> 'result =
  fun fmt  a0  -> pp_print_list pp_print_types fmt a0
let rec pp_print_obj_dest: 'fmt -> obj_dest -> 'result =
  fun fmt  ->
    function
    | Obj a0 -> Format.fprintf fmt "@[<1>(Obj@ %a)@]" pp_print_k a0
    | Str_item  -> Format.fprintf fmt "Str_item"
and pp_print_k: 'fmt -> k -> 'result =
  fun fmt  ->
    function
    | Fold  -> Format.fprintf fmt "Fold"
    | Iter  -> Format.fprintf fmt "Iter"
    | Map  -> Format.fprintf fmt "Map"
let pp_print_warning_type: 'fmt -> warning_type -> 'result =
  fun fmt  ->
    function
    | Abstract a0 ->
        Format.fprintf fmt "@[<1>(Abstract@ %a)@]" pp_print_string a0
    | Qualified a0 ->
        Format.fprintf fmt "@[<1>(Qualified@ %a)@]" pp_print_string a0