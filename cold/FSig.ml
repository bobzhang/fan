open LibUtil
open FanAst
type vrn =  
  | Sum
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
  id_patts: patt list;
  ty: ctyp} 
type vbranch = [ `variant of (string* ctyp list) | `abbrev of ident] 
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
type destination =  
  | Obj of kind
  | Str_item 
and kind =  
  | Fold
  | Iter
  | Map 
type warning_type =  
  | Abstract of string
  | Qualified of string 
let rec pp_print_named_type fmt _a0 =
  (fun fmt  (_a0,_a1)  ->
     Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_string _a0 pp_print_ctyp
       _a1) fmt _a0
and pp_print_and_types fmt _a0 = pp_print_list pp_print_named_type fmt _a0
and pp_print_types fmt =
  function
  | `Mutual _a0 ->
      Format.fprintf fmt "@[<1>(`Mutual@ %a)@]" pp_print_and_types _a0
  | `Single _a0 ->
      Format.fprintf fmt "@[<1>(`Single@ %a)@]" pp_print_named_type _a0
and pp_print_module_types fmt _a0 = pp_print_list pp_print_types fmt _a0
let rec pp_print_destination fmt =
  function
  | Obj _a0 -> Format.fprintf fmt "@[<1>(Obj@ %a)@]" pp_print_kind _a0
  | Str_item  -> Format.fprintf fmt "Str_item"
and pp_print_kind fmt =
  function
  | Fold  -> Format.fprintf fmt "Fold"
  | Iter  -> Format.fprintf fmt "Iter"
  | Map  -> Format.fprintf fmt "Map"
let pp_print_warning_type fmt =
  function
  | Abstract _a0 ->
      Format.fprintf fmt "@[<1>(Abstract@ %a)@]" pp_print_string _a0
  | Qualified _a0 ->
      Format.fprintf fmt "@[<1>(Qualified@ %a)@]" pp_print_string _a0
let str_item_of_module_types ~f:(aux : named_type -> ctyp) 
  (x : module_types) =
  (let _loc = FanLoc.ghost in
   sem_of_list
     (List.map
        (function
         | `Mutual tys -> `Type (_loc, (and_of_list (List.map aux tys)))
         | `Single ty -> `Type (_loc, (aux ty))) x) : str_item )