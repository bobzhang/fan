open Ast

type vrn =  
  | Sum
  | TyVrnEq
  | TyVrnSup
  | TyVrnInf
  | TyVrnInfSup
  | TyAbstr 

type trail_info = (vrn * int) 

type col =  {
  col_label: string;
  col_mutable: bool;
  col_ctyp: ctyp} 

type ty_info = 
  {
  name_exp: exp;
  info_exp: exp;
  ep0: ep;
  id_ep: ep;
  id_eps: ep list;
  ty: ctyp} 

type vbranch = [ `variant of (string * ctyp list) | `abbrev of ident] 

type branch = [ `branch of (string * ctyp list)] 

type record_col =  {
  re_label: string;
  re_mutable: bool;
  re_info: ty_info} 

type record_info = record_col list 

type basic_id_transform =
  [ `Pre of string | `Post of string | `Fun of string -> string] 

type rhs_basic_id_transform = [ basic_id_transform | `Exp of string -> exp] 

type full_id_transform =
  [ basic_id_transform | `Idents of vid list -> vid | `Id of vid -> vid
  | `Last of string -> vid | `Obj of string -> string] 

open StdLib

open Objs

let _ = begin (); () end

type named_type = (string * typedecl) 
and and_types = named_type list 
and types = [ `Mutual of and_types | `Single of named_type] 
and mtyps = types list 

type destination =  
  | Obj of kind
  | Str_item 
and kind =  
  | Fold
  | Iter
  | Map
  | Concrete of ctyp 

type warning_type =  
  | Abstract of string
  | Qualified of string 

let rec pp_print_named_type: Format.formatter -> named_type -> unit =
  fun fmt  _a0  ->
    (fun fmt  (_a0,_a1)  ->
       Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_string _a0
         pp_print_typedecl _a1) fmt _a0
and pp_print_and_types: Format.formatter -> and_types -> unit =
  fun fmt  _a0  -> pp_print_list pp_print_named_type fmt _a0
and pp_print_types: Format.formatter -> types -> unit =
  fun fmt  ->
    function
    | `Mutual _a0 ->
        Format.fprintf fmt "@[<1>(`Mutual@ %a)@]" pp_print_and_types _a0
    | `Single _a0 ->
        Format.fprintf fmt "@[<1>(`Single@ %a)@]" pp_print_named_type _a0
and pp_print_mtyps: Format.formatter -> mtyps -> unit =
  fun fmt  _a0  -> pp_print_list pp_print_types fmt _a0

let rec pp_print_destination: Format.formatter -> destination -> unit =
  fun fmt  ->
    function
    | Obj _a0 -> Format.fprintf fmt "@[<1>(Obj@ %a)@]" pp_print_kind _a0
    | Str_item  -> Format.fprintf fmt "Str_item"
and pp_print_kind: Format.formatter -> kind -> unit =
  fun fmt  ->
    function
    | Fold  -> Format.fprintf fmt "Fold"
    | Iter  -> Format.fprintf fmt "Iter"
    | Map  -> Format.fprintf fmt "Map"
    | Concrete _a0 ->
        Format.fprintf fmt "@[<1>(Concrete@ %a)@]" pp_print_ctyp _a0

let pp_print_warning_type: Format.formatter -> warning_type -> unit =
  fun fmt  ->
    function
    | Abstract _a0 ->
        Format.fprintf fmt "@[<1>(Abstract@ %a)@]" pp_print_string _a0
    | Qualified _a0 ->
        Format.fprintf fmt "@[<1>(Qualified@ %a)@]" pp_print_string _a0

type plugin_name = string 

type plugin = 
  {
  transform: mtyps -> stru option;
  position: string option;
  filter: (string -> bool) option} 