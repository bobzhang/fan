open Ast

let _ = (); ()

let _ = ()

type 'a tag =  
  | Literal
  | Rec_flag
  | Direction_flag
  | Mutable_flag
  | Private_flag
  | Virtual_flag
  | Override_flag
  | Row_var_flag
  | Position_flag
  | Strings
  | Alident
  | Auident
  | Aident
  | Astring
  | Uident
  | Ident
  | Ident'
  | Vid
  | Vid'
  | Dupath
  | Dlpath
  | Any
  | Ctyp
  | Type_parameters
  | Row_field
  | Tag_names
  | Typedecl
  | Type_constr
  | Opt_type_constr
  | Decl_param
  | Decl_params
  | Opt_decl_params
  | Type_info
  | Type_repr
  | Name_ctyp
  | Or_ctyp
  | Of_ctyp
  | Pat
  | Rec_pat
  | Exp
  | Rec_exp
  | Module_type
  | Sig_item
  | With_constr
  | Binding
  | Module_binding
  | Case
  | Module_exp
  | Stru
  | Class_type
  | Class_sig_item
  | Class_exp
  | Cstru
  | Ep
  | Rec_bind 

let string_of_tag =
  function
  | Literal  -> "literal"
  | Rec_flag  -> "rec_flag"
  | Direction_flag  -> "direction_flag"
  | Mutable_flag  -> "mutable_flag"
  | Private_flag  -> "private_flag"
  | Virtual_flag  -> "virtual_flag"
  | Override_flag  -> "override_flag"
  | Row_var_flag  -> "row_var_flag"
  | Position_flag  -> "position_flag"
  | Strings  -> "strings"
  | Alident  -> "alident"
  | Auident  -> "auident"
  | Aident  -> "aident"
  | Astring  -> "astring"
  | Uident  -> "uident"
  | Ident  -> "ident"
  | Ident'  -> "ident'"
  | Vid  -> "vid"
  | Vid'  -> "vid'"
  | Dupath  -> "dupath"
  | Dlpath  -> "dlpath"
  | Any  -> "any"
  | Ctyp  -> "ctyp"
  | Type_parameters  -> "type_parameters"
  | Row_field  -> "row_field"
  | Tag_names  -> "tag_names"
  | Typedecl  -> "typedecl"
  | Type_constr  -> "type_constr"
  | Opt_type_constr  -> "opt_type_constr"
  | Decl_param  -> "decl_param"
  | Decl_params  -> "decl_params"
  | Opt_decl_params  -> "opt_decl_params"
  | Type_info  -> "type_info"
  | Type_repr  -> "type_repr"
  | Name_ctyp  -> "name_ctyp"
  | Or_ctyp  -> "or_ctyp"
  | Of_ctyp  -> "of_ctyp"
  | Pat  -> "pat"
  | Rec_pat  -> "rec_pat"
  | Exp  -> "exp"
  | Rec_exp  -> "rec_exp"
  | Module_type  -> "module_type"
  | Sig_item  -> "sig_item"
  | With_constr  -> "with_constr"
  | Binding  -> "binding"
  | Module_binding  -> "module_binding"
  | Case  -> "case"
  | Module_exp  -> "module_exp"
  | Stru  -> "stru"
  | Class_type  -> "class_type"
  | Class_sig_item  -> "class_sig_item"
  | Class_exp  -> "class_exp"
  | Cstru  -> "cstru"
  | Ep  -> "ep"
  | Rec_bind  -> "rec_bind"

let literal_tag: literal tag = Literal

let rec_flag_tag: rec_flag tag = Rec_flag

let direction_flag_tag: direction_flag tag = Direction_flag

let mutable_flag_tag: mutable_flag tag = Mutable_flag

let private_flag_tag: private_flag tag = Private_flag

let virtual_flag_tag: virtual_flag tag = Virtual_flag

let override_flag_tag: override_flag tag = Override_flag

let row_var_flag_tag: row_var_flag tag = Row_var_flag

let position_flag_tag: position_flag tag = Position_flag

let strings_tag: strings tag = Strings

let alident_tag: alident tag = Alident

let auident_tag: auident tag = Auident

let aident_tag: aident tag = Aident

let astring_tag: astring tag = Astring

let uident_tag: uident tag = Uident

let ident_tag: ident tag = Ident

let ident'_tag: ident' tag = Ident'

let vid_tag: vid tag = Vid

let vid'_tag: vid' tag = Vid'

let dupath_tag: dupath tag = Dupath

let dlpath_tag: dlpath tag = Dlpath

let any_tag: any tag = Any

let ctyp_tag: ctyp tag = Ctyp

let type_parameters_tag: type_parameters tag = Type_parameters

let row_field_tag: row_field tag = Row_field

let tag_names_tag: tag_names tag = Tag_names

let typedecl_tag: typedecl tag = Typedecl

let type_constr_tag: type_constr tag = Type_constr

let opt_type_constr_tag: opt_type_constr tag = Opt_type_constr

let decl_param_tag: decl_param tag = Decl_param

let decl_params_tag: decl_params tag = Decl_params

let opt_decl_params_tag: opt_decl_params tag = Opt_decl_params

let type_info_tag: type_info tag = Type_info

let type_repr_tag: type_repr tag = Type_repr

let name_ctyp_tag: name_ctyp tag = Name_ctyp

let or_ctyp_tag: or_ctyp tag = Or_ctyp

let of_ctyp_tag: of_ctyp tag = Of_ctyp

let pat_tag: pat tag = Pat

let rec_pat_tag: rec_pat tag = Rec_pat

let exp_tag: exp tag = Exp

let rec_exp_tag: rec_exp tag = Rec_exp

let module_type_tag: module_type tag = Module_type

let sig_item_tag: sig_item tag = Sig_item

let with_constr_tag: with_constr tag = With_constr

let binding_tag: binding tag = Binding

let module_binding_tag: module_binding tag = Module_binding

let case_tag: case tag = Case

let module_exp_tag: module_exp tag = Module_exp

let stru_tag: stru tag = Stru

let class_type_tag: class_type tag = Class_type

let class_sig_item_tag: class_sig_item tag = Class_sig_item

let class_exp_tag: class_exp tag = Class_exp

let cstru_tag: cstru tag = Cstru

let ep_tag: ep tag = Ep

let rec_bind_tag: rec_bind tag = Rec_bind

type dyn  

external dyn_tag : 'a tag -> dyn tag = "%identity"

module Pack(X:sig type 'a t   end) =
  struct
    type pack = (dyn tag * Obj.t) 
    exception Pack_error
    let pack tag (v : 'a X.t) = ((dyn_tag tag), (Obj.repr v))
    let unpack: 'a tag -> pack -> 'a X.t =
      fun tag  (tag',obj)  ->
        if (dyn_tag tag) = tag'
        then (Obj.obj obj : 'a X.t )
        else raise Pack_error
    let print_tag: Format.formatter -> pack -> unit =
      fun f  (tag,_)  -> Format.pp_print_string f (string_of_tag tag)
  end