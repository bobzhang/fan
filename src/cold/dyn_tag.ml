open FAst
type 'a t =  
  | Literal
  | Flag
  | Position_flag
  | Strings
  | Lident
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
  | Mtyp
  | Sigi
  | Mbind
  | Constr
  | Bind
  | Case
  | Mexp
  | Stru
  | Cltdecl
  | Cltyp
  | Clsigi
  | Cldecl
  | Clexp
  | Clfield
  | Ep
  | Rec_bind 
let of_string =
  function
  | Literal  -> "literal"
  | Flag  -> "flag"
  | Position_flag  -> "position_flag"
  | Strings  -> "strings"
  | Lident  -> "lident"
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
  | Mtyp  -> "mtyp"
  | Sigi  -> "sigi"
  | Mbind  -> "mbind"
  | Constr  -> "constr"
  | Bind  -> "bind"
  | Case  -> "case"
  | Mexp  -> "mexp"
  | Stru  -> "stru"
  | Cltdecl  -> "cltdecl"
  | Cltyp  -> "cltyp"
  | Clsigi  -> "clsigi"
  | Cldecl  -> "cldecl"
  | Clexp  -> "clexp"
  | Clfield  -> "clfield"
  | Ep  -> "ep"
  | Rec_bind  -> "rec_bind"
let literal: literal t = Literal
let flag: flag t = Flag
let position_flag: position_flag t = Position_flag
let strings: strings t = Strings
let lident: lident t = Lident
let alident: alident t = Alident
let auident: auident t = Auident
let aident: aident t = Aident
let astring: astring t = Astring
let uident: uident t = Uident
let ident: ident t = Ident
let ident': ident' t = Ident'
let vid: vid t = Vid
let vid': vid' t = Vid'
let dupath: dupath t = Dupath
let dlpath: dlpath t = Dlpath
let any: any t = Any
let ctyp: ctyp t = Ctyp
let type_parameters: type_parameters t = Type_parameters
let row_field: row_field t = Row_field
let tag_names: tag_names t = Tag_names
let typedecl: typedecl t = Typedecl
let type_constr: type_constr t = Type_constr
let opt_type_constr: opt_type_constr t = Opt_type_constr
let decl_param: decl_param t = Decl_param
let decl_params: decl_params t = Decl_params
let opt_decl_params: opt_decl_params t = Opt_decl_params
let type_info: type_info t = Type_info
let type_repr: type_repr t = Type_repr
let name_ctyp: name_ctyp t = Name_ctyp
let or_ctyp: or_ctyp t = Or_ctyp
let of_ctyp: of_ctyp t = Of_ctyp
let pat: pat t = Pat
let rec_pat: rec_pat t = Rec_pat
let exp: exp t = Exp
let rec_exp: rec_exp t = Rec_exp
let mtyp: mtyp t = Mtyp
let sigi: sigi t = Sigi
let mbind: mbind t = Mbind
let constr: constr t = Constr
let bind: bind t = Bind
let case: case t = Case
let mexp: mexp t = Mexp
let stru: stru t = Stru
let cltdecl: cltdecl t = Cltdecl
let cltyp: cltyp t = Cltyp
let clsigi: clsigi t = Clsigi
let cldecl: cldecl t = Cldecl
let clexp: clexp t = Clexp
let clfield: clfield t = Clfield
let ep: ep t = Ep
let rec_bind: rec_bind t = Rec_bind
type dyn  
external dyn_tag : 'a t -> dyn t = "%identity"
module Pack(X:sig type 'a t   end) =
  struct
    type pack = (dyn t* Obj.t) 
    exception Pack_error
    let pack tag (v : 'a X.t) = ((dyn_tag tag), (Obj.repr v))
    let unpack: 'a t -> pack -> 'a X.t =
      fun tag  (tag',obj)  ->
        if (dyn_tag tag) = tag'
        then (Obj.obj obj : 'a X.t )
        else raise Pack_error
    let print_tag: Format.formatter -> pack -> unit =
      fun f  (tag,_)  -> Format.pp_print_string f (of_string tag)
  end