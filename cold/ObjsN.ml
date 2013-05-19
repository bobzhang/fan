open StdLib

open AstN

let _ = (); ()

let pp_print_loc: Format.formatter -> loc -> unit =
  fun fmt  _a0  -> FanLoc.pp_print_t fmt _a0

let pp_print_ant: Format.formatter -> ant -> unit =
  fun fmt  (`Ant (_a0,_a1))  ->
    Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" pp_print_loc _a0
      FanUtil.pp_print_anti_cxt _a1

let pp_print_nil: Format.formatter -> nil -> unit =
  fun fmt  `Nil  -> Format.fprintf fmt "`Nil"

let pp_print_literal: Format.formatter -> literal -> unit =
  fun fmt  ->
    function
    | `Chr _a0 -> Format.fprintf fmt "@[<1>(`Chr@ %a)@]" pp_print_string _a0
    | `Int _a0 -> Format.fprintf fmt "@[<1>(`Int@ %a)@]" pp_print_string _a0
    | `Int32 _a0 ->
        Format.fprintf fmt "@[<1>(`Int32@ %a)@]" pp_print_string _a0
    | `Int64 _a0 ->
        Format.fprintf fmt "@[<1>(`Int64@ %a)@]" pp_print_string _a0
    | `Flo _a0 -> Format.fprintf fmt "@[<1>(`Flo@ %a)@]" pp_print_string _a0
    | `Nativeint _a0 ->
        Format.fprintf fmt "@[<1>(`Nativeint@ %a)@]" pp_print_string _a0
    | `Str _a0 -> Format.fprintf fmt "@[<1>(`Str@ %a)@]" pp_print_string _a0

let pp_print_flag: Format.formatter -> flag -> unit =
  fun fmt  ->
    function
    | `Positive -> Format.fprintf fmt "`Positive"
    | `Negative -> Format.fprintf fmt "`Negative"
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)

let pp_print_position_flag: Format.formatter -> position_flag -> unit =
  fun fmt  ->
    function
    | `Positive -> Format.fprintf fmt "`Positive"
    | `Negative -> Format.fprintf fmt "`Negative"
    | `Normal -> Format.fprintf fmt "`Normal"
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)

let rec pp_print_strings: Format.formatter -> strings -> unit =
  fun fmt  ->
    function
    | `App (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" pp_print_strings _a0
          pp_print_strings _a1
    | `Str _a0 -> Format.fprintf fmt "@[<1>(`Str@ %a)@]" pp_print_string _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)

let pp_print_lident: Format.formatter -> lident -> unit =
  fun fmt  (`Lid _a0)  ->
    Format.fprintf fmt "@[<1>(`Lid@ %a)@]" pp_print_string _a0

let pp_print_alident: Format.formatter -> alident -> unit =
  fun fmt  ->
    function
    | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" pp_print_string _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)

let pp_print_auident: Format.formatter -> auident -> unit =
  fun fmt  ->
    function
    | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" pp_print_string _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)

let pp_print_aident: Format.formatter -> aident -> unit =
  fun fmt  ->
    function
    | #alident as _a0 -> (pp_print_alident fmt _a0 :>unit)
    | #auident as _a0 -> (pp_print_auident fmt _a0 :>unit)

let pp_print_astring: Format.formatter -> astring -> unit =
  fun fmt  ->
    function
    | `C _a0 -> Format.fprintf fmt "@[<1>(`C@ %a)@]" pp_print_string _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)

let rec pp_print_uident: Format.formatter -> uident -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" pp_print_uident _a0
          pp_print_uident _a1
    | `App (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" pp_print_uident _a0
          pp_print_uident _a1
    | #auident as _a0 -> (pp_print_auident fmt _a0 :>unit)

let rec pp_print_ident: Format.formatter -> ident -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" pp_print_ident _a0
          pp_print_ident _a1
    | `Apply (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Apply@ %a@ %a)@]" pp_print_ident _a0
          pp_print_ident _a1
    | #alident as _a0 -> (pp_print_alident fmt _a0 :>unit)
    | #auident as _a0 -> (pp_print_auident fmt _a0 :>unit)

let pp_print_ident': Format.formatter -> ident' -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" pp_print_ident _a0
          pp_print_ident _a1
    | `Apply (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Apply@ %a@ %a)@]" pp_print_ident _a0
          pp_print_ident _a1
    | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" pp_print_string _a0
    | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" pp_print_string _a0

let rec pp_print_vid: Format.formatter -> vid -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" pp_print_vid _a0
          pp_print_vid _a1
    | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" pp_print_string _a0
    | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" pp_print_string _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)

let pp_print_vid': Format.formatter -> vid' -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" pp_print_vid _a0
          pp_print_vid _a1
    | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" pp_print_string _a0
    | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" pp_print_string _a0

let rec pp_print_dupath: Format.formatter -> dupath -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" pp_print_dupath _a0
          pp_print_dupath _a1
    | #auident as _a0 -> (pp_print_auident fmt _a0 :>unit)

let pp_print_dlpath: Format.formatter -> dlpath -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" pp_print_dupath _a0
          pp_print_alident _a1
    | #alident as _a0 -> (pp_print_alident fmt _a0 :>unit)

let pp_print_any: Format.formatter -> any -> unit =
  fun fmt  `Any  -> Format.fprintf fmt "`Any"

let rec pp_print_ctyp: Format.formatter -> ctyp -> unit =
  fun fmt  ->
    function
    | `Alias (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Alias@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_alident _a1
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | `App (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_ctyp _a1
    | `Arrow (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Arrow@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_ctyp _a1
    | `ClassPath _a0 ->
        Format.fprintf fmt "@[<1>(`ClassPath@ %a)@]" pp_print_ident _a0
    | `Label (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" pp_print_alident _a0
          pp_print_ctyp _a1
    | `OptLabl (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" pp_print_alident _a0
          pp_print_ctyp _a1
    | #ident' as _a0 -> (pp_print_ident' fmt _a0 :>unit)
    | `TyObj (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyObj@ %a@ %a)@]" pp_print_name_ctyp _a0
          pp_print_flag _a1
    | `TyObjEnd _a0 ->
        Format.fprintf fmt "@[<1>(`TyObjEnd@ %a)@]" pp_print_flag _a0
    | `TyPol (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyPol@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_ctyp _a1
    | `TyPolEnd _a0 ->
        Format.fprintf fmt "@[<1>(`TyPolEnd@ %a)@]" pp_print_ctyp _a0
    | `TyTypePol (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyTypePol@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_ctyp _a1
    | `Quote (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]" pp_print_position_flag
          _a0 pp_print_alident _a1
    | `QuoteAny _a0 ->
        Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]" pp_print_position_flag
          _a0
    | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" pp_print_ctyp _a0
    | `Sta (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sta@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_ctyp _a1
    | `PolyEq _a0 ->
        Format.fprintf fmt "@[<1>(`PolyEq@ %a)@]" pp_print_row_field _a0
    | `PolySup _a0 ->
        Format.fprintf fmt "@[<1>(`PolySup@ %a)@]" pp_print_row_field _a0
    | `PolyInf _a0 ->
        Format.fprintf fmt "@[<1>(`PolyInf@ %a)@]" pp_print_row_field _a0
    | `Com (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_ctyp _a1
    | `PolyInfSup (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`PolyInfSup@ %a@ %a)@]" pp_print_row_field
          _a0 pp_print_tag_names _a1
    | `Package _a0 ->
        Format.fprintf fmt "@[<1>(`Package@ %a)@]" pp_print_mtyp _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_type_parameters: Format.formatter -> type_parameters -> unit =
  fun fmt  ->
    function
    | `Com (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" pp_print_type_parameters
          _a0 pp_print_type_parameters _a1
    | `Ctyp _a0 -> Format.fprintf fmt "@[<1>(`Ctyp@ %a)@]" pp_print_ctyp _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_row_field: Format.formatter -> row_field -> unit =
  fun fmt  ->
    function
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    | `Bar (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Bar@ %a@ %a)@]" pp_print_row_field _a0
          pp_print_row_field _a1
    | `TyVrn _a0 ->
        Format.fprintf fmt "@[<1>(`TyVrn@ %a)@]" pp_print_astring _a0
    | `TyVrnOf (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyVrnOf@ %a@ %a)@]" pp_print_astring _a0
          pp_print_ctyp _a1
    | `Ctyp _a0 -> Format.fprintf fmt "@[<1>(`Ctyp@ %a)@]" pp_print_ctyp _a0
and pp_print_tag_names: Format.formatter -> tag_names -> unit =
  fun fmt  ->
    function
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    | `App (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" pp_print_tag_names _a0
          pp_print_tag_names _a1
    | `TyVrn _a0 ->
        Format.fprintf fmt "@[<1>(`TyVrn@ %a)@]" pp_print_astring _a0
and pp_print_typedecl: Format.formatter -> typedecl -> unit =
  fun fmt  ->
    function
    | `TyDcl (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`TyDcl@ %a@ %a@ %a@ %a)@]" pp_print_alident
          _a0 pp_print_opt_decl_params _a1 pp_print_type_info _a2
          pp_print_opt_type_constr _a3
    | `TyAbstr (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TyAbstr@ %a@ %a@ %a)@]" pp_print_alident
          _a0 pp_print_opt_decl_params _a1 pp_print_opt_type_constr _a2
    | `And (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" pp_print_typedecl _a0
          pp_print_typedecl _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_type_constr: Format.formatter -> type_constr -> unit =
  fun fmt  ->
    function
    | `And (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" pp_print_type_constr _a0
          pp_print_type_constr _a1
    | `Eq (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_ctyp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_opt_type_constr: Format.formatter -> opt_type_constr -> unit =
  fun fmt  ->
    function
    | `Some _a0 ->
        Format.fprintf fmt "@[<1>(`Some@ %a)@]" pp_print_type_constr _a0
    | `None -> Format.fprintf fmt "`None"
and pp_print_decl_param: Format.formatter -> decl_param -> unit =
  fun fmt  ->
    function
    | `Quote (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]" pp_print_position_flag
          _a0 pp_print_alident _a1
    | `QuoteAny _a0 ->
        Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]" pp_print_position_flag
          _a0
    | `Any -> Format.fprintf fmt "`Any"
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_decl_params: Format.formatter -> decl_params -> unit =
  fun fmt  ->
    function
    | `Quote (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]" pp_print_position_flag
          _a0 pp_print_alident _a1
    | `QuoteAny _a0 ->
        Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]" pp_print_position_flag
          _a0
    | `Any -> Format.fprintf fmt "`Any"
    | `Com (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" pp_print_decl_params _a0
          pp_print_decl_params _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_opt_decl_params: Format.formatter -> opt_decl_params -> unit =
  fun fmt  ->
    function
    | `Some _a0 ->
        Format.fprintf fmt "@[<1>(`Some@ %a)@]" pp_print_decl_params _a0
    | `None -> Format.fprintf fmt "`None"
and pp_print_type_info: Format.formatter -> type_info -> unit =
  fun fmt  ->
    function
    | `TyMan (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TyMan@ %a@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_flag _a1 pp_print_type_repr _a2
    | `TyRepr (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyRepr@ %a@ %a)@]" pp_print_flag _a0
          pp_print_type_repr _a1
    | `TyEq (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyEq@ %a@ %a)@]" pp_print_flag _a0
          pp_print_ctyp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_type_repr: Format.formatter -> type_repr -> unit =
  fun fmt  ->
    function
    | `Record _a0 ->
        Format.fprintf fmt "@[<1>(`Record@ %a)@]" pp_print_name_ctyp _a0
    | `Sum _a0 -> Format.fprintf fmt "@[<1>(`Sum@ %a)@]" pp_print_or_ctyp _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_name_ctyp: Format.formatter -> name_ctyp -> unit =
  fun fmt  ->
    function
    | `Sem (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_name_ctyp _a0
          pp_print_name_ctyp _a1
    | `TyCol (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a)@]" pp_print_alident _a0
          pp_print_ctyp _a1
    | `TyColMut (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyColMut@ %a@ %a)@]" pp_print_alident _a0
          pp_print_ctyp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_or_ctyp: Format.formatter -> or_ctyp -> unit =
  fun fmt  ->
    function
    | `Bar (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Bar@ %a@ %a)@]" pp_print_or_ctyp _a0
          pp_print_or_ctyp _a1
    | `TyCol (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a)@]" pp_print_auident _a0
          pp_print_ctyp _a1
    | `Of (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Of@ %a@ %a)@]" pp_print_auident _a0
          pp_print_ctyp _a1
    | #auident as _a0 -> (pp_print_auident fmt _a0 :>unit)
and pp_print_of_ctyp: Format.formatter -> of_ctyp -> unit =
  fun fmt  ->
    function
    | `Of (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Of@ %a@ %a)@]" pp_print_vid _a0
          pp_print_ctyp _a1
    | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>unit)
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_pat: Format.formatter -> pat -> unit =
  fun fmt  ->
    function
    | #vid as _a0 -> (pp_print_vid fmt _a0 :>unit)
    | `App (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" pp_print_pat _a0
          pp_print_pat _a1
    | `Vrn _a0 -> Format.fprintf fmt "@[<1>(`Vrn@ %a)@]" pp_print_string _a0
    | `Com (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" pp_print_pat _a0
          pp_print_pat _a1
    | `Sem (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_pat _a0
          pp_print_pat _a1
    | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" pp_print_pat _a0
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | `Record _a0 ->
        Format.fprintf fmt "@[<1>(`Record@ %a)@]" pp_print_rec_pat _a0
    | #literal as _a0 -> (pp_print_literal fmt _a0 :>unit)
    | `Alias (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Alias@ %a@ %a)@]" pp_print_pat _a0
          pp_print_alident _a1
    | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
    | `Array _a0 -> Format.fprintf fmt "@[<1>(`Array@ %a)@]" pp_print_pat _a0
    | `LabelS _a0 ->
        Format.fprintf fmt "@[<1>(`LabelS@ %a)@]" pp_print_alident _a0
    | `Label (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" pp_print_alident _a0
          pp_print_pat _a1
    | `OptLabl (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" pp_print_alident _a0
          pp_print_pat _a1
    | `OptLablS _a0 ->
        Format.fprintf fmt "@[<1>(`OptLablS@ %a)@]" pp_print_alident _a0
    | `OptLablExpr (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`OptLablExpr@ %a@ %a@ %a)@]"
          pp_print_alident _a0 pp_print_pat _a1 pp_print_exp _a2
    | `Bar (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Bar@ %a@ %a)@]" pp_print_pat _a0
          pp_print_pat _a1
    | `PaRng (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a)@]" pp_print_pat _a0
          pp_print_pat _a1
    | `Constraint (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" pp_print_pat _a0
          pp_print_ctyp _a1
    | `ClassPath _a0 ->
        Format.fprintf fmt "@[<1>(`ClassPath@ %a)@]" pp_print_ident _a0
    | `Lazy _a0 -> Format.fprintf fmt "@[<1>(`Lazy@ %a)@]" pp_print_pat _a0
    | `ModuleUnpack _a0 ->
        Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a)@]" pp_print_auident _a0
    | `ModuleConstraint (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a)@]"
          pp_print_auident _a0 pp_print_ctyp _a1
and pp_print_rec_pat: Format.formatter -> rec_pat -> unit =
  fun fmt  ->
    function
    | `RecBind (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" pp_print_ident _a0
          pp_print_pat _a1
    | `Sem (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_rec_pat _a0
          pp_print_rec_pat _a1
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_exp: Format.formatter -> exp -> unit =
  fun fmt  ->
    function
    | #vid as _a0 -> (pp_print_vid fmt _a0 :>unit)
    | `App (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" pp_print_exp _a0
          pp_print_exp _a1
    | `Vrn _a0 -> Format.fprintf fmt "@[<1>(`Vrn@ %a)@]" pp_print_string _a0
    | `Com (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" pp_print_exp _a0
          pp_print_exp _a1
    | `Sem (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_exp _a0
          pp_print_exp _a1
    | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" pp_print_exp _a0
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | `Record _a0 ->
        Format.fprintf fmt "@[<1>(`Record@ %a)@]" pp_print_rec_exp _a0
    | #literal as _a0 -> (pp_print_literal fmt _a0 :>unit)
    | `RecordWith (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a)@]" pp_print_rec_exp
          _a0 pp_print_exp _a1
    | `Field (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Field@ %a@ %a)@]" pp_print_exp _a0
          pp_print_exp _a1
    | `ArrayDot (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ArrayDot@ %a@ %a)@]" pp_print_exp _a0
          pp_print_exp _a1
    | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
    | `Array _a0 -> Format.fprintf fmt "@[<1>(`Array@ %a)@]" pp_print_exp _a0
    | `Assert _a0 ->
        Format.fprintf fmt "@[<1>(`Assert@ %a)@]" pp_print_exp _a0
    | `Assign (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Assign@ %a@ %a)@]" pp_print_exp _a0
          pp_print_exp _a1
    | `For (_a0,_a1,_a2,_a3,_a4) ->
        Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_alident _a0 pp_print_exp _a1 pp_print_exp _a2
          pp_print_flag _a3 pp_print_exp _a4
    | `Fun _a0 -> Format.fprintf fmt "@[<1>(`Fun@ %a)@]" pp_print_case _a0
    | `IfThenElse (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a)@]" pp_print_exp
          _a0 pp_print_exp _a1 pp_print_exp _a2
    | `IfThen (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`IfThen@ %a@ %a)@]" pp_print_exp _a0
          pp_print_exp _a1
    | `LabelS _a0 ->
        Format.fprintf fmt "@[<1>(`LabelS@ %a)@]" pp_print_alident _a0
    | `Label (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" pp_print_alident _a0
          pp_print_exp _a1
    | `Lazy _a0 -> Format.fprintf fmt "@[<1>(`Lazy@ %a)@]" pp_print_exp _a0
    | `LetIn (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a)@]" pp_print_flag _a0
          pp_print_bind _a1 pp_print_exp _a2
    | `LetTryInWith (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`LetTryInWith@ %a@ %a@ %a@ %a)@]"
          pp_print_flag _a0 pp_print_bind _a1 pp_print_exp _a2 pp_print_case
          _a3
    | `LetModule (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a)@]" pp_print_auident
          _a0 pp_print_mexp _a1 pp_print_exp _a2
    | `Match (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Match@ %a@ %a)@]" pp_print_exp _a0
          pp_print_case _a1
    | `New _a0 -> Format.fprintf fmt "@[<1>(`New@ %a)@]" pp_print_ident _a0
    | `Obj _a0 -> Format.fprintf fmt "@[<1>(`Obj@ %a)@]" pp_print_clfield _a0
    | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
    | `ObjPat (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a)@]" pp_print_pat _a0
          pp_print_clfield _a1
    | `ObjPatEnd _a0 ->
        Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a)@]" pp_print_pat _a0
    | `OptLabl (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" pp_print_alident _a0
          pp_print_exp _a1
    | `OptLablS _a0 ->
        Format.fprintf fmt "@[<1>(`OptLablS@ %a)@]" pp_print_alident _a0
    | `OvrInst _a0 ->
        Format.fprintf fmt "@[<1>(`OvrInst@ %a)@]" pp_print_rec_exp _a0
    | `OvrInstEmpty -> Format.fprintf fmt "`OvrInstEmpty"
    | `Seq _a0 -> Format.fprintf fmt "@[<1>(`Seq@ %a)@]" pp_print_exp _a0
    | `Send (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Send@ %a@ %a)@]" pp_print_exp _a0
          pp_print_alident _a1
    | `StringDot (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`StringDot@ %a@ %a)@]" pp_print_exp _a0
          pp_print_exp _a1
    | `Try (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Try@ %a@ %a)@]" pp_print_exp _a0
          pp_print_case _a1
    | `Constraint (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" pp_print_exp _a0
          pp_print_ctyp _a1
    | `Coercion (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Coercion@ %a@ %a@ %a)@]" pp_print_exp _a0
          pp_print_ctyp _a1 pp_print_ctyp _a2
    | `Subtype (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Subtype@ %a@ %a)@]" pp_print_exp _a0
          pp_print_ctyp _a1
    | `While (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`While@ %a@ %a)@]" pp_print_exp _a0
          pp_print_exp _a1
    | `LetOpen (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a)@]" pp_print_ident _a0
          pp_print_exp _a1
    | `LocalTypeFun (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a)@]" pp_print_alident
          _a0 pp_print_exp _a1
    | `Package_exp _a0 ->
        Format.fprintf fmt "@[<1>(`Package_exp@ %a)@]" pp_print_mexp _a0
and pp_print_rec_exp: Format.formatter -> rec_exp -> unit =
  fun fmt  ->
    function
    | `Sem (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_rec_exp _a0
          pp_print_rec_exp _a1
    | `RecBind (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" pp_print_ident _a0
          pp_print_exp _a1
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_mtyp: Format.formatter -> mtyp -> unit =
  fun fmt  ->
    function
    | #ident' as _a0 -> (pp_print_ident' fmt _a0 :>unit)
    | `Sig _a0 -> Format.fprintf fmt "@[<1>(`Sig@ %a)@]" pp_print_sigi _a0
    | `SigEnd -> Format.fprintf fmt "`SigEnd"
    | `Functor (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a)@]" pp_print_auident
          _a0 pp_print_mtyp _a1 pp_print_mtyp _a2
    | `With (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`With@ %a@ %a)@]" pp_print_mtyp _a0
          pp_print_constr _a1
    | `ModuleTypeOf _a0 ->
        Format.fprintf fmt "@[<1>(`ModuleTypeOf@ %a)@]" pp_print_mexp _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_sigi: Format.formatter -> sigi -> unit =
  fun fmt  ->
    function
    | `Val (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Val@ %a@ %a)@]" pp_print_alident _a0
          pp_print_ctyp _a1
    | `External (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a)@]" pp_print_alident
          _a0 pp_print_ctyp _a1 pp_print_strings _a2
    | `Type _a0 ->
        Format.fprintf fmt "@[<1>(`Type@ %a)@]" pp_print_typedecl _a0
    | `Exception _a0 ->
        Format.fprintf fmt "@[<1>(`Exception@ %a)@]" pp_print_of_ctyp _a0
    | `Class _a0 ->
        Format.fprintf fmt "@[<1>(`Class@ %a)@]" pp_print_cltdecl _a0
    | `ClassType _a0 ->
        Format.fprintf fmt "@[<1>(`ClassType@ %a)@]" pp_print_cltdecl _a0
    | `Module (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Module@ %a@ %a)@]" pp_print_auident _a0
          pp_print_mtyp _a1
    | `ModuleTypeEnd _a0 ->
        Format.fprintf fmt "@[<1>(`ModuleTypeEnd@ %a)@]" pp_print_auident _a0
    | `ModuleType (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a)@]" pp_print_auident
          _a0 pp_print_mtyp _a1
    | `Sem (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_sigi _a0
          pp_print_sigi _a1
    | `DirectiveSimple _a0 ->
        Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a)@]" pp_print_alident
          _a0
    | `Directive (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Directive@ %a@ %a)@]" pp_print_alident _a0
          pp_print_exp _a1
    | `Open _a0 -> Format.fprintf fmt "@[<1>(`Open@ %a)@]" pp_print_ident _a0
    | `Include _a0 ->
        Format.fprintf fmt "@[<1>(`Include@ %a)@]" pp_print_mtyp _a0
    | `RecModule _a0 ->
        Format.fprintf fmt "@[<1>(`RecModule@ %a)@]" pp_print_mbind _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_mbind: Format.formatter -> mbind -> unit =
  fun fmt  ->
    function
    | `And (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" pp_print_mbind _a0
          pp_print_mbind _a1
    | `ModuleBind (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a)@]"
          pp_print_auident _a0 pp_print_mtyp _a1 pp_print_mexp _a2
    | `Constraint (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" pp_print_auident
          _a0 pp_print_mtyp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_constr: Format.formatter -> constr -> unit =
  fun fmt  ->
    function
    | `TypeEq (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TypeEq@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_ctyp _a1
    | `ModuleEq (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ModuleEq@ %a@ %a)@]" pp_print_ident _a0
          pp_print_ident _a1
    | `TypeEqPriv (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TypeEqPriv@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_ctyp _a1
    | `TypeSubst (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TypeSubst@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_ctyp _a1
    | `ModuleSubst (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ModuleSubst@ %a@ %a)@]" pp_print_ident _a0
          pp_print_ident _a1
    | `And (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" pp_print_constr _a0
          pp_print_constr _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_bind: Format.formatter -> bind -> unit =
  fun fmt  ->
    function
    | `And (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" pp_print_bind _a0
          pp_print_bind _a1
    | `Bind (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Bind@ %a@ %a)@]" pp_print_pat _a0
          pp_print_exp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_case: Format.formatter -> case -> unit =
  fun fmt  ->
    function
    | `Bar (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Bar@ %a@ %a)@]" pp_print_case _a0
          pp_print_case _a1
    | `Case (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Case@ %a@ %a)@]" pp_print_pat _a0
          pp_print_exp _a1
    | `CaseWhen (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`CaseWhen@ %a@ %a@ %a)@]" pp_print_pat _a0
          pp_print_exp _a1 pp_print_exp _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_mexp: Format.formatter -> mexp -> unit =
  fun fmt  ->
    function
    | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>unit)
    | `App (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" pp_print_mexp _a0
          pp_print_mexp _a1
    | `Functor (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a)@]" pp_print_auident
          _a0 pp_print_mtyp _a1 pp_print_mexp _a2
    | `Struct _a0 ->
        Format.fprintf fmt "@[<1>(`Struct@ %a)@]" pp_print_stru _a0
    | `StructEnd -> Format.fprintf fmt "`StructEnd"
    | `Constraint (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" pp_print_mexp _a0
          pp_print_mtyp _a1
    | `PackageModule _a0 ->
        Format.fprintf fmt "@[<1>(`PackageModule@ %a)@]" pp_print_exp _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_stru: Format.formatter -> stru -> unit =
  fun fmt  ->
    function
    | `Class _a0 ->
        Format.fprintf fmt "@[<1>(`Class@ %a)@]" pp_print_cldecl _a0
    | `ClassType _a0 ->
        Format.fprintf fmt "@[<1>(`ClassType@ %a)@]" pp_print_cltdecl _a0
    | `Sem (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_stru _a0
          pp_print_stru _a1
    | `DirectiveSimple _a0 ->
        Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a)@]" pp_print_alident
          _a0
    | `Directive (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Directive@ %a@ %a)@]" pp_print_alident _a0
          pp_print_exp _a1
    | `Exception _a0 ->
        Format.fprintf fmt "@[<1>(`Exception@ %a)@]" pp_print_of_ctyp _a0
    | `StExp _a0 -> Format.fprintf fmt "@[<1>(`StExp@ %a)@]" pp_print_exp _a0
    | `External (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a)@]" pp_print_alident
          _a0 pp_print_ctyp _a1 pp_print_strings _a2
    | `Include _a0 ->
        Format.fprintf fmt "@[<1>(`Include@ %a)@]" pp_print_mexp _a0
    | `Module (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Module@ %a@ %a)@]" pp_print_auident _a0
          pp_print_mexp _a1
    | `RecModule _a0 ->
        Format.fprintf fmt "@[<1>(`RecModule@ %a)@]" pp_print_mbind _a0
    | `ModuleType (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a)@]" pp_print_auident
          _a0 pp_print_mtyp _a1
    | `Open _a0 -> Format.fprintf fmt "@[<1>(`Open@ %a)@]" pp_print_ident _a0
    | `Type _a0 ->
        Format.fprintf fmt "@[<1>(`Type@ %a)@]" pp_print_typedecl _a0
    | `TypeWith (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TypeWith@ %a@ %a)@]" pp_print_typedecl _a0
          pp_print_strings _a1
    | `Value (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Value@ %a@ %a)@]" pp_print_flag _a0
          pp_print_bind _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_cltdecl: Format.formatter -> cltdecl -> unit =
  fun fmt  ->
    function
    | `And (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" pp_print_cltdecl _a0
          pp_print_cltdecl _a1
    | `CtDecl (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`CtDecl@ %a@ %a@ %a@ %a)@]" pp_print_flag
          _a0 pp_print_ident _a1 pp_print_type_parameters _a2 pp_print_cltyp
          _a3
    | `CtDeclS (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`CtDeclS@ %a@ %a@ %a)@]" pp_print_flag _a0
          pp_print_ident _a1 pp_print_cltyp _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_cltyp: Format.formatter -> cltyp -> unit =
  fun fmt  ->
    function
    | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>unit)
    | `ClApply (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ClApply@ %a@ %a)@]" pp_print_vid _a0
          pp_print_type_parameters _a1
    | `CtFun (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`CtFun@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_cltyp _a1
    | `ObjTy (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ObjTy@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_clsigi _a1
    | `ObjTyEnd _a0 ->
        Format.fprintf fmt "@[<1>(`ObjTyEnd@ %a)@]" pp_print_ctyp _a0
    | `Obj _a0 -> Format.fprintf fmt "@[<1>(`Obj@ %a)@]" pp_print_clsigi _a0
    | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
    | `And (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" pp_print_cltyp _a0
          pp_print_cltyp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_clsigi: Format.formatter -> clsigi -> unit =
  fun fmt  ->
    function
    | `Sem (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_clsigi _a0
          pp_print_clsigi _a1
    | `SigInherit _a0 ->
        Format.fprintf fmt "@[<1>(`SigInherit@ %a)@]" pp_print_cltyp _a0
    | `CgVal (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`CgVal@ %a@ %a@ %a@ %a)@]" pp_print_alident
          _a0 pp_print_flag _a1 pp_print_flag _a2 pp_print_ctyp _a3
    | `Method (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Method@ %a@ %a@ %a)@]" pp_print_alident
          _a0 pp_print_flag _a1 pp_print_ctyp _a2
    | `VirMeth (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`VirMeth@ %a@ %a@ %a)@]" pp_print_alident
          _a0 pp_print_flag _a1 pp_print_ctyp _a2
    | `Eq (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_ctyp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_cldecl: Format.formatter -> cldecl -> unit =
  fun fmt  ->
    function
    | `ClDecl (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`ClDecl@ %a@ %a@ %a@ %a)@]" pp_print_flag
          _a0 pp_print_ident _a1 pp_print_type_parameters _a2 pp_print_clexp
          _a3
    | `ClDeclS (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ClDeclS@ %a@ %a@ %a)@]" pp_print_flag _a0
          pp_print_ident _a1 pp_print_clexp _a2
    | `And (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" pp_print_cldecl _a0
          pp_print_cldecl _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_clexp: Format.formatter -> clexp -> unit =
  fun fmt  ->
    function
    | `CeApp (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a)@]" pp_print_clexp _a0
          pp_print_exp _a1
    | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>unit)
    | `ClApply (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ClApply@ %a@ %a)@]" pp_print_vid _a0
          pp_print_type_parameters _a1
    | `CeFun (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a)@]" pp_print_pat _a0
          pp_print_clexp _a1
    | `LetIn (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a)@]" pp_print_flag _a0
          pp_print_bind _a1 pp_print_clexp _a2
    | `Obj _a0 -> Format.fprintf fmt "@[<1>(`Obj@ %a)@]" pp_print_clfield _a0
    | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
    | `ObjPat (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a)@]" pp_print_pat _a0
          pp_print_clfield _a1
    | `ObjPatEnd _a0 ->
        Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a)@]" pp_print_pat _a0
    | `Constraint (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" pp_print_clexp _a0
          pp_print_cltyp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_clfield: Format.formatter -> clfield -> unit =
  fun fmt  ->
    function
    | `Sem (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_clfield _a0
          pp_print_clfield _a1
    | `Inherit (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a)@]" pp_print_flag _a0
          pp_print_clexp _a1
    | `InheritAs (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`InheritAs@ %a@ %a@ %a)@]" pp_print_flag
          _a0 pp_print_clexp _a1 pp_print_alident _a2
    | `CrVal (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a)@]" pp_print_alident
          _a0 pp_print_flag _a1 pp_print_flag _a2 pp_print_exp _a3
    | `VirVal (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`VirVal@ %a@ %a@ %a)@]" pp_print_alident
          _a0 pp_print_flag _a1 pp_print_ctyp _a2
    | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
        Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_alident _a0 pp_print_flag _a1 pp_print_flag _a2
          pp_print_exp _a3 pp_print_ctyp _a4
    | `CrMthS (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`CrMthS@ %a@ %a@ %a@ %a)@]"
          pp_print_alident _a0 pp_print_flag _a1 pp_print_flag _a2
          pp_print_exp _a3
    | `VirMeth (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`VirMeth@ %a@ %a@ %a)@]" pp_print_alident
          _a0 pp_print_flag _a1 pp_print_ctyp _a2
    | `Eq (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" pp_print_ctyp _a0
          pp_print_ctyp _a1
    | `Initializer _a0 ->
        Format.fprintf fmt "@[<1>(`Initializer@ %a)@]" pp_print_exp _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)

let rec pp_print_ep: Format.formatter -> ep -> unit =
  fun fmt  ->
    function
    | #vid as _a0 -> (pp_print_vid fmt _a0 :>unit)
    | `App (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" pp_print_ep _a0
          pp_print_ep _a1
    | `Vrn _a0 -> Format.fprintf fmt "@[<1>(`Vrn@ %a)@]" pp_print_string _a0
    | `Com (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" pp_print_ep _a0
          pp_print_ep _a1
    | `Sem (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_ep _a0
          pp_print_ep _a1
    | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" pp_print_ep _a0
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
    | `Array _a0 -> Format.fprintf fmt "@[<1>(`Array@ %a)@]" pp_print_ep _a0
    | `Record _a0 ->
        Format.fprintf fmt "@[<1>(`Record@ %a)@]" pp_print_rec_bind _a0
    | #literal as _a0 -> (pp_print_literal fmt _a0 :>unit)
and pp_print_rec_bind: Format.formatter -> rec_bind -> unit =
  fun fmt  ->
    function
    | `RecBind (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" pp_print_ident _a0
          pp_print_ep _a1
    | `Sem (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_rec_bind _a0
          pp_print_rec_bind _a1
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)

class print =
  object (self : 'self_type)
    inherit  printbase
    method loc : 'fmt -> loc -> unit= fun fmt  _a0  -> self#fanloc_t fmt _a0
    method ant : 'fmt -> ant -> unit=
      fun fmt  (`Ant (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" self#loc _a0
          self#fanutil_anti_cxt _a1
    method nil : 'fmt -> nil -> unit=
      fun fmt  `Nil  -> Format.fprintf fmt "`Nil"
    method literal : 'fmt -> literal -> unit=
      fun fmt  ->
        function
        | `Chr _a0 -> Format.fprintf fmt "@[<1>(`Chr@ %a)@]" self#string _a0
        | `Int _a0 -> Format.fprintf fmt "@[<1>(`Int@ %a)@]" self#string _a0
        | `Int32 _a0 ->
            Format.fprintf fmt "@[<1>(`Int32@ %a)@]" self#string _a0
        | `Int64 _a0 ->
            Format.fprintf fmt "@[<1>(`Int64@ %a)@]" self#string _a0
        | `Flo _a0 -> Format.fprintf fmt "@[<1>(`Flo@ %a)@]" self#string _a0
        | `Nativeint _a0 ->
            Format.fprintf fmt "@[<1>(`Nativeint@ %a)@]" self#string _a0
        | `Str _a0 -> Format.fprintf fmt "@[<1>(`Str@ %a)@]" self#string _a0
    method flag : 'fmt -> flag -> unit=
      fun fmt  ->
        function
        | `Positive -> Format.fprintf fmt "`Positive"
        | `Negative -> Format.fprintf fmt "`Negative"
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method position_flag : 'fmt -> position_flag -> unit=
      fun fmt  ->
        function
        | `Positive -> Format.fprintf fmt "`Positive"
        | `Negative -> Format.fprintf fmt "`Negative"
        | `Normal -> Format.fprintf fmt "`Normal"
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method strings : 'fmt -> strings -> unit=
      fun fmt  ->
        function
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#strings _a0
              self#strings _a1
        | `Str _a0 -> Format.fprintf fmt "@[<1>(`Str@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method lident : 'fmt -> lident -> unit=
      fun fmt  (`Lid _a0)  ->
        Format.fprintf fmt "@[<1>(`Lid@ %a)@]" self#string _a0
    method alident : 'fmt -> alident -> unit=
      fun fmt  ->
        function
        | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method auident : 'fmt -> auident -> unit=
      fun fmt  ->
        function
        | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method aident : 'fmt -> aident -> unit=
      fun fmt  ->
        function
        | #alident as _a0 -> (self#alident fmt _a0 :>unit)
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method astring : 'fmt -> astring -> unit=
      fun fmt  ->
        function
        | `C _a0 -> Format.fprintf fmt "@[<1>(`C@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method uident : 'fmt -> uident -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#uident _a0
              self#uident _a1
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#uident _a0
              self#uident _a1
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method ident : 'fmt -> ident -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#ident _a0
              self#ident _a1
        | `Apply (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Apply@ %a@ %a)@]" self#ident _a0
              self#ident _a1
        | #alident as _a0 -> (self#alident fmt _a0 :>unit)
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method ident' : 'fmt -> ident' -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#ident _a0
              self#ident _a1
        | `Apply (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Apply@ %a@ %a)@]" self#ident _a0
              self#ident _a1
        | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" self#string _a0
        | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" self#string _a0
    method vid : 'fmt -> vid -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#vid _a0 self#vid
              _a1
        | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" self#string _a0
        | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method vid' : 'fmt -> vid' -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#vid _a0 self#vid
              _a1
        | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" self#string _a0
        | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" self#string _a0
    method dupath : 'fmt -> dupath -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#dupath _a0
              self#dupath _a1
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method dlpath : 'fmt -> dlpath -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#dupath _a0
              self#alident _a1
        | #alident as _a0 -> (self#alident fmt _a0 :>unit)
    method any : 'fmt -> any -> unit=
      fun fmt  `Any  -> Format.fprintf fmt "`Any"
    method ctyp : 'fmt -> ctyp -> unit=
      fun fmt  ->
        function
        | `Alias (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a)@]" self#ctyp _a0
              self#alident _a1
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `Arrow (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Arrow@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `ClassPath _a0 ->
            Format.fprintf fmt "@[<1>(`ClassPath@ %a)@]" self#ident _a0
        | `Label (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" self#alident _a0
              self#ctyp _a1
        | `OptLabl (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" self#alident _a0
              self#ctyp _a1
        | #ident' as _a0 -> (self#ident' fmt _a0 :>unit)
        | `TyObj (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyObj@ %a@ %a)@]" self#name_ctyp _a0
              self#flag _a1
        | `TyObjEnd _a0 ->
            Format.fprintf fmt "@[<1>(`TyObjEnd@ %a)@]" self#flag _a0
        | `TyPol (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyPol@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `TyPolEnd _a0 ->
            Format.fprintf fmt "@[<1>(`TyPolEnd@ %a)@]" self#ctyp _a0
        | `TyTypePol (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyTypePol@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `Quote (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]" self#position_flag
              _a0 self#alident _a1
        | `QuoteAny _a0 ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]" self#position_flag
              _a0
        | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" self#ctyp _a0
        | `Sta (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sta@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `PolyEq _a0 ->
            Format.fprintf fmt "@[<1>(`PolyEq@ %a)@]" self#row_field _a0
        | `PolySup _a0 ->
            Format.fprintf fmt "@[<1>(`PolySup@ %a)@]" self#row_field _a0
        | `PolyInf _a0 ->
            Format.fprintf fmt "@[<1>(`PolyInf@ %a)@]" self#row_field _a0
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `PolyInfSup (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PolyInfSup@ %a@ %a)@]" self#row_field
              _a0 self#tag_names _a1
        | `Package _a0 ->
            Format.fprintf fmt "@[<1>(`Package@ %a)@]" self#mtyp _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method type_parameters : 'fmt -> type_parameters -> unit=
      fun fmt  ->
        function
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#type_parameters
              _a0 self#type_parameters _a1
        | `Ctyp _a0 -> Format.fprintf fmt "@[<1>(`Ctyp@ %a)@]" self#ctyp _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method row_field : 'fmt -> row_field -> unit=
      fun fmt  ->
        function
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
        | `Bar (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Bar@ %a@ %a)@]" self#row_field _a0
              self#row_field _a1
        | `TyVrn _a0 ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a)@]" self#astring _a0
        | `TyVrnOf (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyVrnOf@ %a@ %a)@]" self#astring _a0
              self#ctyp _a1
        | `Ctyp _a0 -> Format.fprintf fmt "@[<1>(`Ctyp@ %a)@]" self#ctyp _a0
    method tag_names : 'fmt -> tag_names -> unit=
      fun fmt  ->
        function
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#tag_names _a0
              self#tag_names _a1
        | `TyVrn _a0 ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a)@]" self#astring _a0
    method typedecl : 'fmt -> typedecl -> unit=
      fun fmt  ->
        function
        | `TyDcl (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`TyDcl@ %a@ %a@ %a@ %a)@]" self#alident
              _a0 self#opt_decl_params _a1 self#type_info _a2
              self#opt_type_constr _a3
        | `TyAbstr (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyAbstr@ %a@ %a@ %a)@]" self#alident
              _a0 self#opt_decl_params _a1 self#opt_type_constr _a2
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#typedecl _a0
              self#typedecl _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method type_constr : 'fmt -> type_constr -> unit=
      fun fmt  ->
        function
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#type_constr _a0
              self#type_constr _a1
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" self#ctyp _a0 self#ctyp
              _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method opt_type_constr : 'fmt -> opt_type_constr -> unit=
      fun fmt  ->
        function
        | `Some _a0 ->
            Format.fprintf fmt "@[<1>(`Some@ %a)@]" self#type_constr _a0
        | `None -> Format.fprintf fmt "`None"
    method decl_param : 'fmt -> decl_param -> unit=
      fun fmt  ->
        function
        | `Quote (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]" self#position_flag
              _a0 self#alident _a1
        | `QuoteAny _a0 ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]" self#position_flag
              _a0
        | `Any -> Format.fprintf fmt "`Any"
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method decl_params : 'fmt -> decl_params -> unit=
      fun fmt  ->
        function
        | `Quote (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]" self#position_flag
              _a0 self#alident _a1
        | `QuoteAny _a0 ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]" self#position_flag
              _a0
        | `Any -> Format.fprintf fmt "`Any"
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#decl_params _a0
              self#decl_params _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method opt_decl_params : 'fmt -> opt_decl_params -> unit=
      fun fmt  ->
        function
        | `Some _a0 ->
            Format.fprintf fmt "@[<1>(`Some@ %a)@]" self#decl_params _a0
        | `None -> Format.fprintf fmt "`None"
    method type_info : 'fmt -> type_info -> unit=
      fun fmt  ->
        function
        | `TyMan (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyMan@ %a@ %a@ %a)@]" self#ctyp _a0
              self#flag _a1 self#type_repr _a2
        | `TyRepr (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyRepr@ %a@ %a)@]" self#flag _a0
              self#type_repr _a1
        | `TyEq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyEq@ %a@ %a)@]" self#flag _a0
              self#ctyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method type_repr : 'fmt -> type_repr -> unit=
      fun fmt  ->
        function
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" self#name_ctyp _a0
        | `Sum _a0 -> Format.fprintf fmt "@[<1>(`Sum@ %a)@]" self#or_ctyp _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method name_ctyp : 'fmt -> name_ctyp -> unit=
      fun fmt  ->
        function
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#name_ctyp _a0
              self#name_ctyp _a1
        | `TyCol (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a)@]" self#alident _a0
              self#ctyp _a1
        | `TyColMut (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyColMut@ %a@ %a)@]" self#alident _a0
              self#ctyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method or_ctyp : 'fmt -> or_ctyp -> unit=
      fun fmt  ->
        function
        | `Bar (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Bar@ %a@ %a)@]" self#or_ctyp _a0
              self#or_ctyp _a1
        | `TyCol (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a)@]" self#auident _a0
              self#ctyp _a1
        | `Of (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a)@]" self#auident _a0
              self#ctyp _a1
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method of_ctyp : 'fmt -> of_ctyp -> unit=
      fun fmt  ->
        function
        | `Of (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a)@]" self#vid _a0 self#ctyp
              _a1
        | #vid' as _a0 -> (self#vid' fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method pat : 'fmt -> pat -> unit=
      fun fmt  ->
        function
        | #vid as _a0 -> (self#vid fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#pat _a0 self#pat
              _a1
        | `Vrn _a0 -> Format.fprintf fmt "@[<1>(`Vrn@ %a)@]" self#string _a0
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#pat _a0 self#pat
              _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#pat _a0 self#pat
              _a1
        | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" self#pat _a0
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" self#rec_pat _a0
        | #literal as _a0 -> (self#literal fmt _a0 :>unit)
        | `Alias (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a)@]" self#pat _a0
              self#alident _a1
        | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
        | `Array _a0 -> Format.fprintf fmt "@[<1>(`Array@ %a)@]" self#pat _a0
        | `LabelS _a0 ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a)@]" self#alident _a0
        | `Label (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" self#alident _a0
              self#pat _a1
        | `OptLabl (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" self#alident _a0
              self#pat _a1
        | `OptLablS _a0 ->
            Format.fprintf fmt "@[<1>(`OptLablS@ %a)@]" self#alident _a0
        | `OptLablExpr (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`OptLablExpr@ %a@ %a@ %a)@]"
              self#alident _a0 self#pat _a1 self#exp _a2
        | `Bar (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Bar@ %a@ %a)@]" self#pat _a0 self#pat
              _a1
        | `PaRng (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a)@]" self#pat _a0
              self#pat _a1
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#pat _a0
              self#ctyp _a1
        | `ClassPath _a0 ->
            Format.fprintf fmt "@[<1>(`ClassPath@ %a)@]" self#ident _a0
        | `Lazy _a0 -> Format.fprintf fmt "@[<1>(`Lazy@ %a)@]" self#pat _a0
        | `ModuleUnpack _a0 ->
            Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a)@]" self#auident _a0
        | `ModuleConstraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a)@]"
              self#auident _a0 self#ctyp _a1
    method rec_pat : 'fmt -> rec_pat -> unit=
      fun fmt  ->
        function
        | `RecBind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" self#ident _a0
              self#pat _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#rec_pat _a0
              self#rec_pat _a1
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method exp : 'fmt -> exp -> unit=
      fun fmt  ->
        function
        | #vid as _a0 -> (self#vid fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#exp _a0 self#exp
              _a1
        | `Vrn _a0 -> Format.fprintf fmt "@[<1>(`Vrn@ %a)@]" self#string _a0
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#exp _a0 self#exp
              _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#exp _a0 self#exp
              _a1
        | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" self#exp _a0
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" self#rec_exp _a0
        | #literal as _a0 -> (self#literal fmt _a0 :>unit)
        | `RecordWith (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a)@]" self#rec_exp
              _a0 self#exp _a1
        | `Field (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Field@ %a@ %a)@]" self#exp _a0
              self#exp _a1
        | `ArrayDot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ArrayDot@ %a@ %a)@]" self#exp _a0
              self#exp _a1
        | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
        | `Array _a0 -> Format.fprintf fmt "@[<1>(`Array@ %a)@]" self#exp _a0
        | `Assert _a0 ->
            Format.fprintf fmt "@[<1>(`Assert@ %a)@]" self#exp _a0
        | `Assign (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Assign@ %a@ %a)@]" self#exp _a0
              self#exp _a1
        | `For (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a)@]"
              self#alident _a0 self#exp _a1 self#exp _a2 self#flag _a3
              self#exp _a4
        | `Fun _a0 -> Format.fprintf fmt "@[<1>(`Fun@ %a)@]" self#case _a0
        | `IfThenElse (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a)@]" self#exp
              _a0 self#exp _a1 self#exp _a2
        | `IfThen (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`IfThen@ %a@ %a)@]" self#exp _a0
              self#exp _a1
        | `LabelS _a0 ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a)@]" self#alident _a0
        | `Label (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" self#alident _a0
              self#exp _a1
        | `Lazy _a0 -> Format.fprintf fmt "@[<1>(`Lazy@ %a)@]" self#exp _a0
        | `LetIn (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a)@]" self#flag _a0
              self#bind _a1 self#exp _a2
        | `LetTryInWith (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`LetTryInWith@ %a@ %a@ %a@ %a)@]"
              self#flag _a0 self#bind _a1 self#exp _a2 self#case _a3
        | `LetModule (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a)@]" self#auident
              _a0 self#mexp _a1 self#exp _a2
        | `Match (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Match@ %a@ %a)@]" self#exp _a0
              self#case _a1
        | `New _a0 -> Format.fprintf fmt "@[<1>(`New@ %a)@]" self#ident _a0
        | `Obj _a0 -> Format.fprintf fmt "@[<1>(`Obj@ %a)@]" self#clfield _a0
        | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
        | `ObjPat (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a)@]" self#pat _a0
              self#clfield _a1
        | `ObjPatEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a)@]" self#pat _a0
        | `OptLabl (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" self#alident _a0
              self#exp _a1
        | `OptLablS _a0 ->
            Format.fprintf fmt "@[<1>(`OptLablS@ %a)@]" self#alident _a0
        | `OvrInst _a0 ->
            Format.fprintf fmt "@[<1>(`OvrInst@ %a)@]" self#rec_exp _a0
        | `OvrInstEmpty -> Format.fprintf fmt "`OvrInstEmpty"
        | `Seq _a0 -> Format.fprintf fmt "@[<1>(`Seq@ %a)@]" self#exp _a0
        | `Send (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Send@ %a@ %a)@]" self#exp _a0
              self#alident _a1
        | `StringDot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`StringDot@ %a@ %a)@]" self#exp _a0
              self#exp _a1
        | `Try (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Try@ %a@ %a)@]" self#exp _a0 self#case
              _a1
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#exp _a0
              self#ctyp _a1
        | `Coercion (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Coercion@ %a@ %a@ %a)@]" self#exp _a0
              self#ctyp _a1 self#ctyp _a2
        | `Subtype (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Subtype@ %a@ %a)@]" self#exp _a0
              self#ctyp _a1
        | `While (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`While@ %a@ %a)@]" self#exp _a0
              self#exp _a1
        | `LetOpen (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a)@]" self#ident _a0
              self#exp _a1
        | `LocalTypeFun (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a)@]" self#alident
              _a0 self#exp _a1
        | `Package_exp _a0 ->
            Format.fprintf fmt "@[<1>(`Package_exp@ %a)@]" self#mexp _a0
    method rec_exp : 'fmt -> rec_exp -> unit=
      fun fmt  ->
        function
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#rec_exp _a0
              self#rec_exp _a1
        | `RecBind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" self#ident _a0
              self#exp _a1
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method mtyp : 'fmt -> mtyp -> unit=
      fun fmt  ->
        function
        | #ident' as _a0 -> (self#ident' fmt _a0 :>unit)
        | `Sig _a0 -> Format.fprintf fmt "@[<1>(`Sig@ %a)@]" self#sigi _a0
        | `SigEnd -> Format.fprintf fmt "`SigEnd"
        | `Functor (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a)@]" self#auident
              _a0 self#mtyp _a1 self#mtyp _a2
        | `With (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`With@ %a@ %a)@]" self#mtyp _a0
              self#constr _a1
        | `ModuleTypeOf _a0 ->
            Format.fprintf fmt "@[<1>(`ModuleTypeOf@ %a)@]" self#mexp _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method sigi : 'fmt -> sigi -> unit=
      fun fmt  ->
        function
        | `Val (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Val@ %a@ %a)@]" self#alident _a0
              self#ctyp _a1
        | `External (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a)@]" self#alident
              _a0 self#ctyp _a1 self#strings _a2
        | `Type _a0 ->
            Format.fprintf fmt "@[<1>(`Type@ %a)@]" self#typedecl _a0
        | `Exception _a0 ->
            Format.fprintf fmt "@[<1>(`Exception@ %a)@]" self#of_ctyp _a0
        | `Class _a0 ->
            Format.fprintf fmt "@[<1>(`Class@ %a)@]" self#cltdecl _a0
        | `ClassType _a0 ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a)@]" self#cltdecl _a0
        | `Module (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a)@]" self#auident _a0
              self#mtyp _a1
        | `ModuleTypeEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ModuleTypeEnd@ %a)@]" self#auident _a0
        | `ModuleType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a)@]" self#auident
              _a0 self#mtyp _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#sigi _a0
              self#sigi _a1
        | `DirectiveSimple _a0 ->
            Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a)@]" self#alident
              _a0
        | `Directive (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a)@]" self#alident _a0
              self#exp _a1
        | `Open _a0 -> Format.fprintf fmt "@[<1>(`Open@ %a)@]" self#ident _a0
        | `Include _a0 ->
            Format.fprintf fmt "@[<1>(`Include@ %a)@]" self#mtyp _a0
        | `RecModule _a0 ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a)@]" self#mbind _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method mbind : 'fmt -> mbind -> unit=
      fun fmt  ->
        function
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#mbind _a0
              self#mbind _a1
        | `ModuleBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a)@]"
              self#auident _a0 self#mtyp _a1 self#mexp _a2
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#auident
              _a0 self#mtyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method constr : 'fmt -> constr -> unit=
      fun fmt  ->
        function
        | `TypeEq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TypeEq@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `ModuleEq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleEq@ %a@ %a)@]" self#ident _a0
              self#ident _a1
        | `TypeEqPriv (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TypeEqPriv@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `TypeSubst (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TypeSubst@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `ModuleSubst (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleSubst@ %a@ %a)@]" self#ident _a0
              self#ident _a1
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#constr _a0
              self#constr _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method bind : 'fmt -> bind -> unit=
      fun fmt  ->
        function
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#bind _a0
              self#bind _a1
        | `Bind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Bind@ %a@ %a)@]" self#pat _a0 
              self#exp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method case : 'fmt -> case -> unit=
      fun fmt  ->
        function
        | `Bar (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Bar@ %a@ %a)@]" self#case _a0
              self#case _a1
        | `Case (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Case@ %a@ %a)@]" self#pat _a0 
              self#exp _a1
        | `CaseWhen (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CaseWhen@ %a@ %a@ %a)@]" self#pat _a0
              self#exp _a1 self#exp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method mexp : 'fmt -> mexp -> unit=
      fun fmt  ->
        function
        | #vid' as _a0 -> (self#vid' fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#mexp _a0
              self#mexp _a1
        | `Functor (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a)@]" self#auident
              _a0 self#mtyp _a1 self#mexp _a2
        | `Struct _a0 ->
            Format.fprintf fmt "@[<1>(`Struct@ %a)@]" self#stru _a0
        | `StructEnd -> Format.fprintf fmt "`StructEnd"
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#mexp _a0
              self#mtyp _a1
        | `PackageModule _a0 ->
            Format.fprintf fmt "@[<1>(`PackageModule@ %a)@]" self#exp _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method stru : 'fmt -> stru -> unit=
      fun fmt  ->
        function
        | `Class _a0 ->
            Format.fprintf fmt "@[<1>(`Class@ %a)@]" self#cldecl _a0
        | `ClassType _a0 ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a)@]" self#cltdecl _a0
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#stru _a0
              self#stru _a1
        | `DirectiveSimple _a0 ->
            Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a)@]" self#alident
              _a0
        | `Directive (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a)@]" self#alident _a0
              self#exp _a1
        | `Exception _a0 ->
            Format.fprintf fmt "@[<1>(`Exception@ %a)@]" self#of_ctyp _a0
        | `StExp _a0 -> Format.fprintf fmt "@[<1>(`StExp@ %a)@]" self#exp _a0
        | `External (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a)@]" self#alident
              _a0 self#ctyp _a1 self#strings _a2
        | `Include _a0 ->
            Format.fprintf fmt "@[<1>(`Include@ %a)@]" self#mexp _a0
        | `Module (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a)@]" self#auident _a0
              self#mexp _a1
        | `RecModule _a0 ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a)@]" self#mbind _a0
        | `ModuleType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a)@]" self#auident
              _a0 self#mtyp _a1
        | `Open _a0 -> Format.fprintf fmt "@[<1>(`Open@ %a)@]" self#ident _a0
        | `Type _a0 ->
            Format.fprintf fmt "@[<1>(`Type@ %a)@]" self#typedecl _a0
        | `TypeWith (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TypeWith@ %a@ %a)@]" self#typedecl _a0
              self#strings _a1
        | `Value (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Value@ %a@ %a)@]" self#flag _a0
              self#bind _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method cltdecl : 'fmt -> cltdecl -> unit=
      fun fmt  ->
        function
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#cltdecl _a0
              self#cltdecl _a1
        | `CtDecl (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CtDecl@ %a@ %a@ %a@ %a)@]" self#flag
              _a0 self#ident _a1 self#type_parameters _a2 self#cltyp _a3
        | `CtDeclS (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CtDeclS@ %a@ %a@ %a)@]" self#flag _a0
              self#ident _a1 self#cltyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method cltyp : 'fmt -> cltyp -> unit=
      fun fmt  ->
        function
        | #vid' as _a0 -> (self#vid' fmt _a0 :>unit)
        | `ClApply (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClApply@ %a@ %a)@]" self#vid _a0
              self#type_parameters _a1
        | `CtFun (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`CtFun@ %a@ %a)@]" self#ctyp _a0
              self#cltyp _a1
        | `ObjTy (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjTy@ %a@ %a)@]" self#ctyp _a0
              self#clsigi _a1
        | `ObjTyEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjTyEnd@ %a)@]" self#ctyp _a0
        | `Obj _a0 -> Format.fprintf fmt "@[<1>(`Obj@ %a)@]" self#clsigi _a0
        | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#cltyp _a0
              self#cltyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method clsigi : 'fmt -> clsigi -> unit=
      fun fmt  ->
        function
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#clsigi _a0
              self#clsigi _a1
        | `SigInherit _a0 ->
            Format.fprintf fmt "@[<1>(`SigInherit@ %a)@]" self#cltyp _a0
        | `CgVal (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CgVal@ %a@ %a@ %a@ %a)@]" self#alident
              _a0 self#flag _a1 self#flag _a2 self#ctyp _a3
        | `Method (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Method@ %a@ %a@ %a)@]" self#alident
              _a0 self#flag _a1 self#ctyp _a2
        | `VirMeth (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`VirMeth@ %a@ %a@ %a)@]" self#alident
              _a0 self#flag _a1 self#ctyp _a2
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" self#ctyp _a0 self#ctyp
              _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method cldecl : 'fmt -> cldecl -> unit=
      fun fmt  ->
        function
        | `ClDecl (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`ClDecl@ %a@ %a@ %a@ %a)@]" self#flag
              _a0 self#ident _a1 self#type_parameters _a2 self#clexp _a3
        | `ClDeclS (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ClDeclS@ %a@ %a@ %a)@]" self#flag _a0
              self#ident _a1 self#clexp _a2
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#cldecl _a0
              self#cldecl _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method clexp : 'fmt -> clexp -> unit=
      fun fmt  ->
        function
        | `CeApp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a)@]" self#clexp _a0
              self#exp _a1
        | #vid' as _a0 -> (self#vid' fmt _a0 :>unit)
        | `ClApply (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClApply@ %a@ %a)@]" self#vid _a0
              self#type_parameters _a1
        | `CeFun (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a)@]" self#pat _a0
              self#clexp _a1
        | `LetIn (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a)@]" self#flag _a0
              self#bind _a1 self#clexp _a2
        | `Obj _a0 -> Format.fprintf fmt "@[<1>(`Obj@ %a)@]" self#clfield _a0
        | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
        | `ObjPat (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a)@]" self#pat _a0
              self#clfield _a1
        | `ObjPatEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a)@]" self#pat _a0
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#clexp _a0
              self#cltyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method clfield : 'fmt -> clfield -> unit=
      fun fmt  ->
        function
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#clfield _a0
              self#clfield _a1
        | `Inherit (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a)@]" self#flag _a0
              self#clexp _a1
        | `InheritAs (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`InheritAs@ %a@ %a@ %a)@]" self#flag
              _a0 self#clexp _a1 self#alident _a2
        | `CrVal (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a)@]" self#alident
              _a0 self#flag _a1 self#flag _a2 self#exp _a3
        | `VirVal (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`VirVal@ %a@ %a@ %a)@]" self#alident
              _a0 self#flag _a1 self#ctyp _a2
        | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a)@]"
              self#alident _a0 self#flag _a1 self#flag _a2 self#exp _a3
              self#ctyp _a4
        | `CrMthS (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CrMthS@ %a@ %a@ %a@ %a)@]"
              self#alident _a0 self#flag _a1 self#flag _a2 self#exp _a3
        | `VirMeth (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`VirMeth@ %a@ %a@ %a)@]" self#alident
              _a0 self#flag _a1 self#ctyp _a2
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" self#ctyp _a0 self#ctyp
              _a1
        | `Initializer _a0 ->
            Format.fprintf fmt "@[<1>(`Initializer@ %a)@]" self#exp _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method ep : 'fmt -> ep -> unit=
      fun fmt  ->
        function
        | #vid as _a0 -> (self#vid fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#ep _a0 self#ep
              _a1
        | `Vrn _a0 -> Format.fprintf fmt "@[<1>(`Vrn@ %a)@]" self#string _a0
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#ep _a0 self#ep
              _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#ep _a0 self#ep
              _a1
        | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" self#ep _a0
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
        | `Array _a0 -> Format.fprintf fmt "@[<1>(`Array@ %a)@]" self#ep _a0
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" self#rec_bind _a0
        | #literal as _a0 -> (self#literal fmt _a0 :>unit)
    method rec_bind : 'fmt -> rec_bind -> unit=
      fun fmt  ->
        function
        | `RecBind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" self#ident _a0
              self#ep _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#rec_bind _a0
              self#rec_bind _a1
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method fanloc_t : 'fmt -> FanLoc.t -> unit= self#unknown
    method fanutil_anti_cxt : 'fmt -> FanUtil.anti_cxt -> unit= self#unknown
  end

class map =
  object (self : 'self_type)
    inherit  mapbase
    method loc : loc -> loc= fun _a0  -> self#fanloc_t _a0
    method ant : ant -> ant=
      fun (`Ant (_a0,_a1))  ->
        let _a0 = self#loc _a0 in
        let _a1 = self#fanutil_anti_cxt _a1 in `Ant (_a0, _a1)
    method nil : nil -> nil= fun `Nil  -> `Nil
    method literal : literal -> literal=
      function
      | `Chr _a0 -> let _a0 = self#string _a0 in `Chr _a0
      | `Int _a0 -> let _a0 = self#string _a0 in `Int _a0
      | `Int32 _a0 -> let _a0 = self#string _a0 in `Int32 _a0
      | `Int64 _a0 -> let _a0 = self#string _a0 in `Int64 _a0
      | `Flo _a0 -> let _a0 = self#string _a0 in `Flo _a0
      | `Nativeint _a0 -> let _a0 = self#string _a0 in `Nativeint _a0
      | `Str _a0 -> let _a0 = self#string _a0 in `Str _a0
    method flag : flag -> flag=
      function
      | `Positive -> `Positive
      | `Negative -> `Negative
      | #ant as _a0 -> (self#ant _a0 : ant  :>flag)
    method position_flag : position_flag -> position_flag=
      function
      | `Positive -> `Positive
      | `Negative -> `Negative
      | `Normal -> `Normal
      | #ant as _a0 -> (self#ant _a0 : ant  :>position_flag)
    method strings : strings -> strings=
      function
      | `App (_a0,_a1) ->
          let _a0 = self#strings _a0 in
          let _a1 = self#strings _a1 in `App (_a0, _a1)
      | `Str _a0 -> let _a0 = self#string _a0 in `Str _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>strings)
    method lident : lident -> lident=
      fun (`Lid _a0)  -> let _a0 = self#string _a0 in `Lid _a0
    method alident : alident -> alident=
      function
      | `Lid _a0 -> let _a0 = self#string _a0 in `Lid _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>alident)
    method auident : auident -> auident=
      function
      | `Uid _a0 -> let _a0 = self#string _a0 in `Uid _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>auident)
    method aident : aident -> aident=
      function
      | #alident as _a0 -> (self#alident _a0 : alident  :>aident)
      | #auident as _a0 -> (self#auident _a0 : auident  :>aident)
    method astring : astring -> astring=
      function
      | `C _a0 -> let _a0 = self#string _a0 in `C _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>astring)
    method uident : uident -> uident=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#uident _a0 in
          let _a1 = self#uident _a1 in `Dot (_a0, _a1)
      | `App (_a0,_a1) ->
          let _a0 = self#uident _a0 in
          let _a1 = self#uident _a1 in `App (_a0, _a1)
      | #auident as _a0 -> (self#auident _a0 : auident  :>uident)
    method ident : ident -> ident=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ident _a1 in `Dot (_a0, _a1)
      | `Apply (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ident _a1 in `Apply (_a0, _a1)
      | #alident as _a0 -> (self#alident _a0 : alident  :>ident)
      | #auident as _a0 -> (self#auident _a0 : auident  :>ident)
    method ident' : ident' -> ident'=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ident _a1 in `Dot (_a0, _a1)
      | `Apply (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ident _a1 in `Apply (_a0, _a1)
      | `Lid _a0 -> let _a0 = self#string _a0 in `Lid _a0
      | `Uid _a0 -> let _a0 = self#string _a0 in `Uid _a0
    method vid : vid -> vid=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#vid _a0 in let _a1 = self#vid _a1 in `Dot (_a0, _a1)
      | `Lid _a0 -> let _a0 = self#string _a0 in `Lid _a0
      | `Uid _a0 -> let _a0 = self#string _a0 in `Uid _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>vid)
    method vid' : vid' -> vid'=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#vid _a0 in let _a1 = self#vid _a1 in `Dot (_a0, _a1)
      | `Lid _a0 -> let _a0 = self#string _a0 in `Lid _a0
      | `Uid _a0 -> let _a0 = self#string _a0 in `Uid _a0
    method dupath : dupath -> dupath=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#dupath _a0 in
          let _a1 = self#dupath _a1 in `Dot (_a0, _a1)
      | #auident as _a0 -> (self#auident _a0 : auident  :>dupath)
    method dlpath : dlpath -> dlpath=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#dupath _a0 in
          let _a1 = self#alident _a1 in `Dot (_a0, _a1)
      | #alident as _a0 -> (self#alident _a0 : alident  :>dlpath)
    method any : any -> any= fun `Any  -> `Any
    method ctyp : ctyp -> ctyp=
      function
      | `Alias (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#alident _a1 in `Alias (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>ctyp)
      | `App (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `App (_a0, _a1)
      | `Arrow (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `Arrow (_a0, _a1)
      | `ClassPath _a0 -> let _a0 = self#ident _a0 in `ClassPath _a0
      | `Label (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in `Label (_a0, _a1)
      | `OptLabl (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in `OptLabl (_a0, _a1)
      | #ident' as _a0 -> (self#ident' _a0 : ident'  :>ctyp)
      | `TyObj (_a0,_a1) ->
          let _a0 = self#name_ctyp _a0 in
          let _a1 = self#flag _a1 in `TyObj (_a0, _a1)
      | `TyObjEnd _a0 -> let _a0 = self#flag _a0 in `TyObjEnd _a0
      | `TyPol (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `TyPol (_a0, _a1)
      | `TyPolEnd _a0 -> let _a0 = self#ctyp _a0 in `TyPolEnd _a0
      | `TyTypePol (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `TyTypePol (_a0, _a1)
      | `Quote (_a0,_a1) ->
          let _a0 = self#position_flag _a0 in
          let _a1 = self#alident _a1 in `Quote (_a0, _a1)
      | `QuoteAny _a0 -> let _a0 = self#position_flag _a0 in `QuoteAny _a0
      | `Par _a0 -> let _a0 = self#ctyp _a0 in `Par _a0
      | `Sta (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `Sta (_a0, _a1)
      | `PolyEq _a0 -> let _a0 = self#row_field _a0 in `PolyEq _a0
      | `PolySup _a0 -> let _a0 = self#row_field _a0 in `PolySup _a0
      | `PolyInf _a0 -> let _a0 = self#row_field _a0 in `PolyInf _a0
      | `Com (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `Com (_a0, _a1)
      | `PolyInfSup (_a0,_a1) ->
          let _a0 = self#row_field _a0 in
          let _a1 = self#tag_names _a1 in `PolyInfSup (_a0, _a1)
      | `Package _a0 -> let _a0 = self#mtyp _a0 in `Package _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>ctyp)
    method type_parameters : type_parameters -> type_parameters=
      function
      | `Com (_a0,_a1) ->
          let _a0 = self#type_parameters _a0 in
          let _a1 = self#type_parameters _a1 in `Com (_a0, _a1)
      | `Ctyp _a0 -> let _a0 = self#ctyp _a0 in `Ctyp _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_parameters)
    method row_field : row_field -> row_field=
      function
      | #ant as _a0 -> (self#ant _a0 : ant  :>row_field)
      | `Bar (_a0,_a1) ->
          let _a0 = self#row_field _a0 in
          let _a1 = self#row_field _a1 in `Bar (_a0, _a1)
      | `TyVrn _a0 -> let _a0 = self#astring _a0 in `TyVrn _a0
      | `TyVrnOf (_a0,_a1) ->
          let _a0 = self#astring _a0 in
          let _a1 = self#ctyp _a1 in `TyVrnOf (_a0, _a1)
      | `Ctyp _a0 -> let _a0 = self#ctyp _a0 in `Ctyp _a0
    method tag_names : tag_names -> tag_names=
      function
      | #ant as _a0 -> (self#ant _a0 : ant  :>tag_names)
      | `App (_a0,_a1) ->
          let _a0 = self#tag_names _a0 in
          let _a1 = self#tag_names _a1 in `App (_a0, _a1)
      | `TyVrn _a0 -> let _a0 = self#astring _a0 in `TyVrn _a0
    method typedecl : typedecl -> typedecl=
      function
      | `TyDcl (_a0,_a1,_a2,_a3) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#opt_decl_params _a1 in
          let _a2 = self#type_info _a2 in
          let _a3 = self#opt_type_constr _a3 in `TyDcl (_a0, _a1, _a2, _a3)
      | `TyAbstr (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#opt_decl_params _a1 in
          let _a2 = self#opt_type_constr _a2 in `TyAbstr (_a0, _a1, _a2)
      | `And (_a0,_a1) ->
          let _a0 = self#typedecl _a0 in
          let _a1 = self#typedecl _a1 in `And (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>typedecl)
    method type_constr : type_constr -> type_constr=
      function
      | `And (_a0,_a1) ->
          let _a0 = self#type_constr _a0 in
          let _a1 = self#type_constr _a1 in `And (_a0, _a1)
      | `Eq (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `Eq (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_constr)
    method opt_type_constr : opt_type_constr -> opt_type_constr=
      function
      | `Some _a0 -> let _a0 = self#type_constr _a0 in `Some _a0
      | `None -> `None
    method decl_param : decl_param -> decl_param=
      function
      | `Quote (_a0,_a1) ->
          let _a0 = self#position_flag _a0 in
          let _a1 = self#alident _a1 in `Quote (_a0, _a1)
      | `QuoteAny _a0 -> let _a0 = self#position_flag _a0 in `QuoteAny _a0
      | `Any -> `Any
      | #ant as _a0 -> (self#ant _a0 : ant  :>decl_param)
    method decl_params : decl_params -> decl_params=
      function
      | `Quote (_a0,_a1) ->
          let _a0 = self#position_flag _a0 in
          let _a1 = self#alident _a1 in `Quote (_a0, _a1)
      | `QuoteAny _a0 -> let _a0 = self#position_flag _a0 in `QuoteAny _a0
      | `Any -> `Any
      | `Com (_a0,_a1) ->
          let _a0 = self#decl_params _a0 in
          let _a1 = self#decl_params _a1 in `Com (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>decl_params)
    method opt_decl_params : opt_decl_params -> opt_decl_params=
      function
      | `Some _a0 -> let _a0 = self#decl_params _a0 in `Some _a0
      | `None -> `None
    method type_info : type_info -> type_info=
      function
      | `TyMan (_a0,_a1,_a2) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#type_repr _a2 in `TyMan (_a0, _a1, _a2)
      | `TyRepr (_a0,_a1) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#type_repr _a1 in `TyRepr (_a0, _a1)
      | `TyEq (_a0,_a1) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ctyp _a1 in `TyEq (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_info)
    method type_repr : type_repr -> type_repr=
      function
      | `Record _a0 -> let _a0 = self#name_ctyp _a0 in `Record _a0
      | `Sum _a0 -> let _a0 = self#or_ctyp _a0 in `Sum _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_repr)
    method name_ctyp : name_ctyp -> name_ctyp=
      function
      | `Sem (_a0,_a1) ->
          let _a0 = self#name_ctyp _a0 in
          let _a1 = self#name_ctyp _a1 in `Sem (_a0, _a1)
      | `TyCol (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in `TyCol (_a0, _a1)
      | `TyColMut (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in `TyColMut (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>name_ctyp)
    method or_ctyp : or_ctyp -> or_ctyp=
      function
      | `Bar (_a0,_a1) ->
          let _a0 = self#or_ctyp _a0 in
          let _a1 = self#or_ctyp _a1 in `Bar (_a0, _a1)
      | `TyCol (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#ctyp _a1 in `TyCol (_a0, _a1)
      | `Of (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#ctyp _a1 in `Of (_a0, _a1)
      | #auident as _a0 -> (self#auident _a0 : auident  :>or_ctyp)
    method of_ctyp : of_ctyp -> of_ctyp=
      function
      | `Of (_a0,_a1) ->
          let _a0 = self#vid _a0 in let _a1 = self#ctyp _a1 in `Of (_a0, _a1)
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>of_ctyp)
      | #ant as _a0 -> (self#ant _a0 : ant  :>of_ctyp)
    method pat : pat -> pat=
      function
      | #vid as _a0 -> (self#vid _a0 : vid  :>pat)
      | `App (_a0,_a1) ->
          let _a0 = self#pat _a0 in let _a1 = self#pat _a1 in `App (_a0, _a1)
      | `Vrn _a0 -> let _a0 = self#string _a0 in `Vrn _a0
      | `Com (_a0,_a1) ->
          let _a0 = self#pat _a0 in let _a1 = self#pat _a1 in `Com (_a0, _a1)
      | `Sem (_a0,_a1) ->
          let _a0 = self#pat _a0 in let _a1 = self#pat _a1 in `Sem (_a0, _a1)
      | `Par _a0 -> let _a0 = self#pat _a0 in `Par _a0
      | #any as _a0 -> (self#any _a0 : any  :>pat)
      | `Record _a0 -> let _a0 = self#rec_pat _a0 in `Record _a0
      | #literal as _a0 -> (self#literal _a0 : literal  :>pat)
      | `Alias (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#alident _a1 in `Alias (_a0, _a1)
      | `ArrayEmpty -> `ArrayEmpty
      | `Array _a0 -> let _a0 = self#pat _a0 in `Array _a0
      | `LabelS _a0 -> let _a0 = self#alident _a0 in `LabelS _a0
      | `Label (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#pat _a1 in `Label (_a0, _a1)
      | `OptLabl (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#pat _a1 in `OptLabl (_a0, _a1)
      | `OptLablS _a0 -> let _a0 = self#alident _a0 in `OptLablS _a0
      | `OptLablExpr (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#exp _a2 in `OptLablExpr (_a0, _a1, _a2)
      | `Bar (_a0,_a1) ->
          let _a0 = self#pat _a0 in let _a1 = self#pat _a1 in `Bar (_a0, _a1)
      | `PaRng (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#pat _a1 in `PaRng (_a0, _a1)
      | `Constraint (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#ctyp _a1 in `Constraint (_a0, _a1)
      | `ClassPath _a0 -> let _a0 = self#ident _a0 in `ClassPath _a0
      | `Lazy _a0 -> let _a0 = self#pat _a0 in `Lazy _a0
      | `ModuleUnpack _a0 -> let _a0 = self#auident _a0 in `ModuleUnpack _a0
      | `ModuleConstraint (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#ctyp _a1 in `ModuleConstraint (_a0, _a1)
    method rec_pat : rec_pat -> rec_pat=
      function
      | `RecBind (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#pat _a1 in `RecBind (_a0, _a1)
      | `Sem (_a0,_a1) ->
          let _a0 = self#rec_pat _a0 in
          let _a1 = self#rec_pat _a1 in `Sem (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>rec_pat)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_pat)
    method exp : exp -> exp=
      function
      | #vid as _a0 -> (self#vid _a0 : vid  :>exp)
      | `App (_a0,_a1) ->
          let _a0 = self#exp _a0 in let _a1 = self#exp _a1 in `App (_a0, _a1)
      | `Vrn _a0 -> let _a0 = self#string _a0 in `Vrn _a0
      | `Com (_a0,_a1) ->
          let _a0 = self#exp _a0 in let _a1 = self#exp _a1 in `Com (_a0, _a1)
      | `Sem (_a0,_a1) ->
          let _a0 = self#exp _a0 in let _a1 = self#exp _a1 in `Sem (_a0, _a1)
      | `Par _a0 -> let _a0 = self#exp _a0 in `Par _a0
      | #any as _a0 -> (self#any _a0 : any  :>exp)
      | `Record _a0 -> let _a0 = self#rec_exp _a0 in `Record _a0
      | #literal as _a0 -> (self#literal _a0 : literal  :>exp)
      | `RecordWith (_a0,_a1) ->
          let _a0 = self#rec_exp _a0 in
          let _a1 = self#exp _a1 in `RecordWith (_a0, _a1)
      | `Field (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in `Field (_a0, _a1)
      | `ArrayDot (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in `ArrayDot (_a0, _a1)
      | `ArrayEmpty -> `ArrayEmpty
      | `Array _a0 -> let _a0 = self#exp _a0 in `Array _a0
      | `Assert _a0 -> let _a0 = self#exp _a0 in `Assert _a0
      | `Assign (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in `Assign (_a0, _a1)
      | `For (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in
          let _a3 = self#flag _a3 in
          let _a4 = self#exp _a4 in `For (_a0, _a1, _a2, _a3, _a4)
      | `Fun _a0 -> let _a0 = self#case _a0 in `Fun _a0
      | `IfThenElse (_a0,_a1,_a2) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `IfThenElse (_a0, _a1, _a2)
      | `IfThen (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in `IfThen (_a0, _a1)
      | `LabelS _a0 -> let _a0 = self#alident _a0 in `LabelS _a0
      | `Label (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#exp _a1 in `Label (_a0, _a1)
      | `Lazy _a0 -> let _a0 = self#exp _a0 in `Lazy _a0
      | `LetIn (_a0,_a1,_a2) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#bind _a1 in
          let _a2 = self#exp _a2 in `LetIn (_a0, _a1, _a2)
      | `LetTryInWith (_a0,_a1,_a2,_a3) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#bind _a1 in
          let _a2 = self#exp _a2 in
          let _a3 = self#case _a3 in `LetTryInWith (_a0, _a1, _a2, _a3)
      | `LetModule (_a0,_a1,_a2) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mexp _a1 in
          let _a2 = self#exp _a2 in `LetModule (_a0, _a1, _a2)
      | `Match (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#case _a1 in `Match (_a0, _a1)
      | `New _a0 -> let _a0 = self#ident _a0 in `New _a0
      | `Obj _a0 -> let _a0 = self#clfield _a0 in `Obj _a0
      | `ObjEnd -> `ObjEnd
      | `ObjPat (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#clfield _a1 in `ObjPat (_a0, _a1)
      | `ObjPatEnd _a0 -> let _a0 = self#pat _a0 in `ObjPatEnd _a0
      | `OptLabl (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#exp _a1 in `OptLabl (_a0, _a1)
      | `OptLablS _a0 -> let _a0 = self#alident _a0 in `OptLablS _a0
      | `OvrInst _a0 -> let _a0 = self#rec_exp _a0 in `OvrInst _a0
      | `OvrInstEmpty -> `OvrInstEmpty
      | `Seq _a0 -> let _a0 = self#exp _a0 in `Seq _a0
      | `Send (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#alident _a1 in `Send (_a0, _a1)
      | `StringDot (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in `StringDot (_a0, _a1)
      | `Try (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#case _a1 in `Try (_a0, _a1)
      | `Constraint (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#ctyp _a1 in `Constraint (_a0, _a1)
      | `Coercion (_a0,_a1,_a2) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Coercion (_a0, _a1, _a2)
      | `Subtype (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#ctyp _a1 in `Subtype (_a0, _a1)
      | `While (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in `While (_a0, _a1)
      | `LetOpen (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#exp _a1 in `LetOpen (_a0, _a1)
      | `LocalTypeFun (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#exp _a1 in `LocalTypeFun (_a0, _a1)
      | `Package_exp _a0 -> let _a0 = self#mexp _a0 in `Package_exp _a0
    method rec_exp : rec_exp -> rec_exp=
      function
      | `Sem (_a0,_a1) ->
          let _a0 = self#rec_exp _a0 in
          let _a1 = self#rec_exp _a1 in `Sem (_a0, _a1)
      | `RecBind (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#exp _a1 in `RecBind (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>rec_exp)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_exp)
    method mtyp : mtyp -> mtyp=
      function
      | #ident' as _a0 -> (self#ident' _a0 : ident'  :>mtyp)
      | `Sig _a0 -> let _a0 = self#sigi _a0 in `Sig _a0
      | `SigEnd -> `SigEnd
      | `Functor (_a0,_a1,_a2) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in
          let _a2 = self#mtyp _a2 in `Functor (_a0, _a1, _a2)
      | `With (_a0,_a1) ->
          let _a0 = self#mtyp _a0 in
          let _a1 = self#constr _a1 in `With (_a0, _a1)
      | `ModuleTypeOf _a0 -> let _a0 = self#mexp _a0 in `ModuleTypeOf _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>mtyp)
    method sigi : sigi -> sigi=
      function
      | `Val (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in `Val (_a0, _a1)
      | `External (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#strings _a2 in `External (_a0, _a1, _a2)
      | `Type _a0 -> let _a0 = self#typedecl _a0 in `Type _a0
      | `Exception _a0 -> let _a0 = self#of_ctyp _a0 in `Exception _a0
      | `Class _a0 -> let _a0 = self#cltdecl _a0 in `Class _a0
      | `ClassType _a0 -> let _a0 = self#cltdecl _a0 in `ClassType _a0
      | `Module (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in `Module (_a0, _a1)
      | `ModuleTypeEnd _a0 ->
          let _a0 = self#auident _a0 in `ModuleTypeEnd _a0
      | `ModuleType (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in `ModuleType (_a0, _a1)
      | `Sem (_a0,_a1) ->
          let _a0 = self#sigi _a0 in
          let _a1 = self#sigi _a1 in `Sem (_a0, _a1)
      | `DirectiveSimple _a0 ->
          let _a0 = self#alident _a0 in `DirectiveSimple _a0
      | `Directive (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#exp _a1 in `Directive (_a0, _a1)
      | `Open _a0 -> let _a0 = self#ident _a0 in `Open _a0
      | `Include _a0 -> let _a0 = self#mtyp _a0 in `Include _a0
      | `RecModule _a0 -> let _a0 = self#mbind _a0 in `RecModule _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>sigi)
    method mbind : mbind -> mbind=
      function
      | `And (_a0,_a1) ->
          let _a0 = self#mbind _a0 in
          let _a1 = self#mbind _a1 in `And (_a0, _a1)
      | `ModuleBind (_a0,_a1,_a2) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in
          let _a2 = self#mexp _a2 in `ModuleBind (_a0, _a1, _a2)
      | `Constraint (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in `Constraint (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>mbind)
    method constr : constr -> constr=
      function
      | `TypeEq (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `TypeEq (_a0, _a1)
      | `ModuleEq (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ident _a1 in `ModuleEq (_a0, _a1)
      | `TypeEqPriv (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `TypeEqPriv (_a0, _a1)
      | `TypeSubst (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `TypeSubst (_a0, _a1)
      | `ModuleSubst (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ident _a1 in `ModuleSubst (_a0, _a1)
      | `And (_a0,_a1) ->
          let _a0 = self#constr _a0 in
          let _a1 = self#constr _a1 in `And (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>constr)
    method bind : bind -> bind=
      function
      | `And (_a0,_a1) ->
          let _a0 = self#bind _a0 in
          let _a1 = self#bind _a1 in `And (_a0, _a1)
      | `Bind (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#exp _a1 in `Bind (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>bind)
    method case : case -> case=
      function
      | `Bar (_a0,_a1) ->
          let _a0 = self#case _a0 in
          let _a1 = self#case _a1 in `Bar (_a0, _a1)
      | `Case (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#exp _a1 in `Case (_a0, _a1)
      | `CaseWhen (_a0,_a1,_a2) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `CaseWhen (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>case)
    method mexp : mexp -> mexp=
      function
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>mexp)
      | `App (_a0,_a1) ->
          let _a0 = self#mexp _a0 in
          let _a1 = self#mexp _a1 in `App (_a0, _a1)
      | `Functor (_a0,_a1,_a2) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in
          let _a2 = self#mexp _a2 in `Functor (_a0, _a1, _a2)
      | `Struct _a0 -> let _a0 = self#stru _a0 in `Struct _a0
      | `StructEnd -> `StructEnd
      | `Constraint (_a0,_a1) ->
          let _a0 = self#mexp _a0 in
          let _a1 = self#mtyp _a1 in `Constraint (_a0, _a1)
      | `PackageModule _a0 -> let _a0 = self#exp _a0 in `PackageModule _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>mexp)
    method stru : stru -> stru=
      function
      | `Class _a0 -> let _a0 = self#cldecl _a0 in `Class _a0
      | `ClassType _a0 -> let _a0 = self#cltdecl _a0 in `ClassType _a0
      | `Sem (_a0,_a1) ->
          let _a0 = self#stru _a0 in
          let _a1 = self#stru _a1 in `Sem (_a0, _a1)
      | `DirectiveSimple _a0 ->
          let _a0 = self#alident _a0 in `DirectiveSimple _a0
      | `Directive (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#exp _a1 in `Directive (_a0, _a1)
      | `Exception _a0 -> let _a0 = self#of_ctyp _a0 in `Exception _a0
      | `StExp _a0 -> let _a0 = self#exp _a0 in `StExp _a0
      | `External (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#strings _a2 in `External (_a0, _a1, _a2)
      | `Include _a0 -> let _a0 = self#mexp _a0 in `Include _a0
      | `Module (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mexp _a1 in `Module (_a0, _a1)
      | `RecModule _a0 -> let _a0 = self#mbind _a0 in `RecModule _a0
      | `ModuleType (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in `ModuleType (_a0, _a1)
      | `Open _a0 -> let _a0 = self#ident _a0 in `Open _a0
      | `Type _a0 -> let _a0 = self#typedecl _a0 in `Type _a0
      | `TypeWith (_a0,_a1) ->
          let _a0 = self#typedecl _a0 in
          let _a1 = self#strings _a1 in `TypeWith (_a0, _a1)
      | `Value (_a0,_a1) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#bind _a1 in `Value (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>stru)
    method cltdecl : cltdecl -> cltdecl=
      function
      | `And (_a0,_a1) ->
          let _a0 = self#cltdecl _a0 in
          let _a1 = self#cltdecl _a1 in `And (_a0, _a1)
      | `CtDecl (_a0,_a1,_a2,_a3) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#type_parameters _a2 in
          let _a3 = self#cltyp _a3 in `CtDecl (_a0, _a1, _a2, _a3)
      | `CtDeclS (_a0,_a1,_a2) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#cltyp _a2 in `CtDeclS (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cltdecl)
    method cltyp : cltyp -> cltyp=
      function
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>cltyp)
      | `ClApply (_a0,_a1) ->
          let _a0 = self#vid _a0 in
          let _a1 = self#type_parameters _a1 in `ClApply (_a0, _a1)
      | `CtFun (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#cltyp _a1 in `CtFun (_a0, _a1)
      | `ObjTy (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#clsigi _a1 in `ObjTy (_a0, _a1)
      | `ObjTyEnd _a0 -> let _a0 = self#ctyp _a0 in `ObjTyEnd _a0
      | `Obj _a0 -> let _a0 = self#clsigi _a0 in `Obj _a0
      | `ObjEnd -> `ObjEnd
      | `And (_a0,_a1) ->
          let _a0 = self#cltyp _a0 in
          let _a1 = self#cltyp _a1 in `And (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cltyp)
    method clsigi : clsigi -> clsigi=
      function
      | `Sem (_a0,_a1) ->
          let _a0 = self#clsigi _a0 in
          let _a1 = self#clsigi _a1 in `Sem (_a0, _a1)
      | `SigInherit _a0 -> let _a0 = self#cltyp _a0 in `SigInherit _a0
      | `CgVal (_a0,_a1,_a2,_a3) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#ctyp _a3 in `CgVal (_a0, _a1, _a2, _a3)
      | `Method (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ctyp _a2 in `Method (_a0, _a1, _a2)
      | `VirMeth (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ctyp _a2 in `VirMeth (_a0, _a1, _a2)
      | `Eq (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `Eq (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>clsigi)
    method cldecl : cldecl -> cldecl=
      function
      | `ClDecl (_a0,_a1,_a2,_a3) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#type_parameters _a2 in
          let _a3 = self#clexp _a3 in `ClDecl (_a0, _a1, _a2, _a3)
      | `ClDeclS (_a0,_a1,_a2) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#clexp _a2 in `ClDeclS (_a0, _a1, _a2)
      | `And (_a0,_a1) ->
          let _a0 = self#cldecl _a0 in
          let _a1 = self#cldecl _a1 in `And (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cldecl)
    method clexp : clexp -> clexp=
      function
      | `CeApp (_a0,_a1) ->
          let _a0 = self#clexp _a0 in
          let _a1 = self#exp _a1 in `CeApp (_a0, _a1)
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>clexp)
      | `ClApply (_a0,_a1) ->
          let _a0 = self#vid _a0 in
          let _a1 = self#type_parameters _a1 in `ClApply (_a0, _a1)
      | `CeFun (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#clexp _a1 in `CeFun (_a0, _a1)
      | `LetIn (_a0,_a1,_a2) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#bind _a1 in
          let _a2 = self#clexp _a2 in `LetIn (_a0, _a1, _a2)
      | `Obj _a0 -> let _a0 = self#clfield _a0 in `Obj _a0
      | `ObjEnd -> `ObjEnd
      | `ObjPat (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#clfield _a1 in `ObjPat (_a0, _a1)
      | `ObjPatEnd _a0 -> let _a0 = self#pat _a0 in `ObjPatEnd _a0
      | `Constraint (_a0,_a1) ->
          let _a0 = self#clexp _a0 in
          let _a1 = self#cltyp _a1 in `Constraint (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>clexp)
    method clfield : clfield -> clfield=
      function
      | `Sem (_a0,_a1) ->
          let _a0 = self#clfield _a0 in
          let _a1 = self#clfield _a1 in `Sem (_a0, _a1)
      | `Inherit (_a0,_a1) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#clexp _a1 in `Inherit (_a0, _a1)
      | `InheritAs (_a0,_a1,_a2) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#clexp _a1 in
          let _a2 = self#alident _a2 in `InheritAs (_a0, _a1, _a2)
      | `CrVal (_a0,_a1,_a2,_a3) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#exp _a3 in `CrVal (_a0, _a1, _a2, _a3)
      | `VirVal (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ctyp _a2 in `VirVal (_a0, _a1, _a2)
      | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#exp _a3 in
          let _a4 = self#ctyp _a4 in `CrMth (_a0, _a1, _a2, _a3, _a4)
      | `CrMthS (_a0,_a1,_a2,_a3) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#exp _a3 in `CrMthS (_a0, _a1, _a2, _a3)
      | `VirMeth (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ctyp _a2 in `VirMeth (_a0, _a1, _a2)
      | `Eq (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `Eq (_a0, _a1)
      | `Initializer _a0 -> let _a0 = self#exp _a0 in `Initializer _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>clfield)
    method ep : ep -> ep=
      function
      | #vid as _a0 -> (self#vid _a0 : vid  :>ep)
      | `App (_a0,_a1) ->
          let _a0 = self#ep _a0 in let _a1 = self#ep _a1 in `App (_a0, _a1)
      | `Vrn _a0 -> let _a0 = self#string _a0 in `Vrn _a0
      | `Com (_a0,_a1) ->
          let _a0 = self#ep _a0 in let _a1 = self#ep _a1 in `Com (_a0, _a1)
      | `Sem (_a0,_a1) ->
          let _a0 = self#ep _a0 in let _a1 = self#ep _a1 in `Sem (_a0, _a1)
      | `Par _a0 -> let _a0 = self#ep _a0 in `Par _a0
      | #any as _a0 -> (self#any _a0 : any  :>ep)
      | `ArrayEmpty -> `ArrayEmpty
      | `Array _a0 -> let _a0 = self#ep _a0 in `Array _a0
      | `Record _a0 -> let _a0 = self#rec_bind _a0 in `Record _a0
      | #literal as _a0 -> (self#literal _a0 : literal  :>ep)
    method rec_bind : rec_bind -> rec_bind=
      function
      | `RecBind (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ep _a1 in `RecBind (_a0, _a1)
      | `Sem (_a0,_a1) ->
          let _a0 = self#rec_bind _a0 in
          let _a1 = self#rec_bind _a1 in `Sem (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>rec_bind)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_bind)
    method fanloc_t : FanLoc.t -> FanLoc.t= self#unknown
    method fanutil_anti_cxt : FanUtil.anti_cxt -> FanUtil.anti_cxt=
      self#unknown
  end

class fold =
  object (self : 'self_type)
    inherit  foldbase
    method loc : loc -> 'self_type= fun _a0  -> self#fanloc_t _a0
    method ant : ant -> 'self_type=
      fun (`Ant (_a0,_a1))  ->
        let self = self#loc _a0 in self#fanutil_anti_cxt _a1
    method nil : nil -> 'self_type= fun `Nil  -> self
    method literal : literal -> 'self_type=
      function
      | `Chr _a0 -> self#string _a0
      | `Int _a0 -> self#string _a0
      | `Int32 _a0 -> self#string _a0
      | `Int64 _a0 -> self#string _a0
      | `Flo _a0 -> self#string _a0
      | `Nativeint _a0 -> self#string _a0
      | `Str _a0 -> self#string _a0
    method flag : flag -> 'self_type=
      function
      | `Positive -> self
      | `Negative -> self
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method position_flag : position_flag -> 'self_type=
      function
      | `Positive -> self
      | `Negative -> self
      | `Normal -> self
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method strings : strings -> 'self_type=
      function
      | `App (_a0,_a1) -> let self = self#strings _a0 in self#strings _a1
      | `Str _a0 -> self#string _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method lident : lident -> 'self_type= fun (`Lid _a0)  -> self#string _a0
    method alident : alident -> 'self_type=
      function
      | `Lid _a0 -> self#string _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method auident : auident -> 'self_type=
      function
      | `Uid _a0 -> self#string _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method aident : aident -> 'self_type=
      function
      | #alident as _a0 -> (self#alident _a0 :>'self_type)
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method astring : astring -> 'self_type=
      function
      | `C _a0 -> self#string _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method uident : uident -> 'self_type=
      function
      | `Dot (_a0,_a1) -> let self = self#uident _a0 in self#uident _a1
      | `App (_a0,_a1) -> let self = self#uident _a0 in self#uident _a1
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method ident : ident -> 'self_type=
      function
      | `Dot (_a0,_a1) -> let self = self#ident _a0 in self#ident _a1
      | `Apply (_a0,_a1) -> let self = self#ident _a0 in self#ident _a1
      | #alident as _a0 -> (self#alident _a0 :>'self_type)
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method ident' : ident' -> 'self_type=
      function
      | `Dot (_a0,_a1) -> let self = self#ident _a0 in self#ident _a1
      | `Apply (_a0,_a1) -> let self = self#ident _a0 in self#ident _a1
      | `Lid _a0 -> self#string _a0
      | `Uid _a0 -> self#string _a0
    method vid : vid -> 'self_type=
      function
      | `Dot (_a0,_a1) -> let self = self#vid _a0 in self#vid _a1
      | `Lid _a0 -> self#string _a0
      | `Uid _a0 -> self#string _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method vid' : vid' -> 'self_type=
      function
      | `Dot (_a0,_a1) -> let self = self#vid _a0 in self#vid _a1
      | `Lid _a0 -> self#string _a0
      | `Uid _a0 -> self#string _a0
    method dupath : dupath -> 'self_type=
      function
      | `Dot (_a0,_a1) -> let self = self#dupath _a0 in self#dupath _a1
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method dlpath : dlpath -> 'self_type=
      function
      | `Dot (_a0,_a1) -> let self = self#dupath _a0 in self#alident _a1
      | #alident as _a0 -> (self#alident _a0 :>'self_type)
    method any : any -> 'self_type= fun `Any  -> self
    method ctyp : ctyp -> 'self_type=
      function
      | `Alias (_a0,_a1) -> let self = self#ctyp _a0 in self#alident _a1
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `App (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `Arrow (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `ClassPath _a0 -> self#ident _a0
      | `Label (_a0,_a1) -> let self = self#alident _a0 in self#ctyp _a1
      | `OptLabl (_a0,_a1) -> let self = self#alident _a0 in self#ctyp _a1
      | #ident' as _a0 -> (self#ident' _a0 :>'self_type)
      | `TyObj (_a0,_a1) -> let self = self#name_ctyp _a0 in self#flag _a1
      | `TyObjEnd _a0 -> self#flag _a0
      | `TyPol (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `TyPolEnd _a0 -> self#ctyp _a0
      | `TyTypePol (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `Quote (_a0,_a1) ->
          let self = self#position_flag _a0 in self#alident _a1
      | `QuoteAny _a0 -> self#position_flag _a0
      | `Par _a0 -> self#ctyp _a0
      | `Sta (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `PolyEq _a0 -> self#row_field _a0
      | `PolySup _a0 -> self#row_field _a0
      | `PolyInf _a0 -> self#row_field _a0
      | `Com (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `PolyInfSup (_a0,_a1) ->
          let self = self#row_field _a0 in self#tag_names _a1
      | `Package _a0 -> self#mtyp _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method type_parameters : type_parameters -> 'self_type=
      function
      | `Com (_a0,_a1) ->
          let self = self#type_parameters _a0 in self#type_parameters _a1
      | `Ctyp _a0 -> self#ctyp _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method row_field : row_field -> 'self_type=
      function
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | `Bar (_a0,_a1) -> let self = self#row_field _a0 in self#row_field _a1
      | `TyVrn _a0 -> self#astring _a0
      | `TyVrnOf (_a0,_a1) -> let self = self#astring _a0 in self#ctyp _a1
      | `Ctyp _a0 -> self#ctyp _a0
    method tag_names : tag_names -> 'self_type=
      function
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | `App (_a0,_a1) -> let self = self#tag_names _a0 in self#tag_names _a1
      | `TyVrn _a0 -> self#astring _a0
    method typedecl : typedecl -> 'self_type=
      function
      | `TyDcl (_a0,_a1,_a2,_a3) ->
          let self = self#alident _a0 in
          let self = self#opt_decl_params _a1 in
          let self = self#type_info _a2 in self#opt_type_constr _a3
      | `TyAbstr (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#opt_decl_params _a1 in self#opt_type_constr _a2
      | `And (_a0,_a1) -> let self = self#typedecl _a0 in self#typedecl _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method type_constr : type_constr -> 'self_type=
      function
      | `And (_a0,_a1) ->
          let self = self#type_constr _a0 in self#type_constr _a1
      | `Eq (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method opt_type_constr : opt_type_constr -> 'self_type=
      function | `Some _a0 -> self#type_constr _a0 | `None -> self
    method decl_param : decl_param -> 'self_type=
      function
      | `Quote (_a0,_a1) ->
          let self = self#position_flag _a0 in self#alident _a1
      | `QuoteAny _a0 -> self#position_flag _a0
      | `Any -> self
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method decl_params : decl_params -> 'self_type=
      function
      | `Quote (_a0,_a1) ->
          let self = self#position_flag _a0 in self#alident _a1
      | `QuoteAny _a0 -> self#position_flag _a0
      | `Any -> self
      | `Com (_a0,_a1) ->
          let self = self#decl_params _a0 in self#decl_params _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method opt_decl_params : opt_decl_params -> 'self_type=
      function | `Some _a0 -> self#decl_params _a0 | `None -> self
    method type_info : type_info -> 'self_type=
      function
      | `TyMan (_a0,_a1,_a2) ->
          let self = self#ctyp _a0 in
          let self = self#flag _a1 in self#type_repr _a2
      | `TyRepr (_a0,_a1) -> let self = self#flag _a0 in self#type_repr _a1
      | `TyEq (_a0,_a1) -> let self = self#flag _a0 in self#ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method type_repr : type_repr -> 'self_type=
      function
      | `Record _a0 -> self#name_ctyp _a0
      | `Sum _a0 -> self#or_ctyp _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method name_ctyp : name_ctyp -> 'self_type=
      function
      | `Sem (_a0,_a1) -> let self = self#name_ctyp _a0 in self#name_ctyp _a1
      | `TyCol (_a0,_a1) -> let self = self#alident _a0 in self#ctyp _a1
      | `TyColMut (_a0,_a1) -> let self = self#alident _a0 in self#ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method or_ctyp : or_ctyp -> 'self_type=
      function
      | `Bar (_a0,_a1) -> let self = self#or_ctyp _a0 in self#or_ctyp _a1
      | `TyCol (_a0,_a1) -> let self = self#auident _a0 in self#ctyp _a1
      | `Of (_a0,_a1) -> let self = self#auident _a0 in self#ctyp _a1
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method of_ctyp : of_ctyp -> 'self_type=
      function
      | `Of (_a0,_a1) -> let self = self#vid _a0 in self#ctyp _a1
      | #vid' as _a0 -> (self#vid' _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method pat : pat -> 'self_type=
      function
      | #vid as _a0 -> (self#vid _a0 :>'self_type)
      | `App (_a0,_a1) -> let self = self#pat _a0 in self#pat _a1
      | `Vrn _a0 -> self#string _a0
      | `Com (_a0,_a1) -> let self = self#pat _a0 in self#pat _a1
      | `Sem (_a0,_a1) -> let self = self#pat _a0 in self#pat _a1
      | `Par _a0 -> self#pat _a0
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `Record _a0 -> self#rec_pat _a0
      | #literal as _a0 -> (self#literal _a0 :>'self_type)
      | `Alias (_a0,_a1) -> let self = self#pat _a0 in self#alident _a1
      | `ArrayEmpty -> self
      | `Array _a0 -> self#pat _a0
      | `LabelS _a0 -> self#alident _a0
      | `Label (_a0,_a1) -> let self = self#alident _a0 in self#pat _a1
      | `OptLabl (_a0,_a1) -> let self = self#alident _a0 in self#pat _a1
      | `OptLablS _a0 -> self#alident _a0
      | `OptLablExpr (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#pat _a1 in self#exp _a2
      | `Bar (_a0,_a1) -> let self = self#pat _a0 in self#pat _a1
      | `PaRng (_a0,_a1) -> let self = self#pat _a0 in self#pat _a1
      | `Constraint (_a0,_a1) -> let self = self#pat _a0 in self#ctyp _a1
      | `ClassPath _a0 -> self#ident _a0
      | `Lazy _a0 -> self#pat _a0
      | `ModuleUnpack _a0 -> self#auident _a0
      | `ModuleConstraint (_a0,_a1) ->
          let self = self#auident _a0 in self#ctyp _a1
    method rec_pat : rec_pat -> 'self_type=
      function
      | `RecBind (_a0,_a1) -> let self = self#ident _a0 in self#pat _a1
      | `Sem (_a0,_a1) -> let self = self#rec_pat _a0 in self#rec_pat _a1
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method exp : exp -> 'self_type=
      function
      | #vid as _a0 -> (self#vid _a0 :>'self_type)
      | `App (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `Vrn _a0 -> self#string _a0
      | `Com (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `Sem (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `Par _a0 -> self#exp _a0
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `Record _a0 -> self#rec_exp _a0
      | #literal as _a0 -> (self#literal _a0 :>'self_type)
      | `RecordWith (_a0,_a1) -> let self = self#rec_exp _a0 in self#exp _a1
      | `Field (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `ArrayDot (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `ArrayEmpty -> self
      | `Array _a0 -> self#exp _a0
      | `Assert _a0 -> self#exp _a0
      | `Assign (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `For (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#alident _a0 in
          let self = self#exp _a1 in
          let self = self#exp _a2 in let self = self#flag _a3 in self#exp _a4
      | `Fun _a0 -> self#case _a0
      | `IfThenElse (_a0,_a1,_a2) ->
          let self = self#exp _a0 in let self = self#exp _a1 in self#exp _a2
      | `IfThen (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `LabelS _a0 -> self#alident _a0
      | `Label (_a0,_a1) -> let self = self#alident _a0 in self#exp _a1
      | `Lazy _a0 -> self#exp _a0
      | `LetIn (_a0,_a1,_a2) ->
          let self = self#flag _a0 in
          let self = self#bind _a1 in self#exp _a2
      | `LetTryInWith (_a0,_a1,_a2,_a3) ->
          let self = self#flag _a0 in
          let self = self#bind _a1 in
          let self = self#exp _a2 in self#case _a3
      | `LetModule (_a0,_a1,_a2) ->
          let self = self#auident _a0 in
          let self = self#mexp _a1 in self#exp _a2
      | `Match (_a0,_a1) -> let self = self#exp _a0 in self#case _a1
      | `New _a0 -> self#ident _a0
      | `Obj _a0 -> self#clfield _a0
      | `ObjEnd -> self
      | `ObjPat (_a0,_a1) -> let self = self#pat _a0 in self#clfield _a1
      | `ObjPatEnd _a0 -> self#pat _a0
      | `OptLabl (_a0,_a1) -> let self = self#alident _a0 in self#exp _a1
      | `OptLablS _a0 -> self#alident _a0
      | `OvrInst _a0 -> self#rec_exp _a0
      | `OvrInstEmpty -> self
      | `Seq _a0 -> self#exp _a0
      | `Send (_a0,_a1) -> let self = self#exp _a0 in self#alident _a1
      | `StringDot (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `Try (_a0,_a1) -> let self = self#exp _a0 in self#case _a1
      | `Constraint (_a0,_a1) -> let self = self#exp _a0 in self#ctyp _a1
      | `Coercion (_a0,_a1,_a2) ->
          let self = self#exp _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `Subtype (_a0,_a1) -> let self = self#exp _a0 in self#ctyp _a1
      | `While (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `LetOpen (_a0,_a1) -> let self = self#ident _a0 in self#exp _a1
      | `LocalTypeFun (_a0,_a1) ->
          let self = self#alident _a0 in self#exp _a1
      | `Package_exp _a0 -> self#mexp _a0
    method rec_exp : rec_exp -> 'self_type=
      function
      | `Sem (_a0,_a1) -> let self = self#rec_exp _a0 in self#rec_exp _a1
      | `RecBind (_a0,_a1) -> let self = self#ident _a0 in self#exp _a1
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method mtyp : mtyp -> 'self_type=
      function
      | #ident' as _a0 -> (self#ident' _a0 :>'self_type)
      | `Sig _a0 -> self#sigi _a0
      | `SigEnd -> self
      | `Functor (_a0,_a1,_a2) ->
          let self = self#auident _a0 in
          let self = self#mtyp _a1 in self#mtyp _a2
      | `With (_a0,_a1) -> let self = self#mtyp _a0 in self#constr _a1
      | `ModuleTypeOf _a0 -> self#mexp _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method sigi : sigi -> 'self_type=
      function
      | `Val (_a0,_a1) -> let self = self#alident _a0 in self#ctyp _a1
      | `External (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#ctyp _a1 in self#strings _a2
      | `Type _a0 -> self#typedecl _a0
      | `Exception _a0 -> self#of_ctyp _a0
      | `Class _a0 -> self#cltdecl _a0
      | `ClassType _a0 -> self#cltdecl _a0
      | `Module (_a0,_a1) -> let self = self#auident _a0 in self#mtyp _a1
      | `ModuleTypeEnd _a0 -> self#auident _a0
      | `ModuleType (_a0,_a1) -> let self = self#auident _a0 in self#mtyp _a1
      | `Sem (_a0,_a1) -> let self = self#sigi _a0 in self#sigi _a1
      | `DirectiveSimple _a0 -> self#alident _a0
      | `Directive (_a0,_a1) -> let self = self#alident _a0 in self#exp _a1
      | `Open _a0 -> self#ident _a0
      | `Include _a0 -> self#mtyp _a0
      | `RecModule _a0 -> self#mbind _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method mbind : mbind -> 'self_type=
      function
      | `And (_a0,_a1) -> let self = self#mbind _a0 in self#mbind _a1
      | `ModuleBind (_a0,_a1,_a2) ->
          let self = self#auident _a0 in
          let self = self#mtyp _a1 in self#mexp _a2
      | `Constraint (_a0,_a1) -> let self = self#auident _a0 in self#mtyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method constr : constr -> 'self_type=
      function
      | `TypeEq (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `ModuleEq (_a0,_a1) -> let self = self#ident _a0 in self#ident _a1
      | `TypeEqPriv (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `TypeSubst (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `ModuleSubst (_a0,_a1) -> let self = self#ident _a0 in self#ident _a1
      | `And (_a0,_a1) -> let self = self#constr _a0 in self#constr _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method bind : bind -> 'self_type=
      function
      | `And (_a0,_a1) -> let self = self#bind _a0 in self#bind _a1
      | `Bind (_a0,_a1) -> let self = self#pat _a0 in self#exp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method case : case -> 'self_type=
      function
      | `Bar (_a0,_a1) -> let self = self#case _a0 in self#case _a1
      | `Case (_a0,_a1) -> let self = self#pat _a0 in self#exp _a1
      | `CaseWhen (_a0,_a1,_a2) ->
          let self = self#pat _a0 in let self = self#exp _a1 in self#exp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method mexp : mexp -> 'self_type=
      function
      | #vid' as _a0 -> (self#vid' _a0 :>'self_type)
      | `App (_a0,_a1) -> let self = self#mexp _a0 in self#mexp _a1
      | `Functor (_a0,_a1,_a2) ->
          let self = self#auident _a0 in
          let self = self#mtyp _a1 in self#mexp _a2
      | `Struct _a0 -> self#stru _a0
      | `StructEnd -> self
      | `Constraint (_a0,_a1) -> let self = self#mexp _a0 in self#mtyp _a1
      | `PackageModule _a0 -> self#exp _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method stru : stru -> 'self_type=
      function
      | `Class _a0 -> self#cldecl _a0
      | `ClassType _a0 -> self#cltdecl _a0
      | `Sem (_a0,_a1) -> let self = self#stru _a0 in self#stru _a1
      | `DirectiveSimple _a0 -> self#alident _a0
      | `Directive (_a0,_a1) -> let self = self#alident _a0 in self#exp _a1
      | `Exception _a0 -> self#of_ctyp _a0
      | `StExp _a0 -> self#exp _a0
      | `External (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#ctyp _a1 in self#strings _a2
      | `Include _a0 -> self#mexp _a0
      | `Module (_a0,_a1) -> let self = self#auident _a0 in self#mexp _a1
      | `RecModule _a0 -> self#mbind _a0
      | `ModuleType (_a0,_a1) -> let self = self#auident _a0 in self#mtyp _a1
      | `Open _a0 -> self#ident _a0
      | `Type _a0 -> self#typedecl _a0
      | `TypeWith (_a0,_a1) ->
          let self = self#typedecl _a0 in self#strings _a1
      | `Value (_a0,_a1) -> let self = self#flag _a0 in self#bind _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method cltdecl : cltdecl -> 'self_type=
      function
      | `And (_a0,_a1) -> let self = self#cltdecl _a0 in self#cltdecl _a1
      | `CtDecl (_a0,_a1,_a2,_a3) ->
          let self = self#flag _a0 in
          let self = self#ident _a1 in
          let self = self#type_parameters _a2 in self#cltyp _a3
      | `CtDeclS (_a0,_a1,_a2) ->
          let self = self#flag _a0 in
          let self = self#ident _a1 in self#cltyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method cltyp : cltyp -> 'self_type=
      function
      | #vid' as _a0 -> (self#vid' _a0 :>'self_type)
      | `ClApply (_a0,_a1) ->
          let self = self#vid _a0 in self#type_parameters _a1
      | `CtFun (_a0,_a1) -> let self = self#ctyp _a0 in self#cltyp _a1
      | `ObjTy (_a0,_a1) -> let self = self#ctyp _a0 in self#clsigi _a1
      | `ObjTyEnd _a0 -> self#ctyp _a0
      | `Obj _a0 -> self#clsigi _a0
      | `ObjEnd -> self
      | `And (_a0,_a1) -> let self = self#cltyp _a0 in self#cltyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method clsigi : clsigi -> 'self_type=
      function
      | `Sem (_a0,_a1) -> let self = self#clsigi _a0 in self#clsigi _a1
      | `SigInherit _a0 -> self#cltyp _a0
      | `CgVal (_a0,_a1,_a2,_a3) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in
          let self = self#flag _a2 in self#ctyp _a3
      | `Method (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in self#ctyp _a2
      | `VirMeth (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in self#ctyp _a2
      | `Eq (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method cldecl : cldecl -> 'self_type=
      function
      | `ClDecl (_a0,_a1,_a2,_a3) ->
          let self = self#flag _a0 in
          let self = self#ident _a1 in
          let self = self#type_parameters _a2 in self#clexp _a3
      | `ClDeclS (_a0,_a1,_a2) ->
          let self = self#flag _a0 in
          let self = self#ident _a1 in self#clexp _a2
      | `And (_a0,_a1) -> let self = self#cldecl _a0 in self#cldecl _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method clexp : clexp -> 'self_type=
      function
      | `CeApp (_a0,_a1) -> let self = self#clexp _a0 in self#exp _a1
      | #vid' as _a0 -> (self#vid' _a0 :>'self_type)
      | `ClApply (_a0,_a1) ->
          let self = self#vid _a0 in self#type_parameters _a1
      | `CeFun (_a0,_a1) -> let self = self#pat _a0 in self#clexp _a1
      | `LetIn (_a0,_a1,_a2) ->
          let self = self#flag _a0 in
          let self = self#bind _a1 in self#clexp _a2
      | `Obj _a0 -> self#clfield _a0
      | `ObjEnd -> self
      | `ObjPat (_a0,_a1) -> let self = self#pat _a0 in self#clfield _a1
      | `ObjPatEnd _a0 -> self#pat _a0
      | `Constraint (_a0,_a1) -> let self = self#clexp _a0 in self#cltyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method clfield : clfield -> 'self_type=
      function
      | `Sem (_a0,_a1) -> let self = self#clfield _a0 in self#clfield _a1
      | `Inherit (_a0,_a1) -> let self = self#flag _a0 in self#clexp _a1
      | `InheritAs (_a0,_a1,_a2) ->
          let self = self#flag _a0 in
          let self = self#clexp _a1 in self#alident _a2
      | `CrVal (_a0,_a1,_a2,_a3) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in
          let self = self#flag _a2 in self#exp _a3
      | `VirVal (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in self#ctyp _a2
      | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in
          let self = self#flag _a2 in
          let self = self#exp _a3 in self#ctyp _a4
      | `CrMthS (_a0,_a1,_a2,_a3) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in
          let self = self#flag _a2 in self#exp _a3
      | `VirMeth (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in self#ctyp _a2
      | `Eq (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `Initializer _a0 -> self#exp _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method ep : ep -> 'self_type=
      function
      | #vid as _a0 -> (self#vid _a0 :>'self_type)
      | `App (_a0,_a1) -> let self = self#ep _a0 in self#ep _a1
      | `Vrn _a0 -> self#string _a0
      | `Com (_a0,_a1) -> let self = self#ep _a0 in self#ep _a1
      | `Sem (_a0,_a1) -> let self = self#ep _a0 in self#ep _a1
      | `Par _a0 -> self#ep _a0
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `ArrayEmpty -> self
      | `Array _a0 -> self#ep _a0
      | `Record _a0 -> self#rec_bind _a0
      | #literal as _a0 -> (self#literal _a0 :>'self_type)
    method rec_bind : rec_bind -> 'self_type=
      function
      | `RecBind (_a0,_a1) -> let self = self#ident _a0 in self#ep _a1
      | `Sem (_a0,_a1) -> let self = self#rec_bind _a0 in self#rec_bind _a1
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method fanloc_t : FanLoc.t -> 'self_type= self#unknown
    method fanutil_anti_cxt : FanUtil.anti_cxt -> 'self_type= self#unknown
  end

let map_loc f =
  object  inherit  map as super method! loc x = f (super#loc x) end

let map_ant f =
  object  inherit  map as super method! ant x = f (super#ant x) end

let map_nil f =
  object  inherit  map as super method! nil x = f (super#nil x) end

let map_literal f =
  object  inherit  map as super method! literal x = f (super#literal x) end

let map_flag f =
  object  inherit  map as super method! flag x = f (super#flag x) end

let map_position_flag f =
  object 
    inherit  map as super
    method! position_flag x = f (super#position_flag x)
  end

let map_strings f =
  object  inherit  map as super method! strings x = f (super#strings x) end

let map_lident f =
  object  inherit  map as super method! lident x = f (super#lident x) end

let map_alident f =
  object  inherit  map as super method! alident x = f (super#alident x) end

let map_auident f =
  object  inherit  map as super method! auident x = f (super#auident x) end

let map_aident f =
  object  inherit  map as super method! aident x = f (super#aident x) end

let map_astring f =
  object  inherit  map as super method! astring x = f (super#astring x) end

let map_uident f =
  object  inherit  map as super method! uident x = f (super#uident x) end

let map_ident f =
  object  inherit  map as super method! ident x = f (super#ident x) end

let map_ident' f =
  object  inherit  map as super method! ident' x = f (super#ident' x) end

let map_vid f =
  object  inherit  map as super method! vid x = f (super#vid x) end

let map_vid' f =
  object  inherit  map as super method! vid' x = f (super#vid' x) end

let map_dupath f =
  object  inherit  map as super method! dupath x = f (super#dupath x) end

let map_dlpath f =
  object  inherit  map as super method! dlpath x = f (super#dlpath x) end

let map_any f =
  object  inherit  map as super method! any x = f (super#any x) end

let map_ctyp f =
  object  inherit  map as super method! ctyp x = f (super#ctyp x) end

let map_type_parameters f =
  object 
    inherit  map as super
    method! type_parameters x = f (super#type_parameters x)
  end

let map_row_field f =
  object  inherit  map as super method! row_field x = f (super#row_field x)
  end

let map_tag_names f =
  object  inherit  map as super method! tag_names x = f (super#tag_names x)
  end

let map_typedecl f =
  object  inherit  map as super method! typedecl x = f (super#typedecl x) end

let map_type_constr f =
  object 
    inherit  map as super
    method! type_constr x = f (super#type_constr x)
  end

let map_opt_type_constr f =
  object 
    inherit  map as super
    method! opt_type_constr x = f (super#opt_type_constr x)
  end

let map_decl_param f =
  object  inherit  map as super method! decl_param x = f (super#decl_param x)
  end

let map_decl_params f =
  object 
    inherit  map as super
    method! decl_params x = f (super#decl_params x)
  end

let map_opt_decl_params f =
  object 
    inherit  map as super
    method! opt_decl_params x = f (super#opt_decl_params x)
  end

let map_type_info f =
  object  inherit  map as super method! type_info x = f (super#type_info x)
  end

let map_type_repr f =
  object  inherit  map as super method! type_repr x = f (super#type_repr x)
  end

let map_name_ctyp f =
  object  inherit  map as super method! name_ctyp x = f (super#name_ctyp x)
  end

let map_or_ctyp f =
  object  inherit  map as super method! or_ctyp x = f (super#or_ctyp x) end

let map_of_ctyp f =
  object  inherit  map as super method! of_ctyp x = f (super#of_ctyp x) end

let map_pat f =
  object  inherit  map as super method! pat x = f (super#pat x) end

let map_rec_pat f =
  object  inherit  map as super method! rec_pat x = f (super#rec_pat x) end

let map_exp f =
  object  inherit  map as super method! exp x = f (super#exp x) end

let map_rec_exp f =
  object  inherit  map as super method! rec_exp x = f (super#rec_exp x) end

let map_mtyp f =
  object  inherit  map as super method! mtyp x = f (super#mtyp x) end

let map_sigi f =
  object  inherit  map as super method! sigi x = f (super#sigi x) end

let map_mbind f =
  object  inherit  map as super method! mbind x = f (super#mbind x) end

let map_constr f =
  object  inherit  map as super method! constr x = f (super#constr x) end

let map_bind f =
  object  inherit  map as super method! bind x = f (super#bind x) end

let map_case f =
  object  inherit  map as super method! case x = f (super#case x) end

let map_mexp f =
  object  inherit  map as super method! mexp x = f (super#mexp x) end

let map_stru f =
  object  inherit  map as super method! stru x = f (super#stru x) end

let map_cltdecl f =
  object  inherit  map as super method! cltdecl x = f (super#cltdecl x) end

let map_cltyp f =
  object  inherit  map as super method! cltyp x = f (super#cltyp x) end

let map_clsigi f =
  object  inherit  map as super method! clsigi x = f (super#clsigi x) end

let map_cldecl f =
  object  inherit  map as super method! cldecl x = f (super#cldecl x) end

let map_clexp f =
  object  inherit  map as super method! clexp x = f (super#clexp x) end

let map_clfield f =
  object  inherit  map as super method! clfield x = f (super#clfield x) end

let map_ep f =
  object  inherit  map as super method! ep x = f (super#ep x) end

let map_rec_bind f =
  object  inherit  map as super method! rec_bind x = f (super#rec_bind x) end

let dump = new print

let dump_literal = LibUtil.to_string_of_printer dump#literal

let dump_flag = LibUtil.to_string_of_printer dump#flag

let dump_position_flag = LibUtil.to_string_of_printer dump#position_flag

let dump_strings = LibUtil.to_string_of_printer dump#strings

let dump_lident = LibUtil.to_string_of_printer dump#lident

let dump_alident = LibUtil.to_string_of_printer dump#alident

let dump_auident = LibUtil.to_string_of_printer dump#auident

let dump_aident = LibUtil.to_string_of_printer dump#aident

let dump_astring = LibUtil.to_string_of_printer dump#astring

let dump_uident = LibUtil.to_string_of_printer dump#uident

let dump_ident = LibUtil.to_string_of_printer dump#ident

let dump_ident' = LibUtil.to_string_of_printer dump#ident'

let dump_vid = LibUtil.to_string_of_printer dump#vid

let dump_vid' = LibUtil.to_string_of_printer dump#vid'

let dump_dupath = LibUtil.to_string_of_printer dump#dupath

let dump_dlpath = LibUtil.to_string_of_printer dump#dlpath

let dump_any = LibUtil.to_string_of_printer dump#any

let dump_ctyp = LibUtil.to_string_of_printer dump#ctyp

let dump_type_parameters = LibUtil.to_string_of_printer dump#type_parameters

let dump_row_field = LibUtil.to_string_of_printer dump#row_field

let dump_tag_names = LibUtil.to_string_of_printer dump#tag_names

let dump_typedecl = LibUtil.to_string_of_printer dump#typedecl

let dump_type_constr = LibUtil.to_string_of_printer dump#type_constr

let dump_opt_type_constr = LibUtil.to_string_of_printer dump#opt_type_constr

let dump_decl_param = LibUtil.to_string_of_printer dump#decl_param

let dump_decl_params = LibUtil.to_string_of_printer dump#decl_params

let dump_opt_decl_params = LibUtil.to_string_of_printer dump#opt_decl_params

let dump_type_info = LibUtil.to_string_of_printer dump#type_info

let dump_type_repr = LibUtil.to_string_of_printer dump#type_repr

let dump_name_ctyp = LibUtil.to_string_of_printer dump#name_ctyp

let dump_or_ctyp = LibUtil.to_string_of_printer dump#or_ctyp

let dump_of_ctyp = LibUtil.to_string_of_printer dump#of_ctyp

let dump_pat = LibUtil.to_string_of_printer dump#pat

let dump_rec_pat = LibUtil.to_string_of_printer dump#rec_pat

let dump_exp = LibUtil.to_string_of_printer dump#exp

let dump_rec_exp = LibUtil.to_string_of_printer dump#rec_exp

let dump_mtyp = LibUtil.to_string_of_printer dump#mtyp

let dump_sigi = LibUtil.to_string_of_printer dump#sigi

let dump_mbind = LibUtil.to_string_of_printer dump#mbind

let dump_constr = LibUtil.to_string_of_printer dump#constr

let dump_bind = LibUtil.to_string_of_printer dump#bind

let dump_case = LibUtil.to_string_of_printer dump#case

let dump_mexp = LibUtil.to_string_of_printer dump#mexp

let dump_stru = LibUtil.to_string_of_printer dump#stru

let dump_cltdecl = LibUtil.to_string_of_printer dump#cltdecl

let dump_cltyp = LibUtil.to_string_of_printer dump#cltyp

let dump_clsigi = LibUtil.to_string_of_printer dump#clsigi

let dump_cldecl = LibUtil.to_string_of_printer dump#cldecl

let dump_clexp = LibUtil.to_string_of_printer dump#clexp

let dump_clfield = LibUtil.to_string_of_printer dump#clfield

let dump_ep = LibUtil.to_string_of_printer dump#ep

let dump_rec_bind = LibUtil.to_string_of_printer dump#rec_bind

let wildcarder =
  object (self)
    inherit  map as super
    method! pat =
      function
      | (`Lid _loc : AstN.pat) -> (`Any : AstN.pat )
      | (`Alias (p,_) : AstN.pat) -> self#pat p
      | p -> super#pat p
  end