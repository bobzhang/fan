open StdFan
open Astf
let strip_ant ant = ant
let pp_print_loc: Format.formatter -> loc -> unit =
  fun fmt  _a0  -> Locf.pp_print_t fmt _a0
let pp_print_ant: Format.formatter -> ant -> unit =
  fun fmt  (`Ant (_a0,_a1))  ->
    Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" pp_print_loc _a0
      Tokenf.pp_print_ant _a1
let pp_print_literal: Format.formatter -> literal -> unit =
  fun fmt  ->
    function
    | `Chr (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Chr@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Int (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Int@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Int32 (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Int32@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Int64 (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Int64@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Flo (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Flo@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Nativeint (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Nativeint@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Str (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
let pp_print_flag: Format.formatter -> flag -> unit =
  fun fmt  ->
    function
    | `Positive _a0 ->
        Format.fprintf fmt "@[<1>(`Positive@ %a)@]" pp_print_loc _a0
    | `Negative _a0 ->
        Format.fprintf fmt "@[<1>(`Negative@ %a)@]" pp_print_loc _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
let pp_print_position_flag: Format.formatter -> position_flag -> unit =
  fun fmt  ->
    function
    | `Positive _a0 ->
        Format.fprintf fmt "@[<1>(`Positive@ %a)@]" pp_print_loc _a0
    | `Negative _a0 ->
        Format.fprintf fmt "@[<1>(`Negative@ %a)@]" pp_print_loc _a0
    | `Normal _a0 ->
        Format.fprintf fmt "@[<1>(`Normal@ %a)@]" pp_print_loc _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
let rec pp_print_strings: Format.formatter -> strings -> unit =
  fun fmt  ->
    function
    | `App (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_strings _a1 pp_print_strings _a2
    | `Str (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
let pp_print_lident: Format.formatter -> lident -> unit =
  fun fmt  (`Lid (_a0,_a1))  ->
    Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0
      pp_print_string _a1
let pp_print_alident: Format.formatter -> alident -> unit =
  fun fmt  ->
    function
    | `Lid (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
let pp_print_auident: Format.formatter -> auident -> unit =
  fun fmt  ->
    function
    | `Uid (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
let pp_print_aident: Format.formatter -> aident -> unit =
  fun fmt  ->
    function
    | #alident as _a0 -> (pp_print_alident fmt _a0 :>unit)
    | #auident as _a0 -> (pp_print_auident fmt _a0 :>unit)
let pp_print_astring: Format.formatter -> astring -> unit =
  fun fmt  ->
    function
    | `C (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`C@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
let rec pp_print_uident: Format.formatter -> uident -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_uident _a1 pp_print_uident _a2
    | `App (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_uident _a1 pp_print_uident _a2
    | #auident as _a0 -> (pp_print_auident fmt _a0 :>unit)
let rec pp_print_ident: Format.formatter -> ident -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ident _a1 pp_print_ident _a2
    | `Apply (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Apply@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ident _a1 pp_print_ident _a2
    | #alident as _a0 -> (pp_print_alident fmt _a0 :>unit)
    | #auident as _a0 -> (pp_print_auident fmt _a0 :>unit)
let pp_print_ident': Format.formatter -> ident' -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ident _a1 pp_print_ident _a2
    | `Apply (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Apply@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ident _a1 pp_print_ident _a2
    | `Lid (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Uid (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
let rec pp_print_vid: Format.formatter -> vid -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_vid _a1 pp_print_vid _a2
    | `Lid (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Uid (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
let pp_print_vid': Format.formatter -> vid' -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_vid _a1 pp_print_vid _a2
    | `Lid (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Uid (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
let rec pp_print_dupath: Format.formatter -> dupath -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_dupath _a1 pp_print_dupath _a2
    | #auident as _a0 -> (pp_print_auident fmt _a0 :>unit)
let pp_print_dlpath: Format.formatter -> dlpath -> unit =
  fun fmt  ->
    function
    | `Dot (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_dupath _a1 pp_print_alident _a2
    | #alident as _a0 -> (pp_print_alident fmt _a0 :>unit)
let pp_print_any: Format.formatter -> any -> unit =
  fun fmt  (`Any _a0)  ->
    Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc _a0
let rec pp_print_ctyp: Format.formatter -> ctyp -> unit =
  fun fmt  ->
    function
    | `Alias (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_alident _a2
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | `App (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_ctyp _a2
    | `Arrow (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Arrow@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_ctyp _a2
    | `ClassPath (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ClassPath@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ident _a1
    | `Label (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1 pp_print_ctyp _a2
    | `OptLabl (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1 pp_print_ctyp _a2
    | #ident' as _a0 -> (pp_print_ident' fmt _a0 :>unit)
    | `TyObj (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TyObj@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_name_ctyp _a1 pp_print_flag _a2
    | `TyObjEnd (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyObjEnd@ %a@ %a)@]" pp_print_loc _a0
          pp_print_flag _a1
    | `TyPol (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TyPol@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_ctyp _a2
    | `TyPolEnd (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyPolEnd@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1
    | `TyTypePol (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TyTypePol@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_ctyp _a2
    | `Quote (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_position_flag _a1 pp_print_alident _a2
    | `QuoteAny (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" pp_print_loc _a0
          pp_print_position_flag _a1
    | `Par (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Par@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1
    | `Sta (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Sta@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_ctyp _a2
    | `PolyEq (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`PolyEq@ %a@ %a)@]" pp_print_loc _a0
          pp_print_row_field _a1
    | `PolySup (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`PolySup@ %a@ %a)@]" pp_print_loc _a0
          pp_print_row_field _a1
    | `PolyInf (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`PolyInf@ %a@ %a)@]" pp_print_loc _a0
          pp_print_row_field _a1
    | `Com (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_ctyp _a2
    | `PolyInfSup (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`PolyInfSup@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_row_field _a1 pp_print_tag_names _a2
    | `Package (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Package@ %a@ %a)@]" pp_print_loc _a0
          pp_print_mtyp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_type_parameters: Format.formatter -> type_parameters -> unit =
  fun fmt  ->
    function
    | `Com (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_type_parameters _a1 pp_print_type_parameters _a2
    | `Ctyp (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Ctyp@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_row_field: Format.formatter -> row_field -> unit =
  fun fmt  ->
    function
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    | `Bar (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Bar@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_row_field _a1 pp_print_row_field _a2
    | `TyVrn (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" pp_print_loc _a0
          pp_print_astring _a1
    | `TyVrnOf (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TyVrnOf@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_astring _a1 pp_print_ctyp _a2
    | `Ctyp (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Ctyp@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1
and pp_print_tag_names: Format.formatter -> tag_names -> unit =
  fun fmt  ->
    function
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    | `App (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_tag_names _a1 pp_print_tag_names _a2
    | `TyVrn (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" pp_print_loc _a0
          pp_print_astring _a1
and pp_print_typedecl: Format.formatter -> typedecl -> unit =
  fun fmt  ->
    function
    | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
        Format.fprintf fmt "@[<1>(`TyDcl@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1 pp_print_opt_decl_params _a2
          pp_print_type_info _a3 pp_print_opt_type_constr _a4
    | `TyAbstr (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`TyAbstr@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1 pp_print_opt_decl_params _a2
          pp_print_opt_type_constr _a3
    | `And (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_typedecl _a1 pp_print_typedecl _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_type_constr: Format.formatter -> type_constr -> unit =
  fun fmt  ->
    function
    | `And (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_type_constr _a1 pp_print_type_constr _a2
    | `Eq (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_ctyp _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_opt_type_constr: Format.formatter -> opt_type_constr -> unit =
  fun fmt  ->
    function
    | `Some (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Some@ %a@ %a)@]" pp_print_loc _a0
          pp_print_type_constr _a1
    | `None _a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" pp_print_loc _a0
and pp_print_decl_param: Format.formatter -> decl_param -> unit =
  fun fmt  ->
    function
    | `Quote (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_position_flag _a1 pp_print_alident _a2
    | `QuoteAny (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" pp_print_loc _a0
          pp_print_position_flag _a1
    | `Any _a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc _a0
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_decl_params: Format.formatter -> decl_params -> unit =
  fun fmt  ->
    function
    | `Quote (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_position_flag _a1 pp_print_alident _a2
    | `QuoteAny (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" pp_print_loc _a0
          pp_print_position_flag _a1
    | `Any _a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc _a0
    | `Com (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_decl_params _a1 pp_print_decl_params _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_opt_decl_params: Format.formatter -> opt_decl_params -> unit =
  fun fmt  ->
    function
    | `Some (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Some@ %a@ %a)@]" pp_print_loc _a0
          pp_print_decl_params _a1
    | `None _a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" pp_print_loc _a0
and pp_print_type_info: Format.formatter -> type_info -> unit =
  fun fmt  ->
    function
    | `TyMan (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`TyMan@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_flag _a2 pp_print_type_repr _a3
    | `TyRepr (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TyRepr@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_flag _a1 pp_print_type_repr _a2
    | `TyEq (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TyEq@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_flag _a1 pp_print_ctyp _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_type_repr: Format.formatter -> type_repr -> unit =
  fun fmt  ->
    function
    | `Record (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
          pp_print_name_ctyp _a1
    | `Sum (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sum@ %a@ %a)@]" pp_print_loc _a0
          pp_print_or_ctyp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_name_ctyp: Format.formatter -> name_ctyp -> unit =
  fun fmt  ->
    function
    | `Sem (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_name_ctyp _a1 pp_print_name_ctyp _a2
    | `TyCol (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1 pp_print_ctyp _a2
    | `TyColMut (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TyColMut@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1 pp_print_ctyp _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_or_ctyp: Format.formatter -> or_ctyp -> unit =
  fun fmt  ->
    function
    | `Bar (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Bar@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_or_ctyp _a1 pp_print_or_ctyp _a2
    | `TyCol (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_auident _a1 pp_print_ctyp _a2
    | `Of (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_auident _a1 pp_print_ctyp _a2
    | #auident as _a0 -> (pp_print_auident fmt _a0 :>unit)
and pp_print_of_ctyp: Format.formatter -> of_ctyp -> unit =
  fun fmt  ->
    function
    | `Of (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_vid _a1 pp_print_ctyp _a2
    | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>unit)
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_pat: Format.formatter -> pat -> unit =
  fun fmt  ->
    function
    | #vid as _a0 -> (pp_print_vid fmt _a0 :>unit)
    | `App (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1 pp_print_pat _a2
    | `Vrn (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Com (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1 pp_print_pat _a2
    | `Sem (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1 pp_print_pat _a2
    | `Par (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Par@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | `Record (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
          pp_print_rec_pat _a1
    | #literal as _a0 -> (pp_print_literal fmt _a0 :>unit)
    | `Alias (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1 pp_print_alident _a2
    | `ArrayEmpty _a0 ->
        Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" pp_print_loc _a0
    | `Array (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1
    | `LabelS (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`LabelS@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1
    | `Label (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1 pp_print_pat _a2
    | `OptLabl (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1 pp_print_pat _a2
    | `OptLablS (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`OptLablS@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1
    | `OptLablExpr (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`OptLablExpr@ %a@ %a@ %a@ %a)@]"
          pp_print_loc _a0 pp_print_alident _a1 pp_print_pat _a2 pp_print_exp
          _a3
    | `Bar (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Bar@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1 pp_print_pat _a2
    | `PaRng (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1 pp_print_pat _a2
    | `Constraint (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_pat _a1 pp_print_ctyp _a2
    | `ClassPath (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ClassPath@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ident _a1
    | `Lazy (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1
    | `ModuleUnpack (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a@ %a)@]" pp_print_loc _a0
          pp_print_auident _a1
    | `ModuleConstraint (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a@ %a)@]"
          pp_print_loc _a0 pp_print_auident _a1 pp_print_ctyp _a2
and pp_print_rec_pat: Format.formatter -> rec_pat -> unit =
  fun fmt  ->
    function
    | `RecBind (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_vid _a1 pp_print_pat _a2
    | `Sem (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_rec_pat _a1 pp_print_rec_pat _a2
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_exp: Format.formatter -> exp -> unit =
  fun fmt  ->
    function
    | #vid as _a0 -> (pp_print_vid fmt _a0 :>unit)
    | `App (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_exp _a2
    | `Vrn (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Com (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_exp _a2
    | `Sem (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_exp _a2
    | `Par (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Par@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | `Record (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
          pp_print_rec_exp _a1
    | #literal as _a0 -> (pp_print_literal fmt _a0 :>unit)
    | `RecordWith (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_rec_exp _a1 pp_print_exp _a2
    | `Field (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Field@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_vid _a2
    | `ArrayDot (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ArrayDot@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_exp _a2
    | `ArrayEmpty _a0 ->
        Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" pp_print_loc _a0
    | `Array (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1
    | `Assert (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Assert@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1
    | `Assign (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Assign@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_exp _a2
    | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
        Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_loc _a0 pp_print_alident _a1 pp_print_exp _a2 pp_print_exp
          _a3 pp_print_flag _a4 pp_print_exp _a5
    | `Fun (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Fun@ %a@ %a)@]" pp_print_loc _a0
          pp_print_case _a1
    | `IfThenElse (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a@ %a)@]"
          pp_print_loc _a0 pp_print_exp _a1 pp_print_exp _a2 pp_print_exp _a3
    | `IfThen (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`IfThen@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_exp _a2
    | `LabelS (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`LabelS@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1
    | `Label (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1 pp_print_exp _a2
    | `Lazy (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1
    | `LetIn (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_flag _a1 pp_print_bind _a2 pp_print_exp _a3
    | `LetTryInWith (_a0,_a1,_a2,_a3,_a4) ->
        Format.fprintf fmt "@[<1>(`LetTryInWith@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_loc _a0 pp_print_flag _a1 pp_print_bind _a2 pp_print_exp
          _a3 pp_print_case _a4
    | `LetModule (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_auident _a1 pp_print_mexp _a2 pp_print_exp _a3
    | `Match (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Match@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_case _a2
    | `New (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`New@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ident _a1
    | `Obj (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" pp_print_loc _a0
          pp_print_clfield _a1
    | `ObjEnd _a0 ->
        Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" pp_print_loc _a0
    | `ObjPat (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1 pp_print_clfield _a2
    | `ObjPatEnd (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1
    | `OptLabl (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1 pp_print_exp _a2
    | `OptLablS (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`OptLablS@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1
    | `OvrInst (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`OvrInst@ %a@ %a)@]" pp_print_loc _a0
          pp_print_rec_exp _a1
    | `OvrInstEmpty _a0 ->
        Format.fprintf fmt "@[<1>(`OvrInstEmpty@ %a)@]" pp_print_loc _a0
    | `Seq (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Seq@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1
    | `Send (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Send@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_alident _a2
    | `StringDot (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`StringDot@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_exp _a2
    | `Try (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Try@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_case _a2
    | `Constraint (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_exp _a1 pp_print_ctyp _a2
    | `Coercion (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`Coercion@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_exp _a1 pp_print_ctyp _a2 pp_print_ctyp _a3
    | `Subtype (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Subtype@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_ctyp _a2
    | `While (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`While@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1 pp_print_exp _a2
    | `LetOpen (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_flag _a1 pp_print_ident _a2 pp_print_exp _a3
    | `LocalTypeFun (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1 pp_print_exp _a2
    | `Package_exp (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Package_exp@ %a@ %a)@]" pp_print_loc _a0
          pp_print_mexp _a1
and pp_print_rec_exp: Format.formatter -> rec_exp -> unit =
  fun fmt  ->
    function
    | `Sem (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_rec_exp _a1 pp_print_rec_exp _a2
    | `RecBind (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_vid _a1 pp_print_exp _a2
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_mtyp: Format.formatter -> mtyp -> unit =
  fun fmt  ->
    function
    | #ident' as _a0 -> (pp_print_ident' fmt _a0 :>unit)
    | `Sig (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Sig@ %a@ %a)@]" pp_print_loc _a0
          pp_print_sigi _a1
    | `SigEnd _a0 ->
        Format.fprintf fmt "@[<1>(`SigEnd@ %a)@]" pp_print_loc _a0
    | `Functor (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_auident _a1 pp_print_mtyp _a2 pp_print_mtyp _a3
    | `With (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`With@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_mtyp _a1 pp_print_constr _a2
    | `ModuleTypeOf (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ModuleTypeOf@ %a@ %a)@]" pp_print_loc _a0
          pp_print_mexp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_sigi: Format.formatter -> sigi -> unit =
  fun fmt  ->
    function
    | `Val (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Val@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1 pp_print_ctyp _a2
    | `External (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1 pp_print_ctyp _a2 pp_print_strings _a3
    | `Type (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" pp_print_loc _a0
          pp_print_typedecl _a1
    | `Exception (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" pp_print_loc _a0
          pp_print_of_ctyp _a1
    | `Class (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" pp_print_loc _a0
          pp_print_cltdecl _a1
    | `ClassType (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" pp_print_loc _a0
          pp_print_cltdecl _a1
    | `Module (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_auident _a1 pp_print_mtyp _a2
    | `ModuleTypeEnd (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ModuleTypeEnd@ %a@ %a)@]" pp_print_loc _a0
          pp_print_auident _a1
    | `ModuleType (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_auident _a1 pp_print_mtyp _a2
    | `Sem (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_sigi _a1 pp_print_sigi _a2
    | `DirectiveSimple (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1
    | `Directive (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1 pp_print_exp _a2
    | `Open (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Open@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_flag _a1 pp_print_ident _a2
    | `Include (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" pp_print_loc _a0
          pp_print_mtyp _a1
    | `RecModule (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" pp_print_loc _a0
          pp_print_mbind _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_mbind: Format.formatter -> mbind -> unit =
  fun fmt  ->
    function
    | `And (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_mbind _a1 pp_print_mbind _a2
    | `ModuleBind (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a@ %a)@]"
          pp_print_loc _a0 pp_print_auident _a1 pp_print_mtyp _a2
          pp_print_mexp _a3
    | `Constraint (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_auident _a1 pp_print_mtyp _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_constr: Format.formatter -> constr -> unit =
  fun fmt  ->
    function
    | `TypeEq (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TypeEq@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_ctyp _a2
    | `ModuleEq (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ModuleEq@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ident _a1 pp_print_ident _a2
    | `TypeEqPriv (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TypeEqPriv@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_ctyp _a1 pp_print_ctyp _a2
    | `TypeSubst (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TypeSubst@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_ctyp _a2
    | `ModuleSubst (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ModuleSubst@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_ident _a1 pp_print_ident _a2
    | `And (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_constr _a1 pp_print_constr _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_bind: Format.formatter -> bind -> unit =
  fun fmt  ->
    function
    | `And (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_bind _a1 pp_print_bind _a2
    | `Bind (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Bind@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1 pp_print_exp _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_case: Format.formatter -> case -> unit =
  fun fmt  ->
    function
    | `Bar (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Bar@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_case _a1 pp_print_case _a2
    | `Case (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Case@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1 pp_print_exp _a2
    | `CaseWhen (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`CaseWhen@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_pat _a1 pp_print_exp _a2 pp_print_exp _a3
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_mexp: Format.formatter -> mexp -> unit =
  fun fmt  ->
    function
    | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>unit)
    | `App (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_mexp _a1 pp_print_mexp _a2
    | `Functor (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_auident _a1 pp_print_mtyp _a2 pp_print_mexp _a3
    | `Struct (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Struct@ %a@ %a)@]" pp_print_loc _a0
          pp_print_stru _a1
    | `StructEnd _a0 ->
        Format.fprintf fmt "@[<1>(`StructEnd@ %a)@]" pp_print_loc _a0
    | `Constraint (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_mexp _a1 pp_print_mtyp _a2
    | `PackageModule (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`PackageModule@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_stru: Format.formatter -> stru -> unit =
  fun fmt  ->
    function
    | `Class (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" pp_print_loc _a0
          pp_print_cldecl _a1
    | `ClassType (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" pp_print_loc _a0
          pp_print_cltdecl _a1
    | `Sem (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_stru _a1 pp_print_stru _a2
    | `DirectiveSimple (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1
    | `Directive (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_alident _a1 pp_print_exp _a2
    | `Exception (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" pp_print_loc _a0
          pp_print_of_ctyp _a1
    | `StExp (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`StExp@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1
    | `External (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1 pp_print_ctyp _a2 pp_print_strings _a3
    | `Include (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" pp_print_loc _a0
          pp_print_mexp _a1
    | `Module (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_auident _a1 pp_print_mexp _a2
    | `RecModule (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" pp_print_loc _a0
          pp_print_mbind _a1
    | `ModuleType (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_auident _a1 pp_print_mtyp _a2
    | `Open (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Open@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_flag _a1 pp_print_ident _a2
    | `Type (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" pp_print_loc _a0
          pp_print_typedecl _a1
    | `TypeWith (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`TypeWith@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_typedecl _a1 pp_print_strings _a2
    | `Value (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Value@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_flag _a1 pp_print_bind _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_cltdecl: Format.formatter -> cltdecl -> unit =
  fun fmt  ->
    function
    | `And (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_cltdecl _a1 pp_print_cltdecl _a2
    | `CtDecl (_a0,_a1,_a2,_a3,_a4) ->
        Format.fprintf fmt "@[<1>(`CtDecl@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_loc _a0 pp_print_flag _a1 pp_print_ident _a2
          pp_print_type_parameters _a3 pp_print_cltyp _a4
    | `CtDeclS (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`CtDeclS@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_flag _a1 pp_print_ident _a2 pp_print_cltyp _a3
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_cltyp: Format.formatter -> cltyp -> unit =
  fun fmt  ->
    function
    | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>unit)
    | `ClApply (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ClApply@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_vid _a1 pp_print_type_parameters _a2
    | `CtFun (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`CtFun@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_cltyp _a2
    | `ObjTy (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ObjTy@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_clsigi _a2
    | `ObjTyEnd (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ObjTyEnd@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1
    | `Obj (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" pp_print_loc _a0
          pp_print_clsigi _a1
    | `ObjEnd _a0 ->
        Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" pp_print_loc _a0
    | `And (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_cltyp _a1 pp_print_cltyp _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_clsigi: Format.formatter -> clsigi -> unit =
  fun fmt  ->
    function
    | `Sem (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_clsigi _a1 pp_print_clsigi _a2
    | `SigInherit (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`SigInherit@ %a@ %a)@]" pp_print_loc _a0
          pp_print_cltyp _a1
    | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
        Format.fprintf fmt "@[<1>(`CgVal@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1 pp_print_flag _a2 pp_print_flag _a3
          pp_print_ctyp _a4
    | `Method (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`Method@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1 pp_print_flag _a2 pp_print_ctyp _a3
    | `VirMeth (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`VirMeth@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1 pp_print_flag _a2 pp_print_ctyp _a3
    | `Eq (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_ctyp _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_cldecl: Format.formatter -> cldecl -> unit =
  fun fmt  ->
    function
    | `ClDecl (_a0,_a1,_a2,_a3,_a4) ->
        Format.fprintf fmt "@[<1>(`ClDecl@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_loc _a0 pp_print_flag _a1 pp_print_ident _a2
          pp_print_type_parameters _a3 pp_print_clexp _a4
    | `ClDeclS (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`ClDeclS@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_flag _a1 pp_print_ident _a2 pp_print_clexp _a3
    | `And (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_cldecl _a1 pp_print_cldecl _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_clexp: Format.formatter -> clexp -> unit =
  fun fmt  ->
    function
    | `CeApp (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_clexp _a1 pp_print_exp _a2
    | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>unit)
    | `ClApply (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ClApply@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_vid _a1 pp_print_type_parameters _a2
    | `CeFun (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1 pp_print_clexp _a2
    | `LetIn (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_flag _a1 pp_print_bind _a2 pp_print_clexp _a3
    | `Obj (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" pp_print_loc _a0
          pp_print_clfield _a1
    | `ObjEnd _a0 ->
        Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" pp_print_loc _a0
    | `ObjPat (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1 pp_print_clfield _a2
    | `ObjPatEnd (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a@ %a)@]" pp_print_loc _a0
          pp_print_pat _a1
    | `Constraint (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_clexp _a1 pp_print_cltyp _a2
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
and pp_print_clfield: Format.formatter -> clfield -> unit =
  fun fmt  ->
    function
    | `Sem (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_clfield _a1 pp_print_clfield _a2
    | `Inherit (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_flag _a1 pp_print_clexp _a2
    | `InheritAs (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`InheritAs@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_flag _a1 pp_print_clexp _a2 pp_print_alident _a3
    | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
        Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1 pp_print_flag _a2 pp_print_flag _a3
          pp_print_exp _a4
    | `VirVal (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`VirVal@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1 pp_print_flag _a2 pp_print_ctyp _a3
    | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
        Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_loc _a0 pp_print_alident _a1 pp_print_flag _a2
          pp_print_flag _a3 pp_print_exp _a4 pp_print_ctyp _a5
    | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
        Format.fprintf fmt "@[<1>(`CrMthS@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_loc _a0 pp_print_alident _a1 pp_print_flag _a2
          pp_print_flag _a3 pp_print_exp _a4
    | `VirMeth (_a0,_a1,_a2,_a3) ->
        Format.fprintf fmt "@[<1>(`VirMeth@ %a@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_alident _a1 pp_print_flag _a2 pp_print_ctyp _a3
    | `Eq (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ctyp _a1 pp_print_ctyp _a2
    | `Initializer (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Initializer@ %a@ %a)@]" pp_print_loc _a0
          pp_print_exp _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
let rec pp_print_ep: Format.formatter -> ep -> unit =
  fun fmt  ->
    function
    | #vid as _a0 -> (pp_print_vid fmt _a0 :>unit)
    | `App (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ep _a1 pp_print_ep _a2
    | `Vrn (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Com (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ep _a1 pp_print_ep _a2
    | `Sem (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ep _a1 pp_print_ep _a2
    | `Par (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Par@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ep _a1
    | `Constraint (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc
          _a0 pp_print_ep _a1 pp_print_ctyp _a2
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | `ArrayEmpty _a0 ->
        Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" pp_print_loc _a0
    | `Array (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" pp_print_loc _a0
          pp_print_ep _a1
    | `Record (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
          pp_print_rec_bind _a1
    | #literal as _a0 -> (pp_print_literal fmt _a0 :>unit)
and pp_print_rec_bind: Format.formatter -> rec_bind -> unit =
  fun fmt  ->
    function
    | `RecBind (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_vid _a1 pp_print_ep _a2
    | `Sem (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_rec_bind _a1 pp_print_rec_bind _a2
    | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
class print =
  object (self : 'self_type)
    inherit  printbase
    method loc : 'fmt -> loc -> unit= fun fmt  _a0  -> self#locf_t fmt _a0
    method ant : 'fmt -> ant -> unit=
      fun fmt  (`Ant (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" self#loc _a0
          self#tokenf_ant _a1
    method literal : 'fmt -> literal -> unit=
      fun fmt  ->
        function
        | `Chr (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Chr@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Int (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Int@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Int32 (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Int32@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Int64 (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Int64@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Flo (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Flo@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Nativeint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Nativeint@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Str (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" self#loc _a0
              self#string _a1
    method flag : 'fmt -> flag -> unit=
      fun fmt  ->
        function
        | `Positive _a0 ->
            Format.fprintf fmt "@[<1>(`Positive@ %a)@]" self#loc _a0
        | `Negative _a0 ->
            Format.fprintf fmt "@[<1>(`Negative@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method position_flag : 'fmt -> position_flag -> unit=
      fun fmt  ->
        function
        | `Positive _a0 ->
            Format.fprintf fmt "@[<1>(`Positive@ %a)@]" self#loc _a0
        | `Negative _a0 ->
            Format.fprintf fmt "@[<1>(`Negative@ %a)@]" self#loc _a0
        | `Normal _a0 ->
            Format.fprintf fmt "@[<1>(`Normal@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method strings : 'fmt -> strings -> unit=
      fun fmt  ->
        function
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#strings _a1 self#strings _a2
        | `Str (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method lident : 'fmt -> lident -> unit=
      fun fmt  (`Lid (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" self#loc _a0 self#string
          _a1
    method alident : 'fmt -> alident -> unit=
      fun fmt  ->
        function
        | `Lid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method auident : 'fmt -> auident -> unit=
      fun fmt  ->
        function
        | `Uid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method aident : 'fmt -> aident -> unit=
      fun fmt  ->
        function
        | #alident as _a0 -> (self#alident fmt _a0 :>unit)
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method astring : 'fmt -> astring -> unit=
      fun fmt  ->
        function
        | `C (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`C@ %a@ %a)@]" self#loc _a0 self#string
              _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method uident : 'fmt -> uident -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#uident _a1 self#uident _a2
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#uident _a1 self#uident _a2
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method ident : 'fmt -> ident -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ident _a2
        | `Apply (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Apply@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ident _a2
        | #alident as _a0 -> (self#alident fmt _a0 :>unit)
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method ident' : 'fmt -> ident' -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ident _a2
        | `Apply (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Apply@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ident _a2
        | `Lid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Uid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" self#loc _a0
              self#string _a1
    method vid : 'fmt -> vid -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#vid _a1 self#vid _a2
        | `Lid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Uid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method vid' : 'fmt -> vid' -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#vid _a1 self#vid _a2
        | `Lid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Uid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" self#loc _a0
              self#string _a1
    method dupath : 'fmt -> dupath -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#dupath _a1 self#dupath _a2
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method dlpath : 'fmt -> dlpath -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#dupath _a1 self#alident _a2
        | #alident as _a0 -> (self#alident fmt _a0 :>unit)
    method any : 'fmt -> any -> unit=
      fun fmt  (`Any _a0)  ->
        Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc _a0
    method ctyp : 'fmt -> ctyp -> unit=
      fun fmt  ->
        function
        | `Alias (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#alident _a2
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `Arrow (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Arrow@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `ClassPath (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassPath@ %a@ %a)@]" self#loc _a0
              self#ident _a1
        | `Label (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#ctyp _a2
        | `OptLabl (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#ctyp _a2
        | #ident' as _a0 -> (self#ident' fmt _a0 :>unit)
        | `TyObj (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyObj@ %a@ %a@ %a)@]" self#loc _a0
              self#name_ctyp _a1 self#flag _a2
        | `TyObjEnd (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyObjEnd@ %a@ %a)@]" self#loc _a0
              self#flag _a1
        | `TyPol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyPol@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `TyPolEnd (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyPolEnd@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1
        | `TyTypePol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyTypePol@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `Quote (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" self#loc _a0
              self#position_flag _a1 self#alident _a2
        | `QuoteAny (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" self#loc _a0
              self#position_flag _a1
        | `Par (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Par@ %a@ %a)@]" self#loc _a0 self#ctyp
              _a1
        | `Sta (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sta@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `PolyEq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PolyEq@ %a@ %a)@]" self#loc _a0
              self#row_field _a1
        | `PolySup (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PolySup@ %a@ %a)@]" self#loc _a0
              self#row_field _a1
        | `PolyInf (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PolyInf@ %a@ %a)@]" self#loc _a0
              self#row_field _a1
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `PolyInfSup (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`PolyInfSup@ %a@ %a@ %a)@]" self#loc
              _a0 self#row_field _a1 self#tag_names _a2
        | `Package (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Package@ %a@ %a)@]" self#loc _a0
              self#mtyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method type_parameters : 'fmt -> type_parameters -> unit=
      fun fmt  ->
        function
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#type_parameters _a1 self#type_parameters _a2
        | `Ctyp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Ctyp@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method row_field : 'fmt -> row_field -> unit=
      fun fmt  ->
        function
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
        | `Bar (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Bar@ %a@ %a@ %a)@]" self#loc _a0
              self#row_field _a1 self#row_field _a2
        | `TyVrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" self#loc _a0
              self#astring _a1
        | `TyVrnOf (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyVrnOf@ %a@ %a@ %a)@]" self#loc _a0
              self#astring _a1 self#ctyp _a2
        | `Ctyp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Ctyp@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1
    method tag_names : 'fmt -> tag_names -> unit=
      fun fmt  ->
        function
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#tag_names _a1 self#tag_names _a2
        | `TyVrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" self#loc _a0
              self#astring _a1
    method typedecl : 'fmt -> typedecl -> unit=
      fun fmt  ->
        function
        | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`TyDcl@ %a@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#alident _a1 self#opt_decl_params _a2
              self#type_info _a3 self#opt_type_constr _a4
        | `TyAbstr (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`TyAbstr@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#opt_decl_params _a2
              self#opt_type_constr _a3
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#typedecl _a1 self#typedecl _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method type_constr : 'fmt -> type_constr -> unit=
      fun fmt  ->
        function
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#type_constr _a1 self#type_constr _a2
        | `Eq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method opt_type_constr : 'fmt -> opt_type_constr -> unit=
      fun fmt  ->
        function
        | `Some (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Some@ %a@ %a)@]" self#loc _a0
              self#type_constr _a1
        | `None _a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" self#loc _a0
    method decl_param : 'fmt -> decl_param -> unit=
      fun fmt  ->
        function
        | `Quote (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" self#loc _a0
              self#position_flag _a1 self#alident _a2
        | `QuoteAny (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" self#loc _a0
              self#position_flag _a1
        | `Any _a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method decl_params : 'fmt -> decl_params -> unit=
      fun fmt  ->
        function
        | `Quote (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" self#loc _a0
              self#position_flag _a1 self#alident _a2
        | `QuoteAny (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" self#loc _a0
              self#position_flag _a1
        | `Any _a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc _a0
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#decl_params _a1 self#decl_params _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method opt_decl_params : 'fmt -> opt_decl_params -> unit=
      fun fmt  ->
        function
        | `Some (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Some@ %a@ %a)@]" self#loc _a0
              self#decl_params _a1
        | `None _a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" self#loc _a0
    method type_info : 'fmt -> type_info -> unit=
      fun fmt  ->
        function
        | `TyMan (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`TyMan@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#flag _a2 self#type_repr _a3
        | `TyRepr (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyRepr@ %a@ %a@ %a)@]" self#loc _a0
              self#flag _a1 self#type_repr _a2
        | `TyEq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyEq@ %a@ %a@ %a)@]" self#loc _a0
              self#flag _a1 self#ctyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method type_repr : 'fmt -> type_repr -> unit=
      fun fmt  ->
        function
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#name_ctyp _a1
        | `Sum (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sum@ %a@ %a)@]" self#loc _a0
              self#or_ctyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method name_ctyp : 'fmt -> name_ctyp -> unit=
      fun fmt  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#name_ctyp _a1 self#name_ctyp _a2
        | `TyCol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#ctyp _a2
        | `TyColMut (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyColMut@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#ctyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method or_ctyp : 'fmt -> or_ctyp -> unit=
      fun fmt  ->
        function
        | `Bar (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Bar@ %a@ %a@ %a)@]" self#loc _a0
              self#or_ctyp _a1 self#or_ctyp _a2
        | `TyCol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a@ %a)@]" self#loc _a0
              self#auident _a1 self#ctyp _a2
        | `Of (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" self#loc _a0
              self#auident _a1 self#ctyp _a2
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method of_ctyp : 'fmt -> of_ctyp -> unit=
      fun fmt  ->
        function
        | `Of (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" self#loc _a0
              self#vid _a1 self#ctyp _a2
        | #vid' as _a0 -> (self#vid' fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method pat : 'fmt -> pat -> unit=
      fun fmt  ->
        function
        | #vid as _a0 -> (self#vid fmt _a0 :>unit)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#pat _a2
        | `Vrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#pat _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#pat _a2
        | `Par (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Par@ %a@ %a)@]" self#loc _a0 self#pat
              _a1
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#rec_pat _a1
        | #literal as _a0 -> (self#literal fmt _a0 :>unit)
        | `Alias (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#alident _a2
        | `ArrayEmpty _a0 ->
            Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" self#loc _a0
        | `Array (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc _a0
              self#pat _a1
        | `LabelS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a@ %a)@]" self#loc _a0
              self#alident _a1
        | `Label (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#pat _a2
        | `OptLabl (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#pat _a2
        | `OptLablS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLablS@ %a@ %a)@]" self#loc _a0
              self#alident _a1
        | `OptLablExpr (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`OptLablExpr@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#alident _a1 self#pat _a2 self#exp _a3
        | `Bar (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Bar@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#pat _a2
        | `PaRng (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#pat _a2
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#pat _a1 self#ctyp _a2
        | `ClassPath (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassPath@ %a@ %a)@]" self#loc _a0
              self#ident _a1
        | `Lazy (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" self#loc _a0 
              self#pat _a1
        | `ModuleUnpack (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a@ %a)@]" self#loc _a0
              self#auident _a1
        | `ModuleConstraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a@ %a)@]"
              self#loc _a0 self#auident _a1 self#ctyp _a2
    method rec_pat : 'fmt -> rec_pat -> unit=
      fun fmt  ->
        function
        | `RecBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc _a0
              self#vid _a1 self#pat _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_pat _a1 self#rec_pat _a2
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method exp : 'fmt -> exp -> unit=
      fun fmt  ->
        function
        | #vid as _a0 -> (self#vid fmt _a0 :>unit)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `Vrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `Par (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Par@ %a@ %a)@]" self#loc _a0 self#exp
              _a1
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#rec_exp _a1
        | #literal as _a0 -> (self#literal fmt _a0 :>unit)
        | `RecordWith (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a@ %a)@]" self#loc
              _a0 self#rec_exp _a1 self#exp _a2
        | `Field (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Field@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#vid _a2
        | `ArrayDot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ArrayDot@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `ArrayEmpty _a0 ->
            Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" self#loc _a0
        | `Array (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc _a0
              self#exp _a1
        | `Assert (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Assert@ %a@ %a)@]" self#loc _a0
              self#exp _a1
        | `Assign (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Assign@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
            Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#alident _a1 self#exp _a2 self#exp _a3
              self#flag _a4 self#exp _a5
        | `Fun (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Fun@ %a@ %a)@]" self#loc _a0 self#case
              _a1
        | `IfThenElse (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#exp _a1 self#exp _a2 self#exp _a3
        | `IfThen (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`IfThen@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `LabelS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a@ %a)@]" self#loc _a0
              self#alident _a1
        | `Label (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#exp _a2
        | `Lazy (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" self#loc _a0 
              self#exp _a1
        | `LetIn (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#flag _a1 self#bind _a2 self#exp _a3
        | `LetTryInWith (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`LetTryInWith@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#flag _a1 self#bind _a2 self#exp _a3 self#case
              _a4
        | `LetModule (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#auident _a1 self#mexp _a2 self#exp _a3
        | `Match (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Match@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#case _a2
        | `New (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`New@ %a@ %a)@]" self#loc _a0
              self#ident _a1
        | `Obj (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" self#loc _a0
              self#clfield _a1
        | `ObjEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" self#loc _a0
        | `ObjPat (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#clfield _a2
        | `ObjPatEnd (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a@ %a)@]" self#loc _a0
              self#pat _a1
        | `OptLabl (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#exp _a2
        | `OptLablS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLablS@ %a@ %a)@]" self#loc _a0
              self#alident _a1
        | `OvrInst (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OvrInst@ %a@ %a)@]" self#loc _a0
              self#rec_exp _a1
        | `OvrInstEmpty _a0 ->
            Format.fprintf fmt "@[<1>(`OvrInstEmpty@ %a)@]" self#loc _a0
        | `Seq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Seq@ %a@ %a)@]" self#loc _a0 self#exp
              _a1
        | `Send (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Send@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#alident _a2
        | `StringDot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`StringDot@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `Try (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Try@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#case _a2
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#exp _a1 self#ctyp _a2
        | `Coercion (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`Coercion@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#exp _a1 self#ctyp _a2 self#ctyp _a3
        | `Subtype (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Subtype@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#ctyp _a2
        | `While (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`While@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `LetOpen (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#flag _a1 self#ident _a2 self#exp _a3
        | `LocalTypeFun (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#exp _a2
        | `Package_exp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Package_exp@ %a@ %a)@]" self#loc _a0
              self#mexp _a1
    method rec_exp : 'fmt -> rec_exp -> unit=
      fun fmt  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_exp _a1 self#rec_exp _a2
        | `RecBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc _a0
              self#vid _a1 self#exp _a2
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method mtyp : 'fmt -> mtyp -> unit=
      fun fmt  ->
        function
        | #ident' as _a0 -> (self#ident' fmt _a0 :>unit)
        | `Sig (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sig@ %a@ %a)@]" self#loc _a0 self#sigi
              _a1
        | `SigEnd _a0 ->
            Format.fprintf fmt "@[<1>(`SigEnd@ %a)@]" self#loc _a0
        | `Functor (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1 self#mtyp _a2 self#mtyp _a3
        | `With (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`With@ %a@ %a@ %a)@]" self#loc _a0
              self#mtyp _a1 self#constr _a2
        | `ModuleTypeOf (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleTypeOf@ %a@ %a)@]" self#loc _a0
              self#mexp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method sigi : 'fmt -> sigi -> unit=
      fun fmt  ->
        function
        | `Val (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Val@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#ctyp _a2
        | `External (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#ctyp _a2 self#strings _a3
        | `Type (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" self#loc _a0
              self#typedecl _a1
        | `Exception (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" self#loc _a0
              self#of_ctyp _a1
        | `Class (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" self#loc _a0
              self#cltdecl _a1
        | `ClassType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" self#loc _a0
              self#cltdecl _a1
        | `Module (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" self#loc _a0
              self#auident _a1 self#mtyp _a2
        | `ModuleTypeEnd (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleTypeEnd@ %a@ %a)@]" self#loc _a0
              self#auident _a1
        | `ModuleType (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1 self#mtyp _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#sigi _a1 self#sigi _a2
        | `DirectiveSimple (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a@ %a)@]" self#loc
              _a0 self#alident _a1
        | `Directive (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#exp _a2
        | `Open (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Open@ %a@ %a@ %a)@]" self#loc _a0
              self#flag _a1 self#ident _a2
        | `Include (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" self#loc _a0
              self#mtyp _a1
        | `RecModule (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" self#loc _a0
              self#mbind _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method mbind : 'fmt -> mbind -> unit=
      fun fmt  ->
        function
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#mbind _a1 self#mbind _a2
        | `ModuleBind (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#auident _a1 self#mtyp _a2 self#mexp _a3
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1 self#mtyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method constr : 'fmt -> constr -> unit=
      fun fmt  ->
        function
        | `TypeEq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TypeEq@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `ModuleEq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleEq@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ident _a2
        | `TypeEqPriv (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TypeEqPriv@ %a@ %a@ %a)@]" self#loc
              _a0 self#ctyp _a1 self#ctyp _a2
        | `TypeSubst (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TypeSubst@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `ModuleSubst (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleSubst@ %a@ %a@ %a)@]" self#loc
              _a0 self#ident _a1 self#ident _a2
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#constr _a1 self#constr _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method bind : 'fmt -> bind -> unit=
      fun fmt  ->
        function
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#bind _a1 self#bind _a2
        | `Bind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Bind@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#exp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method case : 'fmt -> case -> unit=
      fun fmt  ->
        function
        | `Bar (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Bar@ %a@ %a@ %a)@]" self#loc _a0
              self#case _a1 self#case _a2
        | `Case (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Case@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#exp _a2
        | `CaseWhen (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CaseWhen@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#pat _a1 self#exp _a2 self#exp _a3
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method mexp : 'fmt -> mexp -> unit=
      fun fmt  ->
        function
        | #vid' as _a0 -> (self#vid' fmt _a0 :>unit)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#mexp _a1 self#mexp _a2
        | `Functor (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1 self#mtyp _a2 self#mexp _a3
        | `Struct (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Struct@ %a@ %a)@]" self#loc _a0
              self#stru _a1
        | `StructEnd _a0 ->
            Format.fprintf fmt "@[<1>(`StructEnd@ %a)@]" self#loc _a0
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#mexp _a1 self#mtyp _a2
        | `PackageModule (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PackageModule@ %a@ %a)@]" self#loc _a0
              self#exp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method stru : 'fmt -> stru -> unit=
      fun fmt  ->
        function
        | `Class (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" self#loc _a0
              self#cldecl _a1
        | `ClassType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" self#loc _a0
              self#cltdecl _a1
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#stru _a1 self#stru _a2
        | `DirectiveSimple (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a@ %a)@]" self#loc
              _a0 self#alident _a1
        | `Directive (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#exp _a2
        | `Exception (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" self#loc _a0
              self#of_ctyp _a1
        | `StExp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`StExp@ %a@ %a)@]" self#loc _a0
              self#exp _a1
        | `External (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#ctyp _a2 self#strings _a3
        | `Include (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" self#loc _a0
              self#mexp _a1
        | `Module (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" self#loc _a0
              self#auident _a1 self#mexp _a2
        | `RecModule (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" self#loc _a0
              self#mbind _a1
        | `ModuleType (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1 self#mtyp _a2
        | `Open (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Open@ %a@ %a@ %a)@]" self#loc _a0
              self#flag _a1 self#ident _a2
        | `Type (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" self#loc _a0
              self#typedecl _a1
        | `TypeWith (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TypeWith@ %a@ %a@ %a)@]" self#loc _a0
              self#typedecl _a1 self#strings _a2
        | `Value (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Value@ %a@ %a@ %a)@]" self#loc _a0
              self#flag _a1 self#bind _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method cltdecl : 'fmt -> cltdecl -> unit=
      fun fmt  ->
        function
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#cltdecl _a1 self#cltdecl _a2
        | `CtDecl (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`CtDecl@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#flag _a1 self#ident _a2 self#type_parameters
              _a3 self#cltyp _a4
        | `CtDeclS (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CtDeclS@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#flag _a1 self#ident _a2 self#cltyp _a3
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method cltyp : 'fmt -> cltyp -> unit=
      fun fmt  ->
        function
        | #vid' as _a0 -> (self#vid' fmt _a0 :>unit)
        | `ClApply (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ClApply@ %a@ %a@ %a)@]" self#loc _a0
              self#vid _a1 self#type_parameters _a2
        | `CtFun (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CtFun@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#cltyp _a2
        | `ObjTy (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ObjTy@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#clsigi _a2
        | `ObjTyEnd (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjTyEnd@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1
        | `Obj (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" self#loc _a0
              self#clsigi _a1
        | `ObjEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" self#loc _a0
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#cltyp _a1 self#cltyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method clsigi : 'fmt -> clsigi -> unit=
      fun fmt  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#clsigi _a1 self#clsigi _a2
        | `SigInherit (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`SigInherit@ %a@ %a)@]" self#loc _a0
              self#cltyp _a1
        | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`CgVal@ %a@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#alident _a1 self#flag _a2 self#flag _a3
              self#ctyp _a4
        | `Method (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`Method@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#flag _a2 self#ctyp _a3
        | `VirMeth (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`VirMeth@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#flag _a2 self#ctyp _a3
        | `Eq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method cldecl : 'fmt -> cldecl -> unit=
      fun fmt  ->
        function
        | `ClDecl (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`ClDecl@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#flag _a1 self#ident _a2 self#type_parameters
              _a3 self#clexp _a4
        | `ClDeclS (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`ClDeclS@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#flag _a1 self#ident _a2 self#clexp _a3
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#cldecl _a1 self#cldecl _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method clexp : 'fmt -> clexp -> unit=
      fun fmt  ->
        function
        | `CeApp (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a@ %a)@]" self#loc _a0
              self#clexp _a1 self#exp _a2
        | #vid' as _a0 -> (self#vid' fmt _a0 :>unit)
        | `ClApply (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ClApply@ %a@ %a@ %a)@]" self#loc _a0
              self#vid _a1 self#type_parameters _a2
        | `CeFun (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#clexp _a2
        | `LetIn (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#flag _a1 self#bind _a2 self#clexp _a3
        | `Obj (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" self#loc _a0
              self#clfield _a1
        | `ObjEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" self#loc _a0
        | `ObjPat (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#clfield _a2
        | `ObjPatEnd (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a@ %a)@]" self#loc _a0
              self#pat _a1
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#clexp _a1 self#cltyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method clfield : 'fmt -> clfield -> unit=
      fun fmt  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#clfield _a1 self#clfield _a2
        | `Inherit (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a@ %a)@]" self#loc _a0
              self#flag _a1 self#clexp _a2
        | `InheritAs (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`InheritAs@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#flag _a1 self#clexp _a2 self#alident _a3
        | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#alident _a1 self#flag _a2 self#flag _a3
              self#exp _a4
        | `VirVal (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`VirVal@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#flag _a2 self#ctyp _a3
        | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
            Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#alident _a1 self#flag _a2 self#flag _a3
              self#exp _a4 self#ctyp _a5
        | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`CrMthS@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#alident _a1 self#flag _a2 self#flag _a3
              self#exp _a4
        | `VirMeth (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`VirMeth@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#flag _a2 self#ctyp _a3
        | `Eq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `Initializer (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Initializer@ %a@ %a)@]" self#loc _a0
              self#exp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method ep : 'fmt -> ep -> unit=
      fun fmt  ->
        function
        | #vid as _a0 -> (self#vid fmt _a0 :>unit)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#ep _a1 self#ep _a2
        | `Vrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#ep _a1 self#ep _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#ep _a1 self#ep _a2
        | `Par (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Par@ %a@ %a)@]" self#loc _a0 self#ep
              _a1
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#ep _a1 self#ctyp _a2
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | `ArrayEmpty _a0 ->
            Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" self#loc _a0
        | `Array (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc _a0 
              self#ep _a1
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#rec_bind _a1
        | #literal as _a0 -> (self#literal fmt _a0 :>unit)
    method rec_bind : 'fmt -> rec_bind -> unit=
      fun fmt  ->
        function
        | `RecBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc _a0
              self#vid _a1 self#ep _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_bind _a1 self#rec_bind _a2
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method tokenf_ant : 'fmt -> Tokenf.ant -> unit= self#unknown
    method locf_t : 'fmt -> Locf.t -> unit= self#unknown
  end
class map =
  object (self : 'self_type)
    inherit  mapbase
    method loc : loc -> loc= fun _a0  -> self#locf_t _a0
    method ant : ant -> ant=
      fun (`Ant (_a0,_a1))  ->
        let _a0 = self#loc _a0 in
        let _a1 = self#tokenf_ant _a1 in `Ant (_a0, _a1)
    method literal : literal -> literal=
      function
      | `Chr (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Chr (_a0, _a1)
      | `Int (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Int (_a0, _a1)
      | `Int32 (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Int32 (_a0, _a1)
      | `Int64 (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Int64 (_a0, _a1)
      | `Flo (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Flo (_a0, _a1)
      | `Nativeint (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Nativeint (_a0, _a1)
      | `Str (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Str (_a0, _a1)
    method flag : flag -> flag=
      function
      | `Positive _a0 -> let _a0 = self#loc _a0 in `Positive _a0
      | `Negative _a0 -> let _a0 = self#loc _a0 in `Negative _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>flag)
    method position_flag : position_flag -> position_flag=
      function
      | `Positive _a0 -> let _a0 = self#loc _a0 in `Positive _a0
      | `Negative _a0 -> let _a0 = self#loc _a0 in `Negative _a0
      | `Normal _a0 -> let _a0 = self#loc _a0 in `Normal _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>position_flag)
    method strings : strings -> strings=
      function
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#strings _a1 in
          let _a2 = self#strings _a2 in `App (_a0, _a1, _a2)
      | `Str (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Str (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>strings)
    method lident : lident -> lident=
      fun (`Lid (_a0,_a1))  ->
        let _a0 = self#loc _a0 in
        let _a1 = self#string _a1 in `Lid (_a0, _a1)
    method alident : alident -> alident=
      function
      | `Lid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Lid (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>alident)
    method auident : auident -> auident=
      function
      | `Uid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Uid (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>auident)
    method aident : aident -> aident=
      function
      | #alident as _a0 -> (self#alident _a0 : alident  :>aident)
      | #auident as _a0 -> (self#auident _a0 : auident  :>aident)
    method astring : astring -> astring=
      function
      | `C (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `C (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>astring)
    method uident : uident -> uident=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#uident _a1 in
          let _a2 = self#uident _a2 in `Dot (_a0, _a1, _a2)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#uident _a1 in
          let _a2 = self#uident _a2 in `App (_a0, _a1, _a2)
      | #auident as _a0 -> (self#auident _a0 : auident  :>uident)
    method ident : ident -> ident=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `Dot (_a0, _a1, _a2)
      | `Apply (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `Apply (_a0, _a1, _a2)
      | #alident as _a0 -> (self#alident _a0 : alident  :>ident)
      | #auident as _a0 -> (self#auident _a0 : auident  :>ident)
    method ident' : ident' -> ident'=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `Dot (_a0, _a1, _a2)
      | `Apply (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `Apply (_a0, _a1, _a2)
      | `Lid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Lid (_a0, _a1)
      | `Uid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Uid (_a0, _a1)
    method vid : vid -> vid=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#vid _a2 in `Dot (_a0, _a1, _a2)
      | `Lid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Lid (_a0, _a1)
      | `Uid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Uid (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>vid)
    method vid' : vid' -> vid'=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#vid _a2 in `Dot (_a0, _a1, _a2)
      | `Lid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Lid (_a0, _a1)
      | `Uid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Uid (_a0, _a1)
    method dupath : dupath -> dupath=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#dupath _a1 in
          let _a2 = self#dupath _a2 in `Dot (_a0, _a1, _a2)
      | #auident as _a0 -> (self#auident _a0 : auident  :>dupath)
    method dlpath : dlpath -> dlpath=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#dupath _a1 in
          let _a2 = self#alident _a2 in `Dot (_a0, _a1, _a2)
      | #alident as _a0 -> (self#alident _a0 : alident  :>dlpath)
    method any : any -> any=
      fun (`Any _a0)  -> let _a0 = self#loc _a0 in `Any _a0
    method ctyp : ctyp -> ctyp=
      function
      | `Alias (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#alident _a2 in `Alias (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>ctyp)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `App (_a0, _a1, _a2)
      | `Arrow (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Arrow (_a0, _a1, _a2)
      | `ClassPath (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in `ClassPath (_a0, _a1)
      | `Label (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in `Label (_a0, _a1, _a2)
      | `OptLabl (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in `OptLabl (_a0, _a1, _a2)
      | #ident' as _a0 -> (self#ident' _a0 : ident'  :>ctyp)
      | `TyObj (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#name_ctyp _a1 in
          let _a2 = self#flag _a2 in `TyObj (_a0, _a1, _a2)
      | `TyObjEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in `TyObjEnd (_a0, _a1)
      | `TyPol (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TyPol (_a0, _a1, _a2)
      | `TyPolEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in `TyPolEnd (_a0, _a1)
      | `TyTypePol (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TyTypePol (_a0, _a1, _a2)
      | `Quote (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#position_flag _a1 in
          let _a2 = self#alident _a2 in `Quote (_a0, _a1, _a2)
      | `QuoteAny (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#position_flag _a1 in `QuoteAny (_a0, _a1)
      | `Par (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in `Par (_a0, _a1)
      | `Sta (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Sta (_a0, _a1, _a2)
      | `PolyEq (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#row_field _a1 in `PolyEq (_a0, _a1)
      | `PolySup (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#row_field _a1 in `PolySup (_a0, _a1)
      | `PolyInf (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#row_field _a1 in `PolyInf (_a0, _a1)
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Com (_a0, _a1, _a2)
      | `PolyInfSup (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#row_field _a1 in
          let _a2 = self#tag_names _a2 in `PolyInfSup (_a0, _a1, _a2)
      | `Package (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mtyp _a1 in `Package (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>ctyp)
    method type_parameters : type_parameters -> type_parameters=
      function
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#type_parameters _a1 in
          let _a2 = self#type_parameters _a2 in `Com (_a0, _a1, _a2)
      | `Ctyp (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in `Ctyp (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_parameters)
    method row_field : row_field -> row_field=
      function
      | #ant as _a0 -> (self#ant _a0 : ant  :>row_field)
      | `Bar (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#row_field _a1 in
          let _a2 = self#row_field _a2 in `Bar (_a0, _a1, _a2)
      | `TyVrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#astring _a1 in `TyVrn (_a0, _a1)
      | `TyVrnOf (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#astring _a1 in
          let _a2 = self#ctyp _a2 in `TyVrnOf (_a0, _a1, _a2)
      | `Ctyp (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in `Ctyp (_a0, _a1)
    method tag_names : tag_names -> tag_names=
      function
      | #ant as _a0 -> (self#ant _a0 : ant  :>tag_names)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#tag_names _a1 in
          let _a2 = self#tag_names _a2 in `App (_a0, _a1, _a2)
      | `TyVrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#astring _a1 in `TyVrn (_a0, _a1)
    method typedecl : typedecl -> typedecl=
      function
      | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#opt_decl_params _a2 in
          let _a3 = self#type_info _a3 in
          let _a4 = self#opt_type_constr _a4 in
          `TyDcl (_a0, _a1, _a2, _a3, _a4)
      | `TyAbstr (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#opt_decl_params _a2 in
          let _a3 = self#opt_type_constr _a3 in `TyAbstr (_a0, _a1, _a2, _a3)
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#typedecl _a1 in
          let _a2 = self#typedecl _a2 in `And (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>typedecl)
    method type_constr : type_constr -> type_constr=
      function
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#type_constr _a1 in
          let _a2 = self#type_constr _a2 in `And (_a0, _a1, _a2)
      | `Eq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Eq (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_constr)
    method opt_type_constr : opt_type_constr -> opt_type_constr=
      function
      | `Some (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#type_constr _a1 in `Some (_a0, _a1)
      | `None _a0 -> let _a0 = self#loc _a0 in `None _a0
    method decl_param : decl_param -> decl_param=
      function
      | `Quote (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#position_flag _a1 in
          let _a2 = self#alident _a2 in `Quote (_a0, _a1, _a2)
      | `QuoteAny (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#position_flag _a1 in `QuoteAny (_a0, _a1)
      | `Any _a0 -> let _a0 = self#loc _a0 in `Any _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>decl_param)
    method decl_params : decl_params -> decl_params=
      function
      | `Quote (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#position_flag _a1 in
          let _a2 = self#alident _a2 in `Quote (_a0, _a1, _a2)
      | `QuoteAny (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#position_flag _a1 in `QuoteAny (_a0, _a1)
      | `Any _a0 -> let _a0 = self#loc _a0 in `Any _a0
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#decl_params _a1 in
          let _a2 = self#decl_params _a2 in `Com (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>decl_params)
    method opt_decl_params : opt_decl_params -> opt_decl_params=
      function
      | `Some (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#decl_params _a1 in `Some (_a0, _a1)
      | `None _a0 -> let _a0 = self#loc _a0 in `None _a0
    method type_info : type_info -> type_info=
      function
      | `TyMan (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#type_repr _a3 in `TyMan (_a0, _a1, _a2, _a3)
      | `TyRepr (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#type_repr _a2 in `TyRepr (_a0, _a1, _a2)
      | `TyEq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ctyp _a2 in `TyEq (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_info)
    method type_repr : type_repr -> type_repr=
      function
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#name_ctyp _a1 in `Record (_a0, _a1)
      | `Sum (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#or_ctyp _a1 in `Sum (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_repr)
    method name_ctyp : name_ctyp -> name_ctyp=
      function
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#name_ctyp _a1 in
          let _a2 = self#name_ctyp _a2 in `Sem (_a0, _a1, _a2)
      | `TyCol (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in `TyCol (_a0, _a1, _a2)
      | `TyColMut (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in `TyColMut (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>name_ctyp)
    method or_ctyp : or_ctyp -> or_ctyp=
      function
      | `Bar (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#or_ctyp _a1 in
          let _a2 = self#or_ctyp _a2 in `Bar (_a0, _a1, _a2)
      | `TyCol (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#ctyp _a2 in `TyCol (_a0, _a1, _a2)
      | `Of (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#ctyp _a2 in `Of (_a0, _a1, _a2)
      | #auident as _a0 -> (self#auident _a0 : auident  :>or_ctyp)
    method of_ctyp : of_ctyp -> of_ctyp=
      function
      | `Of (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#ctyp _a2 in `Of (_a0, _a1, _a2)
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>of_ctyp)
      | #ant as _a0 -> (self#ant _a0 : ant  :>of_ctyp)
    method pat : pat -> pat=
      function
      | #vid as _a0 -> (self#vid _a0 : vid  :>pat)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#pat _a2 in `App (_a0, _a1, _a2)
      | `Vrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Vrn (_a0, _a1)
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#pat _a2 in `Com (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#pat _a2 in `Sem (_a0, _a1, _a2)
      | `Par (_a0,_a1) ->
          let _a0 = self#loc _a0 in let _a1 = self#pat _a1 in `Par (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>pat)
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_pat _a1 in `Record (_a0, _a1)
      | #literal as _a0 -> (self#literal _a0 : literal  :>pat)
      | `Alias (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#alident _a2 in `Alias (_a0, _a1, _a2)
      | `ArrayEmpty _a0 -> let _a0 = self#loc _a0 in `ArrayEmpty _a0
      | `Array (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in `Array (_a0, _a1)
      | `LabelS (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `LabelS (_a0, _a1)
      | `Label (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#pat _a2 in `Label (_a0, _a1, _a2)
      | `OptLabl (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#pat _a2 in `OptLabl (_a0, _a1, _a2)
      | `OptLablS (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `OptLablS (_a0, _a1)
      | `OptLablExpr (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#pat _a2 in
          let _a3 = self#exp _a3 in `OptLablExpr (_a0, _a1, _a2, _a3)
      | `Bar (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#pat _a2 in `Bar (_a0, _a1, _a2)
      | `PaRng (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#pat _a2 in `PaRng (_a0, _a1, _a2)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#ctyp _a2 in `Constraint (_a0, _a1, _a2)
      | `ClassPath (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in `ClassPath (_a0, _a1)
      | `Lazy (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in `Lazy (_a0, _a1)
      | `ModuleUnpack (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in `ModuleUnpack (_a0, _a1)
      | `ModuleConstraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#ctyp _a2 in `ModuleConstraint (_a0, _a1, _a2)
    method rec_pat : rec_pat -> rec_pat=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#pat _a2 in `RecBind (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_pat _a1 in
          let _a2 = self#rec_pat _a2 in `Sem (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>rec_pat)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_pat)
    method exp : exp -> exp=
      function
      | #vid as _a0 -> (self#vid _a0 : vid  :>exp)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `App (_a0, _a1, _a2)
      | `Vrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Vrn (_a0, _a1)
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `Com (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `Sem (_a0, _a1, _a2)
      | `Par (_a0,_a1) ->
          let _a0 = self#loc _a0 in let _a1 = self#exp _a1 in `Par (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>exp)
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_exp _a1 in `Record (_a0, _a1)
      | #literal as _a0 -> (self#literal _a0 : literal  :>exp)
      | `RecordWith (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_exp _a1 in
          let _a2 = self#exp _a2 in `RecordWith (_a0, _a1, _a2)
      | `Field (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#vid _a2 in `Field (_a0, _a1, _a2)
      | `ArrayDot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `ArrayDot (_a0, _a1, _a2)
      | `ArrayEmpty _a0 -> let _a0 = self#loc _a0 in `ArrayEmpty _a0
      | `Array (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `Array (_a0, _a1)
      | `Assert (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `Assert (_a0, _a1)
      | `Assign (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `Assign (_a0, _a1, _a2)
      | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in
          let _a3 = self#exp _a3 in
          let _a4 = self#flag _a4 in
          let _a5 = self#exp _a5 in `For (_a0, _a1, _a2, _a3, _a4, _a5)
      | `Fun (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#case _a1 in `Fun (_a0, _a1)
      | `IfThenElse (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in
          let _a3 = self#exp _a3 in `IfThenElse (_a0, _a1, _a2, _a3)
      | `IfThen (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `IfThen (_a0, _a1, _a2)
      | `LabelS (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `LabelS (_a0, _a1)
      | `Label (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in `Label (_a0, _a1, _a2)
      | `Lazy (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `Lazy (_a0, _a1)
      | `LetIn (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#bind _a2 in
          let _a3 = self#exp _a3 in `LetIn (_a0, _a1, _a2, _a3)
      | `LetTryInWith (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#bind _a2 in
          let _a3 = self#exp _a3 in
          let _a4 = self#case _a4 in `LetTryInWith (_a0, _a1, _a2, _a3, _a4)
      | `LetModule (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mexp _a2 in
          let _a3 = self#exp _a3 in `LetModule (_a0, _a1, _a2, _a3)
      | `Match (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#case _a2 in `Match (_a0, _a1, _a2)
      | `New (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in `New (_a0, _a1)
      | `Obj (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clfield _a1 in `Obj (_a0, _a1)
      | `ObjEnd _a0 -> let _a0 = self#loc _a0 in `ObjEnd _a0
      | `ObjPat (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#clfield _a2 in `ObjPat (_a0, _a1, _a2)
      | `ObjPatEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in `ObjPatEnd (_a0, _a1)
      | `OptLabl (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in `OptLabl (_a0, _a1, _a2)
      | `OptLablS (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `OptLablS (_a0, _a1)
      | `OvrInst (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_exp _a1 in `OvrInst (_a0, _a1)
      | `OvrInstEmpty _a0 -> let _a0 = self#loc _a0 in `OvrInstEmpty _a0
      | `Seq (_a0,_a1) ->
          let _a0 = self#loc _a0 in let _a1 = self#exp _a1 in `Seq (_a0, _a1)
      | `Send (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#alident _a2 in `Send (_a0, _a1, _a2)
      | `StringDot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `StringDot (_a0, _a1, _a2)
      | `Try (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#case _a2 in `Try (_a0, _a1, _a2)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#ctyp _a2 in `Constraint (_a0, _a1, _a2)
      | `Coercion (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#ctyp _a2 in
          let _a3 = self#ctyp _a3 in `Coercion (_a0, _a1, _a2, _a3)
      | `Subtype (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#ctyp _a2 in `Subtype (_a0, _a1, _a2)
      | `While (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `While (_a0, _a1, _a2)
      | `LetOpen (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#exp _a3 in `LetOpen (_a0, _a1, _a2, _a3)
      | `LocalTypeFun (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in `LocalTypeFun (_a0, _a1, _a2)
      | `Package_exp (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mexp _a1 in `Package_exp (_a0, _a1)
    method rec_exp : rec_exp -> rec_exp=
      function
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_exp _a1 in
          let _a2 = self#rec_exp _a2 in `Sem (_a0, _a1, _a2)
      | `RecBind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#exp _a2 in `RecBind (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>rec_exp)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_exp)
    method mtyp : mtyp -> mtyp=
      function
      | #ident' as _a0 -> (self#ident' _a0 : ident'  :>mtyp)
      | `Sig (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sigi _a1 in `Sig (_a0, _a1)
      | `SigEnd _a0 -> let _a0 = self#loc _a0 in `SigEnd _a0
      | `Functor (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in
          let _a3 = self#mtyp _a3 in `Functor (_a0, _a1, _a2, _a3)
      | `With (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mtyp _a1 in
          let _a2 = self#constr _a2 in `With (_a0, _a1, _a2)
      | `ModuleTypeOf (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mexp _a1 in `ModuleTypeOf (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>mtyp)
    method sigi : sigi -> sigi=
      function
      | `Val (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in `Val (_a0, _a1, _a2)
      | `External (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in
          let _a3 = self#strings _a3 in `External (_a0, _a1, _a2, _a3)
      | `Type (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#typedecl _a1 in `Type (_a0, _a1)
      | `Exception (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#of_ctyp _a1 in `Exception (_a0, _a1)
      | `Class (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cltdecl _a1 in `Class (_a0, _a1)
      | `ClassType (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cltdecl _a1 in `ClassType (_a0, _a1)
      | `Module (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in `Module (_a0, _a1, _a2)
      | `ModuleTypeEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in `ModuleTypeEnd (_a0, _a1)
      | `ModuleType (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in `ModuleType (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sigi _a1 in
          let _a2 = self#sigi _a2 in `Sem (_a0, _a1, _a2)
      | `DirectiveSimple (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `DirectiveSimple (_a0, _a1)
      | `Directive (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in `Directive (_a0, _a1, _a2)
      | `Open (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in `Open (_a0, _a1, _a2)
      | `Include (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mtyp _a1 in `Include (_a0, _a1)
      | `RecModule (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mbind _a1 in `RecModule (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>sigi)
    method mbind : mbind -> mbind=
      function
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mbind _a1 in
          let _a2 = self#mbind _a2 in `And (_a0, _a1, _a2)
      | `ModuleBind (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in
          let _a3 = self#mexp _a3 in `ModuleBind (_a0, _a1, _a2, _a3)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in `Constraint (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>mbind)
    method constr : constr -> constr=
      function
      | `TypeEq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TypeEq (_a0, _a1, _a2)
      | `ModuleEq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `ModuleEq (_a0, _a1, _a2)
      | `TypeEqPriv (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TypeEqPriv (_a0, _a1, _a2)
      | `TypeSubst (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TypeSubst (_a0, _a1, _a2)
      | `ModuleSubst (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `ModuleSubst (_a0, _a1, _a2)
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#constr _a1 in
          let _a2 = self#constr _a2 in `And (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>constr)
    method bind : bind -> bind=
      function
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#bind _a1 in
          let _a2 = self#bind _a2 in `And (_a0, _a1, _a2)
      | `Bind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#exp _a2 in `Bind (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>bind)
    method case : case -> case=
      function
      | `Bar (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#case _a1 in
          let _a2 = self#case _a2 in `Bar (_a0, _a1, _a2)
      | `Case (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#exp _a2 in `Case (_a0, _a1, _a2)
      | `CaseWhen (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#exp _a2 in
          let _a3 = self#exp _a3 in `CaseWhen (_a0, _a1, _a2, _a3)
      | #ant as _a0 -> (self#ant _a0 : ant  :>case)
    method mexp : mexp -> mexp=
      function
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>mexp)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mexp _a1 in
          let _a2 = self#mexp _a2 in `App (_a0, _a1, _a2)
      | `Functor (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in
          let _a3 = self#mexp _a3 in `Functor (_a0, _a1, _a2, _a3)
      | `Struct (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#stru _a1 in `Struct (_a0, _a1)
      | `StructEnd _a0 -> let _a0 = self#loc _a0 in `StructEnd _a0
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mexp _a1 in
          let _a2 = self#mtyp _a2 in `Constraint (_a0, _a1, _a2)
      | `PackageModule (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `PackageModule (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>mexp)
    method stru : stru -> stru=
      function
      | `Class (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cldecl _a1 in `Class (_a0, _a1)
      | `ClassType (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cltdecl _a1 in `ClassType (_a0, _a1)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#stru _a1 in
          let _a2 = self#stru _a2 in `Sem (_a0, _a1, _a2)
      | `DirectiveSimple (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `DirectiveSimple (_a0, _a1)
      | `Directive (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in `Directive (_a0, _a1, _a2)
      | `Exception (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#of_ctyp _a1 in `Exception (_a0, _a1)
      | `StExp (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `StExp (_a0, _a1)
      | `External (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in
          let _a3 = self#strings _a3 in `External (_a0, _a1, _a2, _a3)
      | `Include (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mexp _a1 in `Include (_a0, _a1)
      | `Module (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mexp _a2 in `Module (_a0, _a1, _a2)
      | `RecModule (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mbind _a1 in `RecModule (_a0, _a1)
      | `ModuleType (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in `ModuleType (_a0, _a1, _a2)
      | `Open (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in `Open (_a0, _a1, _a2)
      | `Type (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#typedecl _a1 in `Type (_a0, _a1)
      | `TypeWith (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#typedecl _a1 in
          let _a2 = self#strings _a2 in `TypeWith (_a0, _a1, _a2)
      | `Value (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#bind _a2 in `Value (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>stru)
    method cltdecl : cltdecl -> cltdecl=
      function
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cltdecl _a1 in
          let _a2 = self#cltdecl _a2 in `And (_a0, _a1, _a2)
      | `CtDecl (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#type_parameters _a3 in
          let _a4 = self#cltyp _a4 in `CtDecl (_a0, _a1, _a2, _a3, _a4)
      | `CtDeclS (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#cltyp _a3 in `CtDeclS (_a0, _a1, _a2, _a3)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cltdecl)
    method cltyp : cltyp -> cltyp=
      function
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>cltyp)
      | `ClApply (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#type_parameters _a2 in `ClApply (_a0, _a1, _a2)
      | `CtFun (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#cltyp _a2 in `CtFun (_a0, _a1, _a2)
      | `ObjTy (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#clsigi _a2 in `ObjTy (_a0, _a1, _a2)
      | `ObjTyEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in `ObjTyEnd (_a0, _a1)
      | `Obj (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clsigi _a1 in `Obj (_a0, _a1)
      | `ObjEnd _a0 -> let _a0 = self#loc _a0 in `ObjEnd _a0
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cltyp _a1 in
          let _a2 = self#cltyp _a2 in `And (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cltyp)
    method clsigi : clsigi -> clsigi=
      function
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clsigi _a1 in
          let _a2 = self#clsigi _a2 in `Sem (_a0, _a1, _a2)
      | `SigInherit (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cltyp _a1 in `SigInherit (_a0, _a1)
      | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#flag _a3 in
          let _a4 = self#ctyp _a4 in `CgVal (_a0, _a1, _a2, _a3, _a4)
      | `Method (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#ctyp _a3 in `Method (_a0, _a1, _a2, _a3)
      | `VirMeth (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#ctyp _a3 in `VirMeth (_a0, _a1, _a2, _a3)
      | `Eq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Eq (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>clsigi)
    method cldecl : cldecl -> cldecl=
      function
      | `ClDecl (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#type_parameters _a3 in
          let _a4 = self#clexp _a4 in `ClDecl (_a0, _a1, _a2, _a3, _a4)
      | `ClDeclS (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#clexp _a3 in `ClDeclS (_a0, _a1, _a2, _a3)
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cldecl _a1 in
          let _a2 = self#cldecl _a2 in `And (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cldecl)
    method clexp : clexp -> clexp=
      function
      | `CeApp (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clexp _a1 in
          let _a2 = self#exp _a2 in `CeApp (_a0, _a1, _a2)
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>clexp)
      | `ClApply (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#type_parameters _a2 in `ClApply (_a0, _a1, _a2)
      | `CeFun (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#clexp _a2 in `CeFun (_a0, _a1, _a2)
      | `LetIn (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#bind _a2 in
          let _a3 = self#clexp _a3 in `LetIn (_a0, _a1, _a2, _a3)
      | `Obj (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clfield _a1 in `Obj (_a0, _a1)
      | `ObjEnd _a0 -> let _a0 = self#loc _a0 in `ObjEnd _a0
      | `ObjPat (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#clfield _a2 in `ObjPat (_a0, _a1, _a2)
      | `ObjPatEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in `ObjPatEnd (_a0, _a1)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clexp _a1 in
          let _a2 = self#cltyp _a2 in `Constraint (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>clexp)
    method clfield : clfield -> clfield=
      function
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clfield _a1 in
          let _a2 = self#clfield _a2 in `Sem (_a0, _a1, _a2)
      | `Inherit (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#clexp _a2 in `Inherit (_a0, _a1, _a2)
      | `InheritAs (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#clexp _a2 in
          let _a3 = self#alident _a3 in `InheritAs (_a0, _a1, _a2, _a3)
      | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#flag _a3 in
          let _a4 = self#exp _a4 in `CrVal (_a0, _a1, _a2, _a3, _a4)
      | `VirVal (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#ctyp _a3 in `VirVal (_a0, _a1, _a2, _a3)
      | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#flag _a3 in
          let _a4 = self#exp _a4 in
          let _a5 = self#ctyp _a5 in `CrMth (_a0, _a1, _a2, _a3, _a4, _a5)
      | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#flag _a3 in
          let _a4 = self#exp _a4 in `CrMthS (_a0, _a1, _a2, _a3, _a4)
      | `VirMeth (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#ctyp _a3 in `VirMeth (_a0, _a1, _a2, _a3)
      | `Eq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Eq (_a0, _a1, _a2)
      | `Initializer (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `Initializer (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>clfield)
    method ep : ep -> ep=
      function
      | #vid as _a0 -> (self#vid _a0 : vid  :>ep)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ep _a1 in
          let _a2 = self#ep _a2 in `App (_a0, _a1, _a2)
      | `Vrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Vrn (_a0, _a1)
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ep _a1 in
          let _a2 = self#ep _a2 in `Com (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ep _a1 in
          let _a2 = self#ep _a2 in `Sem (_a0, _a1, _a2)
      | `Par (_a0,_a1) ->
          let _a0 = self#loc _a0 in let _a1 = self#ep _a1 in `Par (_a0, _a1)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ep _a1 in
          let _a2 = self#ctyp _a2 in `Constraint (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>ep)
      | `ArrayEmpty _a0 -> let _a0 = self#loc _a0 in `ArrayEmpty _a0
      | `Array (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ep _a1 in `Array (_a0, _a1)
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_bind _a1 in `Record (_a0, _a1)
      | #literal as _a0 -> (self#literal _a0 : literal  :>ep)
    method rec_bind : rec_bind -> rec_bind=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#ep _a2 in `RecBind (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_bind _a1 in
          let _a2 = self#rec_bind _a2 in `Sem (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>rec_bind)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_bind)
    method tokenf_ant : Tokenf.ant -> Tokenf.ant= self#unknown
    method locf_t : Locf.t -> Locf.t= self#unknown
  end
class fold =
  object (self : 'self_type)
    inherit  foldbase
    method loc : loc -> 'self_type= fun _a0  -> self#locf_t _a0
    method ant : ant -> 'self_type=
      fun (`Ant (_a0,_a1))  -> let self = self#loc _a0 in self#tokenf_ant _a1
    method literal : literal -> 'self_type=
      function
      | `Chr (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Int (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Int32 (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Int64 (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Flo (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Nativeint (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Str (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
    method flag : flag -> 'self_type=
      function
      | `Positive _a0 -> self#loc _a0
      | `Negative _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method position_flag : position_flag -> 'self_type=
      function
      | `Positive _a0 -> self#loc _a0
      | `Negative _a0 -> self#loc _a0
      | `Normal _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method strings : strings -> 'self_type=
      function
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#strings _a1 in self#strings _a2
      | `Str (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method lident : lident -> 'self_type=
      fun (`Lid (_a0,_a1))  -> let self = self#loc _a0 in self#string _a1
    method alident : alident -> 'self_type=
      function
      | `Lid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method auident : auident -> 'self_type=
      function
      | `Uid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method aident : aident -> 'self_type=
      function
      | #alident as _a0 -> (self#alident _a0 :>'self_type)
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method astring : astring -> 'self_type=
      function
      | `C (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method uident : uident -> 'self_type=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#uident _a1 in self#uident _a2
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#uident _a1 in self#uident _a2
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method ident : ident -> 'self_type=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `Apply (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | #alident as _a0 -> (self#alident _a0 :>'self_type)
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method ident' : ident' -> 'self_type=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `Apply (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `Lid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Uid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
    method vid : vid -> 'self_type=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#vid _a1 in self#vid _a2
      | `Lid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Uid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method vid' : vid' -> 'self_type=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#vid _a1 in self#vid _a2
      | `Lid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Uid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
    method dupath : dupath -> 'self_type=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#dupath _a1 in self#dupath _a2
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method dlpath : dlpath -> 'self_type=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#dupath _a1 in self#alident _a2
      | #alident as _a0 -> (self#alident _a0 :>'self_type)
    method any : any -> 'self_type= fun (`Any _a0)  -> self#loc _a0
    method ctyp : ctyp -> 'self_type=
      function
      | `Alias (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#alident _a2
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `Arrow (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `ClassPath (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Label (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#ctyp _a2
      | `OptLabl (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#ctyp _a2
      | #ident' as _a0 -> (self#ident' _a0 :>'self_type)
      | `TyObj (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#name_ctyp _a1 in self#flag _a2
      | `TyObjEnd (_a0,_a1) -> let self = self#loc _a0 in self#flag _a1
      | `TyPol (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `TyPolEnd (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
      | `TyTypePol (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `Quote (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#position_flag _a1 in self#alident _a2
      | `QuoteAny (_a0,_a1) ->
          let self = self#loc _a0 in self#position_flag _a1
      | `Par (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
      | `Sta (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `PolyEq (_a0,_a1) -> let self = self#loc _a0 in self#row_field _a1
      | `PolySup (_a0,_a1) -> let self = self#loc _a0 in self#row_field _a1
      | `PolyInf (_a0,_a1) -> let self = self#loc _a0 in self#row_field _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `PolyInfSup (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#row_field _a1 in self#tag_names _a2
      | `Package (_a0,_a1) -> let self = self#loc _a0 in self#mtyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method type_parameters : type_parameters -> 'self_type=
      function
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#type_parameters _a1 in self#type_parameters _a2
      | `Ctyp (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method row_field : row_field -> 'self_type=
      function
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | `Bar (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#row_field _a1 in self#row_field _a2
      | `TyVrn (_a0,_a1) -> let self = self#loc _a0 in self#astring _a1
      | `TyVrnOf (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#astring _a1 in self#ctyp _a2
      | `Ctyp (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
    method tag_names : tag_names -> 'self_type=
      function
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#tag_names _a1 in self#tag_names _a2
      | `TyVrn (_a0,_a1) -> let self = self#loc _a0 in self#astring _a1
    method typedecl : typedecl -> 'self_type=
      function
      | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#opt_decl_params _a2 in
          let self = self#type_info _a3 in self#opt_type_constr _a4
      | `TyAbstr (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#opt_decl_params _a2 in self#opt_type_constr _a3
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#typedecl _a1 in self#typedecl _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method type_constr : type_constr -> 'self_type=
      function
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#type_constr _a1 in self#type_constr _a2
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method opt_type_constr : opt_type_constr -> 'self_type=
      function
      | `Some (_a0,_a1) -> let self = self#loc _a0 in self#type_constr _a1
      | `None _a0 -> self#loc _a0
    method decl_param : decl_param -> 'self_type=
      function
      | `Quote (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#position_flag _a1 in self#alident _a2
      | `QuoteAny (_a0,_a1) ->
          let self = self#loc _a0 in self#position_flag _a1
      | `Any _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method decl_params : decl_params -> 'self_type=
      function
      | `Quote (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#position_flag _a1 in self#alident _a2
      | `QuoteAny (_a0,_a1) ->
          let self = self#loc _a0 in self#position_flag _a1
      | `Any _a0 -> self#loc _a0
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#decl_params _a1 in self#decl_params _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method opt_decl_params : opt_decl_params -> 'self_type=
      function
      | `Some (_a0,_a1) -> let self = self#loc _a0 in self#decl_params _a1
      | `None _a0 -> self#loc _a0
    method type_info : type_info -> 'self_type=
      function
      | `TyMan (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in
          let self = self#flag _a2 in self#type_repr _a3
      | `TyRepr (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in self#type_repr _a2
      | `TyEq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in self#ctyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method type_repr : type_repr -> 'self_type=
      function
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#name_ctyp _a1
      | `Sum (_a0,_a1) -> let self = self#loc _a0 in self#or_ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method name_ctyp : name_ctyp -> 'self_type=
      function
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#name_ctyp _a1 in self#name_ctyp _a2
      | `TyCol (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#ctyp _a2
      | `TyColMut (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#ctyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method or_ctyp : or_ctyp -> 'self_type=
      function
      | `Bar (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#or_ctyp _a1 in self#or_ctyp _a2
      | `TyCol (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#ctyp _a2
      | `Of (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#ctyp _a2
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method of_ctyp : of_ctyp -> 'self_type=
      function
      | `Of (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#vid _a1 in self#ctyp _a2
      | #vid' as _a0 -> (self#vid' _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method pat : pat -> 'self_type=
      function
      | #vid as _a0 -> (self#vid _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `Vrn (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `Par (_a0,_a1) -> let self = self#loc _a0 in self#pat _a1
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#rec_pat _a1
      | #literal as _a0 -> (self#literal _a0 :>'self_type)
      | `Alias (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in self#alident _a2
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> let self = self#loc _a0 in self#pat _a1
      | `LabelS (_a0,_a1) -> let self = self#loc _a0 in self#alident _a1
      | `Label (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#pat _a2
      | `OptLabl (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#pat _a2
      | `OptLablS (_a0,_a1) -> let self = self#loc _a0 in self#alident _a1
      | `OptLablExpr (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#pat _a2 in self#exp _a3
      | `Bar (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `PaRng (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#ctyp _a2
      | `ClassPath (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Lazy (_a0,_a1) -> let self = self#loc _a0 in self#pat _a1
      | `ModuleUnpack (_a0,_a1) ->
          let self = self#loc _a0 in self#auident _a1
      | `ModuleConstraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#ctyp _a2
    method rec_pat : rec_pat -> 'self_type=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#vid _a1 in self#pat _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_pat _a1 in self#rec_pat _a2
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method exp : exp -> 'self_type=
      function
      | #vid as _a0 -> (self#vid _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `Vrn (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `Par (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#rec_exp _a1
      | #literal as _a0 -> (self#literal _a0 :>'self_type)
      | `RecordWith (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_exp _a1 in self#exp _a2
      | `Field (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#vid _a2
      | `ArrayDot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `Assert (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `Assign (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#exp _a2 in
          let self = self#exp _a3 in let self = self#flag _a4 in self#exp _a5
      | `Fun (_a0,_a1) -> let self = self#loc _a0 in self#case _a1
      | `IfThenElse (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#exp _a1 in let self = self#exp _a2 in self#exp _a3
      | `IfThen (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `LabelS (_a0,_a1) -> let self = self#loc _a0 in self#alident _a1
      | `Label (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#exp _a2
      | `Lazy (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `LetIn (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#bind _a2 in self#exp _a3
      | `LetTryInWith (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#bind _a2 in
          let self = self#exp _a3 in self#case _a4
      | `LetModule (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#mexp _a2 in self#exp _a3
      | `Match (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#case _a2
      | `New (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Obj (_a0,_a1) -> let self = self#loc _a0 in self#clfield _a1
      | `ObjEnd _a0 -> self#loc _a0
      | `ObjPat (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in self#clfield _a2
      | `ObjPatEnd (_a0,_a1) -> let self = self#loc _a0 in self#pat _a1
      | `OptLabl (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#exp _a2
      | `OptLablS (_a0,_a1) -> let self = self#loc _a0 in self#alident _a1
      | `OvrInst (_a0,_a1) -> let self = self#loc _a0 in self#rec_exp _a1
      | `OvrInstEmpty _a0 -> self#loc _a0
      | `Seq (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `Send (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#exp _a1 in self#alident _a2
      | `StringDot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `Try (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#case _a2
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#ctyp _a2
      | `Coercion (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#exp _a1 in
          let self = self#ctyp _a2 in self#ctyp _a3
      | `Subtype (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#ctyp _a2
      | `While (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `LetOpen (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#ident _a2 in self#exp _a3
      | `LocalTypeFun (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#exp _a2
      | `Package_exp (_a0,_a1) -> let self = self#loc _a0 in self#mexp _a1
    method rec_exp : rec_exp -> 'self_type=
      function
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_exp _a1 in self#rec_exp _a2
      | `RecBind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#vid _a1 in self#exp _a2
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method mtyp : mtyp -> 'self_type=
      function
      | #ident' as _a0 -> (self#ident' _a0 :>'self_type)
      | `Sig (_a0,_a1) -> let self = self#loc _a0 in self#sigi _a1
      | `SigEnd _a0 -> self#loc _a0
      | `Functor (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#mtyp _a2 in self#mtyp _a3
      | `With (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#mtyp _a1 in self#constr _a2
      | `ModuleTypeOf (_a0,_a1) -> let self = self#loc _a0 in self#mexp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method sigi : sigi -> 'self_type=
      function
      | `Val (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#ctyp _a2
      | `External (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#ctyp _a2 in self#strings _a3
      | `Type (_a0,_a1) -> let self = self#loc _a0 in self#typedecl _a1
      | `Exception (_a0,_a1) -> let self = self#loc _a0 in self#of_ctyp _a1
      | `Class (_a0,_a1) -> let self = self#loc _a0 in self#cltdecl _a1
      | `ClassType (_a0,_a1) -> let self = self#loc _a0 in self#cltdecl _a1
      | `Module (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#mtyp _a2
      | `ModuleTypeEnd (_a0,_a1) ->
          let self = self#loc _a0 in self#auident _a1
      | `ModuleType (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#mtyp _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#sigi _a1 in self#sigi _a2
      | `DirectiveSimple (_a0,_a1) ->
          let self = self#loc _a0 in self#alident _a1
      | `Directive (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#exp _a2
      | `Open (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in self#ident _a2
      | `Include (_a0,_a1) -> let self = self#loc _a0 in self#mtyp _a1
      | `RecModule (_a0,_a1) -> let self = self#loc _a0 in self#mbind _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method mbind : mbind -> 'self_type=
      function
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#mbind _a1 in self#mbind _a2
      | `ModuleBind (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#mtyp _a2 in self#mexp _a3
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#mtyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method constr : constr -> 'self_type=
      function
      | `TypeEq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `ModuleEq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `TypeEqPriv (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `TypeSubst (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `ModuleSubst (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#constr _a1 in self#constr _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method bind : bind -> 'self_type=
      function
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#bind _a1 in self#bind _a2
      | `Bind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#exp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method case : case -> 'self_type=
      function
      | `Bar (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#case _a1 in self#case _a2
      | `Case (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#exp _a2
      | `CaseWhen (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in let self = self#exp _a2 in self#exp _a3
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method mexp : mexp -> 'self_type=
      function
      | #vid' as _a0 -> (self#vid' _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#mexp _a1 in self#mexp _a2
      | `Functor (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#mtyp _a2 in self#mexp _a3
      | `Struct (_a0,_a1) -> let self = self#loc _a0 in self#stru _a1
      | `StructEnd _a0 -> self#loc _a0
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#mexp _a1 in self#mtyp _a2
      | `PackageModule (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method stru : stru -> 'self_type=
      function
      | `Class (_a0,_a1) -> let self = self#loc _a0 in self#cldecl _a1
      | `ClassType (_a0,_a1) -> let self = self#loc _a0 in self#cltdecl _a1
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#stru _a1 in self#stru _a2
      | `DirectiveSimple (_a0,_a1) ->
          let self = self#loc _a0 in self#alident _a1
      | `Directive (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#exp _a2
      | `Exception (_a0,_a1) -> let self = self#loc _a0 in self#of_ctyp _a1
      | `StExp (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `External (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#ctyp _a2 in self#strings _a3
      | `Include (_a0,_a1) -> let self = self#loc _a0 in self#mexp _a1
      | `Module (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#mexp _a2
      | `RecModule (_a0,_a1) -> let self = self#loc _a0 in self#mbind _a1
      | `ModuleType (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#mtyp _a2
      | `Open (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in self#ident _a2
      | `Type (_a0,_a1) -> let self = self#loc _a0 in self#typedecl _a1
      | `TypeWith (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#typedecl _a1 in self#strings _a2
      | `Value (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in self#bind _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method cltdecl : cltdecl -> 'self_type=
      function
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#cltdecl _a1 in self#cltdecl _a2
      | `CtDecl (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#ident _a2 in
          let self = self#type_parameters _a3 in self#cltyp _a4
      | `CtDeclS (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#ident _a2 in self#cltyp _a3
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method cltyp : cltyp -> 'self_type=
      function
      | #vid' as _a0 -> (self#vid' _a0 :>'self_type)
      | `ClApply (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#vid _a1 in self#type_parameters _a2
      | `CtFun (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#cltyp _a2
      | `ObjTy (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#clsigi _a2
      | `ObjTyEnd (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
      | `Obj (_a0,_a1) -> let self = self#loc _a0 in self#clsigi _a1
      | `ObjEnd _a0 -> self#loc _a0
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#cltyp _a1 in self#cltyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method clsigi : clsigi -> 'self_type=
      function
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#clsigi _a1 in self#clsigi _a2
      | `SigInherit (_a0,_a1) -> let self = self#loc _a0 in self#cltyp _a1
      | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in
          let self = self#flag _a3 in self#ctyp _a4
      | `Method (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in self#ctyp _a3
      | `VirMeth (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in self#ctyp _a3
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method cldecl : cldecl -> 'self_type=
      function
      | `ClDecl (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#ident _a2 in
          let self = self#type_parameters _a3 in self#clexp _a4
      | `ClDeclS (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#ident _a2 in self#clexp _a3
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#cldecl _a1 in self#cldecl _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method clexp : clexp -> 'self_type=
      function
      | `CeApp (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#clexp _a1 in self#exp _a2
      | #vid' as _a0 -> (self#vid' _a0 :>'self_type)
      | `ClApply (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#vid _a1 in self#type_parameters _a2
      | `CeFun (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in self#clexp _a2
      | `LetIn (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#bind _a2 in self#clexp _a3
      | `Obj (_a0,_a1) -> let self = self#loc _a0 in self#clfield _a1
      | `ObjEnd _a0 -> self#loc _a0
      | `ObjPat (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in self#clfield _a2
      | `ObjPatEnd (_a0,_a1) -> let self = self#loc _a0 in self#pat _a1
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#clexp _a1 in self#cltyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method clfield : clfield -> 'self_type=
      function
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#clfield _a1 in self#clfield _a2
      | `Inherit (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in self#clexp _a2
      | `InheritAs (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#clexp _a2 in self#alident _a3
      | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in
          let self = self#flag _a3 in self#exp _a4
      | `VirVal (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in self#ctyp _a3
      | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in
          let self = self#flag _a3 in
          let self = self#exp _a4 in self#ctyp _a5
      | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in
          let self = self#flag _a3 in self#exp _a4
      | `VirMeth (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in self#ctyp _a3
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `Initializer (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method ep : ep -> 'self_type=
      function
      | #vid as _a0 -> (self#vid _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ep _a1 in self#ep _a2
      | `Vrn (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ep _a1 in self#ep _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ep _a1 in self#ep _a2
      | `Par (_a0,_a1) -> let self = self#loc _a0 in self#ep _a1
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ep _a1 in self#ctyp _a2
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> let self = self#loc _a0 in self#ep _a1
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#rec_bind _a1
      | #literal as _a0 -> (self#literal _a0 :>'self_type)
    method rec_bind : rec_bind -> 'self_type=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#vid _a1 in self#ep _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_bind _a1 in self#rec_bind _a2
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method tokenf_ant : Tokenf.ant -> 'self_type= self#unknown
    method locf_t : Locf.t -> 'self_type= self#unknown
  end
let strip_literal: Astf.literal -> Astfn.literal =
  function
  | `Chr (_a0,_a1) -> `Chr _a1
  | `Int (_a0,_a1) -> `Int _a1
  | `Int32 (_a0,_a1) -> `Int32 _a1
  | `Int64 (_a0,_a1) -> `Int64 _a1
  | `Flo (_a0,_a1) -> `Flo _a1
  | `Nativeint (_a0,_a1) -> `Nativeint _a1
  | `Str (_a0,_a1) -> `Str _a1
let strip_flag: Astf.flag -> Astfn.flag =
  function
  | `Positive _a0 -> `Positive
  | `Negative _a0 -> `Negative
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.flag)
let strip_position_flag: Astf.position_flag -> Astfn.position_flag =
  function
  | `Positive _a0 -> `Positive
  | `Negative _a0 -> `Negative
  | `Normal _a0 -> `Normal
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.position_flag)
let rec strip_strings: Astf.strings -> Astfn.strings =
  function
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_strings _a1 in
      let _a2 = strip_strings _a2 in `App (_a1, _a2)
  | `Str (_a0,_a1) -> `Str _a1
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.strings)
let strip_lident: Astf.lident -> Astfn.lident =
  fun (`Lid (_a0,_a1))  -> `Lid _a1
let strip_alident: Astf.alident -> Astfn.alident =
  function
  | `Lid (_a0,_a1) -> `Lid _a1
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.alident)
let strip_auident: Astf.auident -> Astfn.auident =
  function
  | `Uid (_a0,_a1) -> `Uid _a1
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.auident)
let strip_aident: Astf.aident -> Astfn.aident =
  function
  | #alident as _a0 -> (strip_alident _a0 :>Astfn.aident)
  | #auident as _a0 -> (strip_auident _a0 :>Astfn.aident)
let strip_astring: Astf.astring -> Astfn.astring =
  function
  | `C (_a0,_a1) -> `C _a1
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.astring)
let rec strip_uident: Astf.uident -> Astfn.uident =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_uident _a1 in
      let _a2 = strip_uident _a2 in `Dot (_a1, _a2)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_uident _a1 in
      let _a2 = strip_uident _a2 in `App (_a1, _a2)
  | #auident as _a0 -> (strip_auident _a0 :>Astfn.uident)
let rec strip_ident: Astf.ident -> Astfn.ident =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_ident _a1 in
      let _a2 = strip_ident _a2 in `Dot (_a1, _a2)
  | `Apply (_a0,_a1,_a2) ->
      let _a1 = strip_ident _a1 in
      let _a2 = strip_ident _a2 in `Apply (_a1, _a2)
  | #alident as _a0 -> (strip_alident _a0 :>Astfn.ident)
  | #auident as _a0 -> (strip_auident _a0 :>Astfn.ident)
let strip_ident': Astf.ident' -> Astfn.ident' =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_ident _a1 in
      let _a2 = strip_ident _a2 in `Dot (_a1, _a2)
  | `Apply (_a0,_a1,_a2) ->
      let _a1 = strip_ident _a1 in
      let _a2 = strip_ident _a2 in `Apply (_a1, _a2)
  | `Lid (_a0,_a1) -> `Lid _a1
  | `Uid (_a0,_a1) -> `Uid _a1
let rec strip_vid: Astf.vid -> Astfn.vid =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_vid _a1 in let _a2 = strip_vid _a2 in `Dot (_a1, _a2)
  | `Lid (_a0,_a1) -> `Lid _a1
  | `Uid (_a0,_a1) -> `Uid _a1
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.vid)
let strip_vid': Astf.vid' -> Astfn.vid' =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_vid _a1 in let _a2 = strip_vid _a2 in `Dot (_a1, _a2)
  | `Lid (_a0,_a1) -> `Lid _a1
  | `Uid (_a0,_a1) -> `Uid _a1
let rec strip_dupath: Astf.dupath -> Astfn.dupath =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_dupath _a1 in
      let _a2 = strip_dupath _a2 in `Dot (_a1, _a2)
  | #auident as _a0 -> (strip_auident _a0 :>Astfn.dupath)
let strip_dlpath: Astf.dlpath -> Astfn.dlpath =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_dupath _a1 in
      let _a2 = strip_alident _a2 in `Dot (_a1, _a2)
  | #alident as _a0 -> (strip_alident _a0 :>Astfn.dlpath)
let strip_any: Astf.any -> Astfn.any = fun (`Any _a0)  -> `Any
let rec strip_ctyp: Astf.ctyp -> Astfn.ctyp =
  function
  | `Alias (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in
      let _a2 = strip_alident _a2 in `Alias (_a1, _a2)
  | #any as _a0 -> (strip_any _a0 :>Astfn.ctyp)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in let _a2 = strip_ctyp _a2 in `App (_a1, _a2)
  | `Arrow (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in
      let _a2 = strip_ctyp _a2 in `Arrow (_a1, _a2)
  | `ClassPath (_a0,_a1) -> let _a1 = strip_ident _a1 in `ClassPath _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_ctyp _a2 in `Label (_a1, _a2)
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_ctyp _a2 in `OptLabl (_a1, _a2)
  | #ident' as _a0 -> (strip_ident' _a0 :>Astfn.ctyp)
  | `TyObj (_a0,_a1,_a2) ->
      let _a1 = strip_name_ctyp _a1 in
      let _a2 = strip_flag _a2 in `TyObj (_a1, _a2)
  | `TyObjEnd (_a0,_a1) -> let _a1 = strip_flag _a1 in `TyObjEnd _a1
  | `TyPol (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in
      let _a2 = strip_ctyp _a2 in `TyPol (_a1, _a2)
  | `TyPolEnd (_a0,_a1) -> let _a1 = strip_ctyp _a1 in `TyPolEnd _a1
  | `TyTypePol (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in
      let _a2 = strip_ctyp _a2 in `TyTypePol (_a1, _a2)
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = strip_position_flag _a1 in
      let _a2 = strip_alident _a2 in `Quote (_a1, _a2)
  | `QuoteAny (_a0,_a1) -> let _a1 = strip_position_flag _a1 in `QuoteAny _a1
  | `Par (_a0,_a1) -> let _a1 = strip_ctyp _a1 in `Par _a1
  | `Sta (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in let _a2 = strip_ctyp _a2 in `Sta (_a1, _a2)
  | `PolyEq (_a0,_a1) -> let _a1 = strip_row_field _a1 in `PolyEq _a1
  | `PolySup (_a0,_a1) -> let _a1 = strip_row_field _a1 in `PolySup _a1
  | `PolyInf (_a0,_a1) -> let _a1 = strip_row_field _a1 in `PolyInf _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in let _a2 = strip_ctyp _a2 in `Com (_a1, _a2)
  | `PolyInfSup (_a0,_a1,_a2) ->
      let _a1 = strip_row_field _a1 in
      let _a2 = strip_tag_names _a2 in `PolyInfSup (_a1, _a2)
  | `Package (_a0,_a1) -> let _a1 = strip_mtyp _a1 in `Package _a1
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.ctyp)
and strip_type_parameters: Astf.type_parameters -> Astfn.type_parameters =
  function
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_type_parameters _a1 in
      let _a2 = strip_type_parameters _a2 in `Com (_a1, _a2)
  | `Ctyp (_a0,_a1) -> let _a1 = strip_ctyp _a1 in `Ctyp _a1
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.type_parameters)
and strip_row_field: Astf.row_field -> Astfn.row_field =
  function
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.row_field)
  | `Bar (_a0,_a1,_a2) ->
      let _a1 = strip_row_field _a1 in
      let _a2 = strip_row_field _a2 in `Bar (_a1, _a2)
  | `TyVrn (_a0,_a1) -> let _a1 = strip_astring _a1 in `TyVrn _a1
  | `TyVrnOf (_a0,_a1,_a2) ->
      let _a1 = strip_astring _a1 in
      let _a2 = strip_ctyp _a2 in `TyVrnOf (_a1, _a2)
  | `Ctyp (_a0,_a1) -> let _a1 = strip_ctyp _a1 in `Ctyp _a1
and strip_tag_names: Astf.tag_names -> Astfn.tag_names =
  function
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.tag_names)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_tag_names _a1 in
      let _a2 = strip_tag_names _a2 in `App (_a1, _a2)
  | `TyVrn (_a0,_a1) -> let _a1 = strip_astring _a1 in `TyVrn _a1
and strip_typedecl: Astf.typedecl -> Astfn.typedecl =
  function
  | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_opt_decl_params _a2 in
      let _a3 = strip_type_info _a3 in
      let _a4 = strip_opt_type_constr _a4 in `TyDcl (_a1, _a2, _a3, _a4)
  | `TyAbstr (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_opt_decl_params _a2 in
      let _a3 = strip_opt_type_constr _a3 in `TyAbstr (_a1, _a2, _a3)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_typedecl _a1 in
      let _a2 = strip_typedecl _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.typedecl)
and strip_type_constr: Astf.type_constr -> Astfn.type_constr =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_type_constr _a1 in
      let _a2 = strip_type_constr _a2 in `And (_a1, _a2)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in let _a2 = strip_ctyp _a2 in `Eq (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.type_constr)
and strip_opt_type_constr: Astf.opt_type_constr -> Astfn.opt_type_constr =
  function
  | `Some (_a0,_a1) -> let _a1 = strip_type_constr _a1 in `Some _a1
  | `None _a0 -> `None
and strip_decl_param: Astf.decl_param -> Astfn.decl_param =
  function
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = strip_position_flag _a1 in
      let _a2 = strip_alident _a2 in `Quote (_a1, _a2)
  | `QuoteAny (_a0,_a1) -> let _a1 = strip_position_flag _a1 in `QuoteAny _a1
  | `Any _a0 -> `Any
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.decl_param)
and strip_decl_params: Astf.decl_params -> Astfn.decl_params =
  function
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = strip_position_flag _a1 in
      let _a2 = strip_alident _a2 in `Quote (_a1, _a2)
  | `QuoteAny (_a0,_a1) -> let _a1 = strip_position_flag _a1 in `QuoteAny _a1
  | `Any _a0 -> `Any
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_decl_params _a1 in
      let _a2 = strip_decl_params _a2 in `Com (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.decl_params)
and strip_opt_decl_params: Astf.opt_decl_params -> Astfn.opt_decl_params =
  function
  | `Some (_a0,_a1) -> let _a1 = strip_decl_params _a1 in `Some _a1
  | `None _a0 -> `None
and strip_type_info: Astf.type_info -> Astfn.type_info =
  function
  | `TyMan (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_ctyp _a1 in
      let _a2 = strip_flag _a2 in
      let _a3 = strip_type_repr _a3 in `TyMan (_a1, _a2, _a3)
  | `TyRepr (_a0,_a1,_a2) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_type_repr _a2 in `TyRepr (_a1, _a2)
  | `TyEq (_a0,_a1,_a2) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_ctyp _a2 in `TyEq (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.type_info)
and strip_type_repr: Astf.type_repr -> Astfn.type_repr =
  function
  | `Record (_a0,_a1) -> let _a1 = strip_name_ctyp _a1 in `Record _a1
  | `Sum (_a0,_a1) -> let _a1 = strip_or_ctyp _a1 in `Sum _a1
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.type_repr)
and strip_name_ctyp: Astf.name_ctyp -> Astfn.name_ctyp =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_name_ctyp _a1 in
      let _a2 = strip_name_ctyp _a2 in `Sem (_a1, _a2)
  | `TyCol (_a0,_a1,_a2) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_ctyp _a2 in `TyCol (_a1, _a2)
  | `TyColMut (_a0,_a1,_a2) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_ctyp _a2 in `TyColMut (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.name_ctyp)
and strip_or_ctyp: Astf.or_ctyp -> Astfn.or_ctyp =
  function
  | `Bar (_a0,_a1,_a2) ->
      let _a1 = strip_or_ctyp _a1 in
      let _a2 = strip_or_ctyp _a2 in `Bar (_a1, _a2)
  | `TyCol (_a0,_a1,_a2) ->
      let _a1 = strip_auident _a1 in
      let _a2 = strip_ctyp _a2 in `TyCol (_a1, _a2)
  | `Of (_a0,_a1,_a2) ->
      let _a1 = strip_auident _a1 in
      let _a2 = strip_ctyp _a2 in `Of (_a1, _a2)
  | #auident as _a0 -> (strip_auident _a0 :>Astfn.or_ctyp)
and strip_of_ctyp: Astf.of_ctyp -> Astfn.of_ctyp =
  function
  | `Of (_a0,_a1,_a2) ->
      let _a1 = strip_vid _a1 in let _a2 = strip_ctyp _a2 in `Of (_a1, _a2)
  | #vid' as _a0 -> (strip_vid' _a0 :>Astfn.of_ctyp)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.of_ctyp)
and strip_pat: Astf.pat -> Astfn.pat =
  function
  | #vid as _a0 -> (strip_vid _a0 :>Astfn.pat)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_pat _a1 in let _a2 = strip_pat _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_pat _a1 in let _a2 = strip_pat _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_pat _a1 in let _a2 = strip_pat _a2 in `Sem (_a1, _a2)
  | `Par (_a0,_a1) -> let _a1 = strip_pat _a1 in `Par _a1
  | #any as _a0 -> (strip_any _a0 :>Astfn.pat)
  | `Record (_a0,_a1) -> let _a1 = strip_rec_pat _a1 in `Record _a1
  | #literal as _a0 -> (strip_literal _a0 :>Astfn.pat)
  | `Alias (_a0,_a1,_a2) ->
      let _a1 = strip_pat _a1 in
      let _a2 = strip_alident _a2 in `Alias (_a1, _a2)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = strip_pat _a1 in `Array _a1
  | `LabelS (_a0,_a1) -> let _a1 = strip_alident _a1 in `LabelS _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_pat _a2 in `Label (_a1, _a2)
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_pat _a2 in `OptLabl (_a1, _a2)
  | `OptLablS (_a0,_a1) -> let _a1 = strip_alident _a1 in `OptLablS _a1
  | `OptLablExpr (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_pat _a2 in
      let _a3 = strip_exp _a3 in `OptLablExpr (_a1, _a2, _a3)
  | `Bar (_a0,_a1,_a2) ->
      let _a1 = strip_pat _a1 in let _a2 = strip_pat _a2 in `Bar (_a1, _a2)
  | `PaRng (_a0,_a1,_a2) ->
      let _a1 = strip_pat _a1 in let _a2 = strip_pat _a2 in `PaRng (_a1, _a2)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_pat _a1 in
      let _a2 = strip_ctyp _a2 in `Constraint (_a1, _a2)
  | `ClassPath (_a0,_a1) -> let _a1 = strip_ident _a1 in `ClassPath _a1
  | `Lazy (_a0,_a1) -> let _a1 = strip_pat _a1 in `Lazy _a1
  | `ModuleUnpack (_a0,_a1) ->
      let _a1 = strip_auident _a1 in `ModuleUnpack _a1
  | `ModuleConstraint (_a0,_a1,_a2) ->
      let _a1 = strip_auident _a1 in
      let _a2 = strip_ctyp _a2 in `ModuleConstraint (_a1, _a2)
and strip_rec_pat: Astf.rec_pat -> Astfn.rec_pat =
  function
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_vid _a1 in
      let _a2 = strip_pat _a2 in `RecBind (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_rec_pat _a1 in
      let _a2 = strip_rec_pat _a2 in `Sem (_a1, _a2)
  | #any as _a0 -> (strip_any _a0 :>Astfn.rec_pat)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.rec_pat)
and strip_exp: Astf.exp -> Astfn.exp =
  function
  | #vid as _a0 -> (strip_vid _a0 :>Astfn.exp)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in let _a2 = strip_exp _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in let _a2 = strip_exp _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in let _a2 = strip_exp _a2 in `Sem (_a1, _a2)
  | `Par (_a0,_a1) -> let _a1 = strip_exp _a1 in `Par _a1
  | #any as _a0 -> (strip_any _a0 :>Astfn.exp)
  | `Record (_a0,_a1) -> let _a1 = strip_rec_exp _a1 in `Record _a1
  | #literal as _a0 -> (strip_literal _a0 :>Astfn.exp)
  | `RecordWith (_a0,_a1,_a2) ->
      let _a1 = strip_rec_exp _a1 in
      let _a2 = strip_exp _a2 in `RecordWith (_a1, _a2)
  | `Field (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in let _a2 = strip_vid _a2 in `Field (_a1, _a2)
  | `ArrayDot (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in
      let _a2 = strip_exp _a2 in `ArrayDot (_a1, _a2)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = strip_exp _a1 in `Array _a1
  | `Assert (_a0,_a1) -> let _a1 = strip_exp _a1 in `Assert _a1
  | `Assign (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in
      let _a2 = strip_exp _a2 in `Assign (_a1, _a2)
  | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_exp _a2 in
      let _a3 = strip_exp _a3 in
      let _a4 = strip_flag _a4 in
      let _a5 = strip_exp _a5 in `For (_a1, _a2, _a3, _a4, _a5)
  | `Fun (_a0,_a1) -> let _a1 = strip_case _a1 in `Fun _a1
  | `IfThenElse (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_exp _a1 in
      let _a2 = strip_exp _a2 in
      let _a3 = strip_exp _a3 in `IfThenElse (_a1, _a2, _a3)
  | `IfThen (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in
      let _a2 = strip_exp _a2 in `IfThen (_a1, _a2)
  | `LabelS (_a0,_a1) -> let _a1 = strip_alident _a1 in `LabelS _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_exp _a2 in `Label (_a1, _a2)
  | `Lazy (_a0,_a1) -> let _a1 = strip_exp _a1 in `Lazy _a1
  | `LetIn (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_bind _a2 in
      let _a3 = strip_exp _a3 in `LetIn (_a1, _a2, _a3)
  | `LetTryInWith (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_bind _a2 in
      let _a3 = strip_exp _a3 in
      let _a4 = strip_case _a4 in `LetTryInWith (_a1, _a2, _a3, _a4)
  | `LetModule (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_auident _a1 in
      let _a2 = strip_mexp _a2 in
      let _a3 = strip_exp _a3 in `LetModule (_a1, _a2, _a3)
  | `Match (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in
      let _a2 = strip_case _a2 in `Match (_a1, _a2)
  | `New (_a0,_a1) -> let _a1 = strip_ident _a1 in `New _a1
  | `Obj (_a0,_a1) -> let _a1 = strip_clfield _a1 in `Obj _a1
  | `ObjEnd _a0 -> `ObjEnd
  | `ObjPat (_a0,_a1,_a2) ->
      let _a1 = strip_pat _a1 in
      let _a2 = strip_clfield _a2 in `ObjPat (_a1, _a2)
  | `ObjPatEnd (_a0,_a1) -> let _a1 = strip_pat _a1 in `ObjPatEnd _a1
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_exp _a2 in `OptLabl (_a1, _a2)
  | `OptLablS (_a0,_a1) -> let _a1 = strip_alident _a1 in `OptLablS _a1
  | `OvrInst (_a0,_a1) -> let _a1 = strip_rec_exp _a1 in `OvrInst _a1
  | `OvrInstEmpty _a0 -> `OvrInstEmpty
  | `Seq (_a0,_a1) -> let _a1 = strip_exp _a1 in `Seq _a1
  | `Send (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in
      let _a2 = strip_alident _a2 in `Send (_a1, _a2)
  | `StringDot (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in
      let _a2 = strip_exp _a2 in `StringDot (_a1, _a2)
  | `Try (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in let _a2 = strip_case _a2 in `Try (_a1, _a2)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in
      let _a2 = strip_ctyp _a2 in `Constraint (_a1, _a2)
  | `Coercion (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_exp _a1 in
      let _a2 = strip_ctyp _a2 in
      let _a3 = strip_ctyp _a3 in `Coercion (_a1, _a2, _a3)
  | `Subtype (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in
      let _a2 = strip_ctyp _a2 in `Subtype (_a1, _a2)
  | `While (_a0,_a1,_a2) ->
      let _a1 = strip_exp _a1 in let _a2 = strip_exp _a2 in `While (_a1, _a2)
  | `LetOpen (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_ident _a2 in
      let _a3 = strip_exp _a3 in `LetOpen (_a1, _a2, _a3)
  | `LocalTypeFun (_a0,_a1,_a2) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_exp _a2 in `LocalTypeFun (_a1, _a2)
  | `Package_exp (_a0,_a1) -> let _a1 = strip_mexp _a1 in `Package_exp _a1
and strip_rec_exp: Astf.rec_exp -> Astfn.rec_exp =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_rec_exp _a1 in
      let _a2 = strip_rec_exp _a2 in `Sem (_a1, _a2)
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_vid _a1 in
      let _a2 = strip_exp _a2 in `RecBind (_a1, _a2)
  | #any as _a0 -> (strip_any _a0 :>Astfn.rec_exp)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.rec_exp)
and strip_mtyp: Astf.mtyp -> Astfn.mtyp =
  function
  | #ident' as _a0 -> (strip_ident' _a0 :>Astfn.mtyp)
  | `Sig (_a0,_a1) -> let _a1 = strip_sigi _a1 in `Sig _a1
  | `SigEnd _a0 -> `SigEnd
  | `Functor (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_auident _a1 in
      let _a2 = strip_mtyp _a2 in
      let _a3 = strip_mtyp _a3 in `Functor (_a1, _a2, _a3)
  | `With (_a0,_a1,_a2) ->
      let _a1 = strip_mtyp _a1 in
      let _a2 = strip_constr _a2 in `With (_a1, _a2)
  | `ModuleTypeOf (_a0,_a1) -> let _a1 = strip_mexp _a1 in `ModuleTypeOf _a1
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.mtyp)
and strip_sigi: Astf.sigi -> Astfn.sigi =
  function
  | `Val (_a0,_a1,_a2) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_ctyp _a2 in `Val (_a1, _a2)
  | `External (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_ctyp _a2 in
      let _a3 = strip_strings _a3 in `External (_a1, _a2, _a3)
  | `Type (_a0,_a1) -> let _a1 = strip_typedecl _a1 in `Type _a1
  | `Exception (_a0,_a1) -> let _a1 = strip_of_ctyp _a1 in `Exception _a1
  | `Class (_a0,_a1) -> let _a1 = strip_cltdecl _a1 in `Class _a1
  | `ClassType (_a0,_a1) -> let _a1 = strip_cltdecl _a1 in `ClassType _a1
  | `Module (_a0,_a1,_a2) ->
      let _a1 = strip_auident _a1 in
      let _a2 = strip_mtyp _a2 in `Module (_a1, _a2)
  | `ModuleTypeEnd (_a0,_a1) ->
      let _a1 = strip_auident _a1 in `ModuleTypeEnd _a1
  | `ModuleType (_a0,_a1,_a2) ->
      let _a1 = strip_auident _a1 in
      let _a2 = strip_mtyp _a2 in `ModuleType (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_sigi _a1 in let _a2 = strip_sigi _a2 in `Sem (_a1, _a2)
  | `DirectiveSimple (_a0,_a1) ->
      let _a1 = strip_alident _a1 in `DirectiveSimple _a1
  | `Directive (_a0,_a1,_a2) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_exp _a2 in `Directive (_a1, _a2)
  | `Open (_a0,_a1,_a2) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_ident _a2 in `Open (_a1, _a2)
  | `Include (_a0,_a1) -> let _a1 = strip_mtyp _a1 in `Include _a1
  | `RecModule (_a0,_a1) -> let _a1 = strip_mbind _a1 in `RecModule _a1
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.sigi)
and strip_mbind: Astf.mbind -> Astfn.mbind =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_mbind _a1 in
      let _a2 = strip_mbind _a2 in `And (_a1, _a2)
  | `ModuleBind (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_auident _a1 in
      let _a2 = strip_mtyp _a2 in
      let _a3 = strip_mexp _a3 in `ModuleBind (_a1, _a2, _a3)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_auident _a1 in
      let _a2 = strip_mtyp _a2 in `Constraint (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.mbind)
and strip_constr: Astf.constr -> Astfn.constr =
  function
  | `TypeEq (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in
      let _a2 = strip_ctyp _a2 in `TypeEq (_a1, _a2)
  | `ModuleEq (_a0,_a1,_a2) ->
      let _a1 = strip_ident _a1 in
      let _a2 = strip_ident _a2 in `ModuleEq (_a1, _a2)
  | `TypeEqPriv (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in
      let _a2 = strip_ctyp _a2 in `TypeEqPriv (_a1, _a2)
  | `TypeSubst (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in
      let _a2 = strip_ctyp _a2 in `TypeSubst (_a1, _a2)
  | `ModuleSubst (_a0,_a1,_a2) ->
      let _a1 = strip_ident _a1 in
      let _a2 = strip_ident _a2 in `ModuleSubst (_a1, _a2)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_constr _a1 in
      let _a2 = strip_constr _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.constr)
and strip_bind: Astf.bind -> Astfn.bind =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_bind _a1 in let _a2 = strip_bind _a2 in `And (_a1, _a2)
  | `Bind (_a0,_a1,_a2) ->
      let _a1 = strip_pat _a1 in let _a2 = strip_exp _a2 in `Bind (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.bind)
and strip_case: Astf.case -> Astfn.case =
  function
  | `Bar (_a0,_a1,_a2) ->
      let _a1 = strip_case _a1 in let _a2 = strip_case _a2 in `Bar (_a1, _a2)
  | `Case (_a0,_a1,_a2) ->
      let _a1 = strip_pat _a1 in let _a2 = strip_exp _a2 in `Case (_a1, _a2)
  | `CaseWhen (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_pat _a1 in
      let _a2 = strip_exp _a2 in
      let _a3 = strip_exp _a3 in `CaseWhen (_a1, _a2, _a3)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.case)
and strip_mexp: Astf.mexp -> Astfn.mexp =
  function
  | #vid' as _a0 -> (strip_vid' _a0 :>Astfn.mexp)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_mexp _a1 in let _a2 = strip_mexp _a2 in `App (_a1, _a2)
  | `Functor (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_auident _a1 in
      let _a2 = strip_mtyp _a2 in
      let _a3 = strip_mexp _a3 in `Functor (_a1, _a2, _a3)
  | `Struct (_a0,_a1) -> let _a1 = strip_stru _a1 in `Struct _a1
  | `StructEnd _a0 -> `StructEnd
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_mexp _a1 in
      let _a2 = strip_mtyp _a2 in `Constraint (_a1, _a2)
  | `PackageModule (_a0,_a1) -> let _a1 = strip_exp _a1 in `PackageModule _a1
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.mexp)
and strip_stru: Astf.stru -> Astfn.stru =
  function
  | `Class (_a0,_a1) -> let _a1 = strip_cldecl _a1 in `Class _a1
  | `ClassType (_a0,_a1) -> let _a1 = strip_cltdecl _a1 in `ClassType _a1
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_stru _a1 in let _a2 = strip_stru _a2 in `Sem (_a1, _a2)
  | `DirectiveSimple (_a0,_a1) ->
      let _a1 = strip_alident _a1 in `DirectiveSimple _a1
  | `Directive (_a0,_a1,_a2) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_exp _a2 in `Directive (_a1, _a2)
  | `Exception (_a0,_a1) -> let _a1 = strip_of_ctyp _a1 in `Exception _a1
  | `StExp (_a0,_a1) -> let _a1 = strip_exp _a1 in `StExp _a1
  | `External (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_ctyp _a2 in
      let _a3 = strip_strings _a3 in `External (_a1, _a2, _a3)
  | `Include (_a0,_a1) -> let _a1 = strip_mexp _a1 in `Include _a1
  | `Module (_a0,_a1,_a2) ->
      let _a1 = strip_auident _a1 in
      let _a2 = strip_mexp _a2 in `Module (_a1, _a2)
  | `RecModule (_a0,_a1) -> let _a1 = strip_mbind _a1 in `RecModule _a1
  | `ModuleType (_a0,_a1,_a2) ->
      let _a1 = strip_auident _a1 in
      let _a2 = strip_mtyp _a2 in `ModuleType (_a1, _a2)
  | `Open (_a0,_a1,_a2) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_ident _a2 in `Open (_a1, _a2)
  | `Type (_a0,_a1) -> let _a1 = strip_typedecl _a1 in `Type _a1
  | `TypeWith (_a0,_a1,_a2) ->
      let _a1 = strip_typedecl _a1 in
      let _a2 = strip_strings _a2 in `TypeWith (_a1, _a2)
  | `Value (_a0,_a1,_a2) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_bind _a2 in `Value (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.stru)
and strip_cltdecl: Astf.cltdecl -> Astfn.cltdecl =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_cltdecl _a1 in
      let _a2 = strip_cltdecl _a2 in `And (_a1, _a2)
  | `CtDecl (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_ident _a2 in
      let _a3 = strip_type_parameters _a3 in
      let _a4 = strip_cltyp _a4 in `CtDecl (_a1, _a2, _a3, _a4)
  | `CtDeclS (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_ident _a2 in
      let _a3 = strip_cltyp _a3 in `CtDeclS (_a1, _a2, _a3)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.cltdecl)
and strip_cltyp: Astf.cltyp -> Astfn.cltyp =
  function
  | #vid' as _a0 -> (strip_vid' _a0 :>Astfn.cltyp)
  | `ClApply (_a0,_a1,_a2) ->
      let _a1 = strip_vid _a1 in
      let _a2 = strip_type_parameters _a2 in `ClApply (_a1, _a2)
  | `CtFun (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in
      let _a2 = strip_cltyp _a2 in `CtFun (_a1, _a2)
  | `ObjTy (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in
      let _a2 = strip_clsigi _a2 in `ObjTy (_a1, _a2)
  | `ObjTyEnd (_a0,_a1) -> let _a1 = strip_ctyp _a1 in `ObjTyEnd _a1
  | `Obj (_a0,_a1) -> let _a1 = strip_clsigi _a1 in `Obj _a1
  | `ObjEnd _a0 -> `ObjEnd
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_cltyp _a1 in
      let _a2 = strip_cltyp _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.cltyp)
and strip_clsigi: Astf.clsigi -> Astfn.clsigi =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_clsigi _a1 in
      let _a2 = strip_clsigi _a2 in `Sem (_a1, _a2)
  | `SigInherit (_a0,_a1) -> let _a1 = strip_cltyp _a1 in `SigInherit _a1
  | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_flag _a2 in
      let _a3 = strip_flag _a3 in
      let _a4 = strip_ctyp _a4 in `CgVal (_a1, _a2, _a3, _a4)
  | `Method (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_flag _a2 in
      let _a3 = strip_ctyp _a3 in `Method (_a1, _a2, _a3)
  | `VirMeth (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_flag _a2 in
      let _a3 = strip_ctyp _a3 in `VirMeth (_a1, _a2, _a3)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in let _a2 = strip_ctyp _a2 in `Eq (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.clsigi)
and strip_cldecl: Astf.cldecl -> Astfn.cldecl =
  function
  | `ClDecl (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_ident _a2 in
      let _a3 = strip_type_parameters _a3 in
      let _a4 = strip_clexp _a4 in `ClDecl (_a1, _a2, _a3, _a4)
  | `ClDeclS (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_ident _a2 in
      let _a3 = strip_clexp _a3 in `ClDeclS (_a1, _a2, _a3)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_cldecl _a1 in
      let _a2 = strip_cldecl _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.cldecl)
and strip_clexp: Astf.clexp -> Astfn.clexp =
  function
  | `CeApp (_a0,_a1,_a2) ->
      let _a1 = strip_clexp _a1 in
      let _a2 = strip_exp _a2 in `CeApp (_a1, _a2)
  | #vid' as _a0 -> (strip_vid' _a0 :>Astfn.clexp)
  | `ClApply (_a0,_a1,_a2) ->
      let _a1 = strip_vid _a1 in
      let _a2 = strip_type_parameters _a2 in `ClApply (_a1, _a2)
  | `CeFun (_a0,_a1,_a2) ->
      let _a1 = strip_pat _a1 in
      let _a2 = strip_clexp _a2 in `CeFun (_a1, _a2)
  | `LetIn (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_bind _a2 in
      let _a3 = strip_clexp _a3 in `LetIn (_a1, _a2, _a3)
  | `Obj (_a0,_a1) -> let _a1 = strip_clfield _a1 in `Obj _a1
  | `ObjEnd _a0 -> `ObjEnd
  | `ObjPat (_a0,_a1,_a2) ->
      let _a1 = strip_pat _a1 in
      let _a2 = strip_clfield _a2 in `ObjPat (_a1, _a2)
  | `ObjPatEnd (_a0,_a1) -> let _a1 = strip_pat _a1 in `ObjPatEnd _a1
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_clexp _a1 in
      let _a2 = strip_cltyp _a2 in `Constraint (_a1, _a2)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.clexp)
and strip_clfield: Astf.clfield -> Astfn.clfield =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_clfield _a1 in
      let _a2 = strip_clfield _a2 in `Sem (_a1, _a2)
  | `Inherit (_a0,_a1,_a2) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_clexp _a2 in `Inherit (_a1, _a2)
  | `InheritAs (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_flag _a1 in
      let _a2 = strip_clexp _a2 in
      let _a3 = strip_alident _a3 in `InheritAs (_a1, _a2, _a3)
  | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_flag _a2 in
      let _a3 = strip_flag _a3 in
      let _a4 = strip_exp _a4 in `CrVal (_a1, _a2, _a3, _a4)
  | `VirVal (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_flag _a2 in
      let _a3 = strip_ctyp _a3 in `VirVal (_a1, _a2, _a3)
  | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_flag _a2 in
      let _a3 = strip_flag _a3 in
      let _a4 = strip_exp _a4 in
      let _a5 = strip_ctyp _a5 in `CrMth (_a1, _a2, _a3, _a4, _a5)
  | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_flag _a2 in
      let _a3 = strip_flag _a3 in
      let _a4 = strip_exp _a4 in `CrMthS (_a1, _a2, _a3, _a4)
  | `VirMeth (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_alident _a1 in
      let _a2 = strip_flag _a2 in
      let _a3 = strip_ctyp _a3 in `VirMeth (_a1, _a2, _a3)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_ctyp _a1 in let _a2 = strip_ctyp _a2 in `Eq (_a1, _a2)
  | `Initializer (_a0,_a1) -> let _a1 = strip_exp _a1 in `Initializer _a1
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.clfield)
let rec strip_ep: Astf.ep -> Astfn.ep =
  function
  | #vid as _a0 -> (strip_vid _a0 :>Astfn.ep)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_ep _a1 in let _a2 = strip_ep _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_ep _a1 in let _a2 = strip_ep _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_ep _a1 in let _a2 = strip_ep _a2 in `Sem (_a1, _a2)
  | `Par (_a0,_a1) -> let _a1 = strip_ep _a1 in `Par _a1
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_ep _a1 in
      let _a2 = strip_ctyp _a2 in `Constraint (_a1, _a2)
  | #any as _a0 -> (strip_any _a0 :>Astfn.ep)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = strip_ep _a1 in `Array _a1
  | `Record (_a0,_a1) -> let _a1 = strip_rec_bind _a1 in `Record _a1
  | #literal as _a0 -> (strip_literal _a0 :>Astfn.ep)
and strip_rec_bind: Astf.rec_bind -> Astfn.rec_bind =
  function
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_vid _a1 in
      let _a2 = strip_ep _a2 in `RecBind (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_rec_bind _a1 in
      let _a2 = strip_rec_bind _a2 in `Sem (_a1, _a2)
  | #any as _a0 -> (strip_any _a0 :>Astfn.rec_bind)
  | #ant as _a0 -> (strip_ant _a0 :>Astfn.rec_bind)
let map_loc f =
  object  inherit  map as super method! loc x = f (super#loc x) end
let map_ant f =
  object  inherit  map as super method! ant x = f (super#ant x) end
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
let dump_literal = Formatf.to_string dump#literal
let dump_flag = Formatf.to_string dump#flag
let dump_position_flag = Formatf.to_string dump#position_flag
let dump_strings = Formatf.to_string dump#strings
let dump_lident = Formatf.to_string dump#lident
let dump_alident = Formatf.to_string dump#alident
let dump_auident = Formatf.to_string dump#auident
let dump_aident = Formatf.to_string dump#aident
let dump_astring = Formatf.to_string dump#astring
let dump_uident = Formatf.to_string dump#uident
let dump_ident = Formatf.to_string dump#ident
let dump_ident' = Formatf.to_string dump#ident'
let dump_vid = Formatf.to_string dump#vid
let dump_vid' = Formatf.to_string dump#vid'
let dump_dupath = Formatf.to_string dump#dupath
let dump_dlpath = Formatf.to_string dump#dlpath
let dump_any = Formatf.to_string dump#any
let dump_ctyp = Formatf.to_string dump#ctyp
let dump_type_parameters = Formatf.to_string dump#type_parameters
let dump_row_field = Formatf.to_string dump#row_field
let dump_tag_names = Formatf.to_string dump#tag_names
let dump_typedecl = Formatf.to_string dump#typedecl
let dump_type_constr = Formatf.to_string dump#type_constr
let dump_opt_type_constr = Formatf.to_string dump#opt_type_constr
let dump_decl_param = Formatf.to_string dump#decl_param
let dump_decl_params = Formatf.to_string dump#decl_params
let dump_opt_decl_params = Formatf.to_string dump#opt_decl_params
let dump_type_info = Formatf.to_string dump#type_info
let dump_type_repr = Formatf.to_string dump#type_repr
let dump_name_ctyp = Formatf.to_string dump#name_ctyp
let dump_or_ctyp = Formatf.to_string dump#or_ctyp
let dump_of_ctyp = Formatf.to_string dump#of_ctyp
let dump_pat = Formatf.to_string dump#pat
let dump_rec_pat = Formatf.to_string dump#rec_pat
let dump_exp = Formatf.to_string dump#exp
let dump_rec_exp = Formatf.to_string dump#rec_exp
let dump_mtyp = Formatf.to_string dump#mtyp
let dump_sigi = Formatf.to_string dump#sigi
let dump_mbind = Formatf.to_string dump#mbind
let dump_constr = Formatf.to_string dump#constr
let dump_bind = Formatf.to_string dump#bind
let dump_case = Formatf.to_string dump#case
let dump_mexp = Formatf.to_string dump#mexp
let dump_stru = Formatf.to_string dump#stru
let dump_cltdecl = Formatf.to_string dump#cltdecl
let dump_cltyp = Formatf.to_string dump#cltyp
let dump_clsigi = Formatf.to_string dump#clsigi
let dump_cldecl = Formatf.to_string dump#cldecl
let dump_clexp = Formatf.to_string dump#clexp
let dump_clfield = Formatf.to_string dump#clfield
let dump_ep = Formatf.to_string dump#ep
let dump_rec_bind = Formatf.to_string dump#rec_bind
class reloc _loc = object  inherit  map method! loc _ = _loc end
let wildcarder =
  object (self)
    inherit  map as super
    method! pat =
      function
      | (`Lid (_loc,_) : Astf.pat) -> (`Any _loc : Astf.pat )
      | (`Alias (_loc,p,_) : Astf.pat) -> self#pat p
      | p -> super#pat p
  end
let () =
  Ast2pt.dump_ident := dump_ident;
  Ast2pt.dump_ident := dump_ident;
  Ast2pt.dump_row_field := dump_row_field;
  Ast2pt.dump_name_ctyp := dump_name_ctyp;
  Ast2pt.dump_constr := dump_constr;
  Ast2pt.dump_mtyp := dump_mtyp;
  Ast2pt.dump_ctyp := dump_ctyp;
  Ast2pt.dump_or_ctyp := dump_or_ctyp;
  Ast2pt.dump_pat := dump_pat;
  Ast2pt.dump_type_parameters := dump_type_parameters;
  Ast2pt.dump_exp := dump_exp;
  Ast2pt.dump_case := dump_case;
  Ast2pt.dump_rec_exp := dump_rec_exp;
  Ast2pt.dump_type_constr := dump_type_constr;
  Ast2pt.dump_typedecl := dump_typedecl;
  Ast2pt.dump_sigi := dump_sigi;
  Ast2pt.dump_mbind := dump_mbind;
  Ast2pt.dump_mexp := dump_mexp;
  Ast2pt.dump_stru := dump_stru;
  Ast2pt.dump_cltyp := dump_cltyp;
  Ast2pt.dump_cldecl := dump_cldecl;
  Ast2pt.dump_cltdecl := dump_cltdecl;
  Ast2pt.dump_clsigi := dump_clsigi;
  Ast2pt.dump_clexp := dump_clexp;
  Ast2pt.dump_clfield := dump_clfield
