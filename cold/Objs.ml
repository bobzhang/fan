open StdLib

open Ast

let strip_loc_ant ant = ant

let _ = (); ()

let _ = ()

let pp_print_loc fmt _a0 = FanLoc.pp_print_t fmt _a0

let pp_print_ant fmt (`Ant (_a0,_a1)) =
  Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" pp_print_loc _a0
    FanUtil.pp_print_anti_cxt _a1

let pp_print_nil fmt (`Nil _a0) =
  Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc _a0

let pp_print_literal fmt =
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

let pp_print_flag fmt =
  function
  | `Positive _a0 ->
      Format.fprintf fmt "@[<1>(`Positive@ %a)@]" pp_print_loc _a0
  | `Negative _a0 ->
      Format.fprintf fmt "@[<1>(`Negative@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result4)

let pp_print_position_flag fmt =
  function
  | `Positive _a0 ->
      Format.fprintf fmt "@[<1>(`Positive@ %a)@]" pp_print_loc _a0
  | `Negative _a0 ->
      Format.fprintf fmt "@[<1>(`Negative@ %a)@]" pp_print_loc _a0
  | `Normal _a0 -> Format.fprintf fmt "@[<1>(`Normal@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result5)

let rec pp_print_strings fmt =
  function
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_strings _a1 pp_print_strings _a2
  | `Str (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result6)

let pp_print_lident fmt (`Lid (_a0,_a1)) =
  Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0 pp_print_string
    _a1

let pp_print_alident fmt =
  function
  | `Lid (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result8)

let pp_print_auident fmt =
  function
  | `Uid (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result9)

let pp_print_aident fmt =
  function
  | #alident as _a0 -> (pp_print_alident fmt _a0 :>'result10)
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result10)

let pp_print_astring fmt =
  function
  | `C (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`C@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result11)

let rec pp_print_uident fmt =
  function
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_uident _a1 pp_print_uident _a2
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_uident _a1 pp_print_uident _a2
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result12)

let rec pp_print_ident fmt =
  function
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ident _a2
  | `Apply (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Apply@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ident _a2
  | #alident as _a0 -> (pp_print_alident fmt _a0 :>'result13)
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result13)

let pp_print_ident' fmt =
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

let rec pp_print_vid fmt =
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
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result15)

let pp_print_vid' fmt =
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

let rec pp_print_dupath fmt =
  function
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_dupath _a1 pp_print_dupath _a2
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result17)

let pp_print_dlpath fmt =
  function
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_dupath _a1 pp_print_alident _a2
  | #alident as _a0 -> (pp_print_alident fmt _a0 :>'result18)

let pp_print_any fmt (`Any _a0) =
  Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc _a0

let rec pp_print_ctyp fmt =
  function
  | `Alias (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_alident _a2
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result52)
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
  | #ident' as _a0 -> (pp_print_ident' fmt _a0 :>'result52)
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
      Format.fprintf fmt "@[<1>(`PolyInfSup@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_row_field _a1 pp_print_tag_names _a2
  | `Package (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Package@ %a@ %a)@]" pp_print_loc _a0
        pp_print_mtyp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result52)
and pp_print_type_parameters fmt =
  function
  | `Com (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_type_parameters _a1 pp_print_type_parameters _a2
  | `Ctyp (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Ctyp@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result51)
and pp_print_row_field fmt =
  function
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result50)
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
and pp_print_tag_names fmt =
  function
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result49)
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_tag_names _a1 pp_print_tag_names _a2
  | `TyVrn (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" pp_print_loc _a0
        pp_print_astring _a1
and pp_print_typedecl fmt =
  function
  | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
      Format.fprintf fmt "@[<1>(`TyDcl@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_opt_decl_params _a2
        pp_print_type_info _a3 pp_print_opt_type_constr _a4
  | `TyAbstr (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`TyAbstr@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_opt_decl_params _a2
        pp_print_opt_type_constr _a3
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_typedecl _a1 pp_print_typedecl _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result48)
and pp_print_type_constr fmt =
  function
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_type_constr _a1 pp_print_type_constr _a2
  | `Eq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result47)
and pp_print_opt_type_constr fmt =
  function
  | `Some (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Some@ %a@ %a)@]" pp_print_loc _a0
        pp_print_type_constr _a1
  | `None _a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" pp_print_loc _a0
and pp_print_decl_param fmt =
  function
  | `Quote (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_position_flag _a1 pp_print_alident _a2
  | `QuoteAny (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" pp_print_loc _a0
        pp_print_position_flag _a1
  | `Any _a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result45)
and pp_print_decl_params fmt =
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
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result44)
and pp_print_opt_decl_params fmt =
  function
  | `Some (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Some@ %a@ %a)@]" pp_print_loc _a0
        pp_print_decl_params _a1
  | `None _a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" pp_print_loc _a0
and pp_print_type_info fmt =
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
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result42)
and pp_print_type_repr fmt =
  function
  | `Record (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
        pp_print_name_ctyp _a1
  | `Sum (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Sum@ %a@ %a)@]" pp_print_loc _a0
        pp_print_or_ctyp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result41)
and pp_print_name_ctyp fmt =
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
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result40)
and pp_print_or_ctyp fmt =
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
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result39)
and pp_print_of_ctyp fmt =
  function
  | `Of (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_vid _a1 pp_print_ctyp _a2
  | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>'result38)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result38)
and pp_print_pat fmt =
  function
  | #vid as _a0 -> (pp_print_vid fmt _a0 :>'result37)
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
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result37)
  | `Record (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_pat _a1
  | #literal as _a0 -> (pp_print_literal fmt _a0 :>'result37)
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
      Format.fprintf fmt "@[<1>(`OptLablExpr@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_pat _a2 pp_print_exp _a3
  | `Bar (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Bar@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_pat _a2
  | `PaRng (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_pat _a2
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_ctyp _a2
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
and pp_print_rec_pat fmt =
  function
  | `RecBind (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_pat _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_pat _a1 pp_print_rec_pat _a2
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result36)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result36)
and pp_print_exp fmt =
  function
  | #vid as _a0 -> (pp_print_vid fmt _a0 :>'result35)
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
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result35)
  | `Record (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_exp _a1
  | #literal as _a0 -> (pp_print_literal fmt _a0 :>'result35)
  | `RecordWith (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_exp _a1 pp_print_exp _a2
  | `Field (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Field@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_exp _a2
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
      Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_exp _a2 pp_print_exp _a3
        pp_print_flag _a4 pp_print_exp _a5
  | `Fun (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Fun@ %a@ %a)@]" pp_print_loc _a0
        pp_print_case _a1
  | `IfThenElse (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_exp _a1 pp_print_exp _a2 pp_print_exp _a3
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
        pp_print_loc _a0 pp_print_flag _a1 pp_print_bind _a2 pp_print_exp _a3
        pp_print_case _a4
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
  | `ObjEnd _a0 -> Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" pp_print_loc _a0
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
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_ctyp _a2
  | `Coercion (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`Coercion@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_exp _a1 pp_print_ctyp _a2 pp_print_ctyp _a3
  | `Subtype (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Subtype@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_ctyp _a2
  | `While (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`While@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_exp _a2
  | `LetOpen (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_exp _a2
  | `LocalTypeFun (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_exp _a2
  | `Package_exp (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Package_exp@ %a@ %a)@]" pp_print_loc _a0
        pp_print_mexp _a1
and pp_print_rec_exp fmt =
  function
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_exp _a1 pp_print_rec_exp _a2
  | `RecBind (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_exp _a2
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result34)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result34)
and pp_print_mtyp fmt =
  function
  | #ident' as _a0 -> (pp_print_ident' fmt _a0 :>'result33)
  | `Sig (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Sig@ %a@ %a)@]" pp_print_loc _a0
        pp_print_sigi _a1
  | `SigEnd _a0 -> Format.fprintf fmt "@[<1>(`SigEnd@ %a)@]" pp_print_loc _a0
  | `Functor (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_mtyp _a2 pp_print_mtyp _a3
  | `With (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`With@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_mtyp _a1 pp_print_constr _a2
  | `ModuleTypeOf (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ModuleTypeOf@ %a@ %a)@]" pp_print_loc _a0
        pp_print_mexp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result33)
and pp_print_sigi fmt =
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
      Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_mtyp _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_sigi _a1 pp_print_sigi _a2
  | `DirectiveSimple (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
  | `Directive (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_exp _a2
  | `Open (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1
  | `Include (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" pp_print_loc _a0
        pp_print_mtyp _a1
  | `RecModule (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" pp_print_loc _a0
        pp_print_mbind _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result32)
and pp_print_mbind fmt =
  function
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_mbind _a1 pp_print_mbind _a2
  | `ModuleBind (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_auident _a1 pp_print_mtyp _a2 pp_print_mexp _a3
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_mtyp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result31)
and pp_print_constr fmt =
  function
  | `TypeEq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TypeEq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `ModuleEq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ModuleEq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ident _a2
  | `TypeEqPriv (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TypeEqPriv@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `TypeSubst (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TypeSubst@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `ModuleSubst (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ModuleSubst@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ident _a2
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_constr _a1 pp_print_constr _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result30)
and pp_print_bind fmt =
  function
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_bind _a1 pp_print_bind _a2
  | `Bind (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Bind@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_exp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result29)
and pp_print_case fmt =
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
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result28)
and pp_print_mexp fmt =
  function
  | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>'result27)
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_mexp _a1 pp_print_mexp _a2
  | `Functor (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_mtyp _a2 pp_print_mexp _a3
  | `Struct (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Struct@ %a@ %a)@]" pp_print_loc _a0
        pp_print_stru _a1
  | `StructEnd _a0 ->
      Format.fprintf fmt "@[<1>(`StructEnd@ %a)@]" pp_print_loc _a0
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_mexp _a1 pp_print_mtyp _a2
  | `PackageModule (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`PackageModule@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result27)
and pp_print_stru fmt =
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
      Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
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
      Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_mtyp _a2
  | `Open (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1
  | `Type (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" pp_print_loc _a0
        pp_print_typedecl _a1
  | `Value (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Value@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_flag _a1 pp_print_bind _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result26)
and pp_print_cltdecl fmt =
  function
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_cltdecl _a1 pp_print_cltdecl _a2
  | `CtDecl (_a0,_a1,_a2,_a3,_a4) ->
      Format.fprintf fmt "@[<1>(`CtDecl@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_flag _a1 pp_print_ident _a2 pp_print_type_parameters _a3
        pp_print_cltyp _a4
  | `CtDeclS (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`CtDeclS@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_flag _a1 pp_print_ident _a2 pp_print_cltyp _a3
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result25)
and pp_print_cltyp fmt =
  function
  | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>'result24)
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
  | `ObjEnd _a0 -> Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" pp_print_loc _a0
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_cltyp _a1 pp_print_cltyp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result24)
and pp_print_clsigi fmt =
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
      Format.fprintf fmt "@[<1>(`Method@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_flag _a2 pp_print_ctyp _a3
  | `VirMeth (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`VirMeth@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_flag _a2 pp_print_ctyp _a3
  | `Eq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result23)
and pp_print_cldecl fmt =
  function
  | `ClDecl (_a0,_a1,_a2,_a3,_a4) ->
      Format.fprintf fmt "@[<1>(`ClDecl@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_flag _a1 pp_print_ident _a2 pp_print_type_parameters _a3
        pp_print_clexp _a4
  | `ClDeclS (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`ClDeclS@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_flag _a1 pp_print_ident _a2 pp_print_clexp _a3
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_cldecl _a1 pp_print_cldecl _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result22)
and pp_print_clexp fmt =
  function
  | `CeApp (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_clexp _a1 pp_print_exp _a2
  | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>'result21)
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
  | `ObjEnd _a0 -> Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" pp_print_loc _a0
  | `ObjPat (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_clfield _a2
  | `ObjPatEnd (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_clexp _a1 pp_print_cltyp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result21)
and pp_print_clfield fmt =
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
      Format.fprintf fmt "@[<1>(`VirVal@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_flag _a2 pp_print_ctyp _a3
  | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
      Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
        pp_print_loc _a0 pp_print_alident _a1 pp_print_flag _a2 pp_print_flag
        _a3 pp_print_exp _a4 pp_print_ctyp _a5
  | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
      Format.fprintf fmt "@[<1>(`CrMthS@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_flag _a2 pp_print_flag _a3
        pp_print_exp _a4
  | `VirMeth (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`VirMeth@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_flag _a2 pp_print_ctyp _a3
  | `Eq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `Initializer (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Initializer@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result20)

let rec pp_print_ep fmt =
  function
  | #vid as _a0 -> (pp_print_vid fmt _a0 :>'result54)
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
      Format.fprintf fmt "@[<1>(`Par@ %a@ %a)@]" pp_print_loc _a0 pp_print_ep
        _a1
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result54)
  | `ArrayEmpty _a0 ->
      Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" pp_print_loc _a0
  | `Array (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ep _a1
  | `Record (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_bind _a1
  | #literal as _a0 -> (pp_print_literal fmt _a0 :>'result54)
and pp_print_rec_bind fmt =
  function
  | `RecBind (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ep _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_bind _a1 pp_print_rec_bind _a2
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result53)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result53)

class print =
  object (self : 'self_type)
    inherit  printbase
    method loc : 'fmt -> loc -> unit= fun fmt  _a0  -> self#fanloc_t fmt _a0
    method ant : 'fmt -> ant -> unit=
      fun fmt  (`Ant (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" self#loc _a0
          self#fanutil_anti_cxt _a1
    method nil : 'fmt -> nil -> unit=
      fun fmt  (`Nil _a0)  ->
        Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
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
              self#ident _a1 self#pat _a2
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
              self#exp _a1 self#exp _a2
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
        | `LetOpen (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#exp _a2
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
              self#ident _a1 self#exp _a2
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
        | `Open (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" self#loc _a0
              self#ident _a1
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
        | `Open (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" self#loc _a0
              self#ident _a1
        | `Type (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" self#loc _a0
              self#typedecl _a1
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
              self#ident _a1 self#ep _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_bind _a1 self#rec_bind _a2
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
    method nil : nil -> nil=
      fun (`Nil _a0)  -> let _a0 = self#loc _a0 in `Nil _a0
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
          let _a1 = self#ident _a1 in
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
          let _a2 = self#exp _a2 in `Field (_a0, _a1, _a2)
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
      | `LetOpen (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#exp _a2 in `LetOpen (_a0, _a1, _a2)
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
          let _a1 = self#ident _a1 in
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
      | `Open (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in `Open (_a0, _a1)
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
      | `Open (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in `Open (_a0, _a1)
      | `Type (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#typedecl _a1 in `Type (_a0, _a1)
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
          let _a1 = self#ident _a1 in
          let _a2 = self#ep _a2 in `RecBind (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_bind _a1 in
          let _a2 = self#rec_bind _a2 in `Sem (_a0, _a1, _a2)
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
    method nil : nil -> 'self_type= fun (`Nil _a0)  -> self#loc _a0
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
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#pat _a2
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
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
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
      | `LetOpen (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#exp _a2
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
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#exp _a2
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
      | `Open (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
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
      | `Open (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Type (_a0,_a1) -> let self = self#loc _a0 in self#typedecl _a1
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
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> let self = self#loc _a0 in self#ep _a1
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#rec_bind _a1
      | #literal as _a0 -> (self#literal _a0 :>'self_type)
    method rec_bind : rec_bind -> 'self_type=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ident _a1 in self#ep _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_bind _a1 in self#rec_bind _a2
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method fanloc_t : FanLoc.t -> 'self_type= self#unknown
    method fanutil_anti_cxt : FanUtil.anti_cxt -> 'self_type= self#unknown
  end

let strip_loc_nil (`Nil _a0) = `Nil

let strip_loc_literal =
  function
  | `Chr (_a0,_a1) -> `Chr _a1
  | `Int (_a0,_a1) -> `Int _a1
  | `Int32 (_a0,_a1) -> `Int32 _a1
  | `Int64 (_a0,_a1) -> `Int64 _a1
  | `Flo (_a0,_a1) -> `Flo _a1
  | `Nativeint (_a0,_a1) -> `Nativeint _a1
  | `Str (_a0,_a1) -> `Str _a1

let strip_loc_flag =
  function
  | `Positive _a0 -> `Positive
  | `Negative _a0 -> `Negative
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result228)

let strip_loc_position_flag =
  function
  | `Positive _a0 -> `Positive
  | `Negative _a0 -> `Negative
  | `Normal _a0 -> `Normal
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result229)

let rec strip_loc_strings =
  function
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_strings _a1 in
      let _a2 = strip_loc_strings _a2 in `App (_a1, _a2)
  | `Str (_a0,_a1) -> `Str _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result230)

let strip_loc_lident (`Lid (_a0,_a1)) = `Lid _a1

let strip_loc_alident =
  function
  | `Lid (_a0,_a1) -> `Lid _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result232)

let strip_loc_auident =
  function
  | `Uid (_a0,_a1) -> `Uid _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result233)

let strip_loc_aident =
  function
  | #alident as _a0 -> (strip_loc_alident _a0 :>'result234)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result234)

let strip_loc_astring =
  function
  | `C (_a0,_a1) -> `C _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result235)

let rec strip_loc_uident =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_uident _a1 in
      let _a2 = strip_loc_uident _a2 in `Dot (_a1, _a2)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_uident _a1 in
      let _a2 = strip_loc_uident _a2 in `App (_a1, _a2)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result236)

let rec strip_loc_ident =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `Dot (_a1, _a2)
  | `Apply (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `Apply (_a1, _a2)
  | #alident as _a0 -> (strip_loc_alident _a0 :>'result237)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result237)

let strip_loc_ident' =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `Dot (_a1, _a2)
  | `Apply (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `Apply (_a1, _a2)
  | `Lid (_a0,_a1) -> `Lid _a1
  | `Uid (_a0,_a1) -> `Uid _a1

let rec strip_loc_vid =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_vid _a1 in
      let _a2 = strip_loc_vid _a2 in `Dot (_a1, _a2)
  | `Lid (_a0,_a1) -> `Lid _a1
  | `Uid (_a0,_a1) -> `Uid _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result239)

let strip_loc_vid' =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_vid _a1 in
      let _a2 = strip_loc_vid _a2 in `Dot (_a1, _a2)
  | `Lid (_a0,_a1) -> `Lid _a1
  | `Uid (_a0,_a1) -> `Uid _a1

let rec strip_loc_dupath =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_dupath _a1 in
      let _a2 = strip_loc_dupath _a2 in `Dot (_a1, _a2)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result241)

let strip_loc_dlpath =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_dupath _a1 in
      let _a2 = strip_loc_alident _a2 in `Dot (_a1, _a2)
  | #alident as _a0 -> (strip_loc_alident _a0 :>'result242)

let strip_loc_any (`Any _a0) = `Any

let rec strip_loc_ctyp =
  function
  | `Alias (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_alident _a2 in `Alias (_a1, _a2)
  | #any as _a0 -> (strip_loc_any _a0 :>'result276)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `App (_a1, _a2)
  | `Arrow (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Arrow (_a1, _a2)
  | `ClassPath (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `ClassPath _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Label (_a1, _a2)
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `OptLabl (_a1, _a2)
  | #ident' as _a0 -> (strip_loc_ident' _a0 :>'result276)
  | `TyObj (_a0,_a1,_a2) ->
      let _a1 = strip_loc_name_ctyp _a1 in
      let _a2 = strip_loc_flag _a2 in `TyObj (_a1, _a2)
  | `TyObjEnd (_a0,_a1) -> let _a1 = strip_loc_flag _a1 in `TyObjEnd _a1
  | `TyPol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyPol (_a1, _a2)
  | `TyPolEnd (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `TyPolEnd _a1
  | `TyTypePol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyTypePol (_a1, _a2)
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = strip_loc_position_flag _a1 in
      let _a2 = strip_loc_alident _a2 in `Quote (_a1, _a2)
  | `QuoteAny (_a0,_a1) ->
      let _a1 = strip_loc_position_flag _a1 in `QuoteAny _a1
  | `Par (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Par _a1
  | `Sta (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Sta (_a1, _a2)
  | `PolyEq (_a0,_a1) -> let _a1 = strip_loc_row_field _a1 in `PolyEq _a1
  | `PolySup (_a0,_a1) -> let _a1 = strip_loc_row_field _a1 in `PolySup _a1
  | `PolyInf (_a0,_a1) -> let _a1 = strip_loc_row_field _a1 in `PolyInf _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Com (_a1, _a2)
  | `PolyInfSup (_a0,_a1,_a2) ->
      let _a1 = strip_loc_row_field _a1 in
      let _a2 = strip_loc_tag_names _a2 in `PolyInfSup (_a1, _a2)
  | `Package (_a0,_a1) -> let _a1 = strip_loc_mtyp _a1 in `Package _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result276)
and strip_loc_type_parameters =
  function
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_type_parameters _a1 in
      let _a2 = strip_loc_type_parameters _a2 in `Com (_a1, _a2)
  | `Ctyp (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Ctyp _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result275)
and strip_loc_row_field =
  function
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result274)
  | `Bar (_a0,_a1,_a2) ->
      let _a1 = strip_loc_row_field _a1 in
      let _a2 = strip_loc_row_field _a2 in `Bar (_a1, _a2)
  | `TyVrn (_a0,_a1) -> let _a1 = strip_loc_astring _a1 in `TyVrn _a1
  | `TyVrnOf (_a0,_a1,_a2) ->
      let _a1 = strip_loc_astring _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyVrnOf (_a1, _a2)
  | `Ctyp (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Ctyp _a1
and strip_loc_tag_names =
  function
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result273)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_tag_names _a1 in
      let _a2 = strip_loc_tag_names _a2 in `App (_a1, _a2)
  | `TyVrn (_a0,_a1) -> let _a1 = strip_loc_astring _a1 in `TyVrn _a1
and strip_loc_typedecl =
  function
  | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_opt_decl_params _a2 in
      let _a3 = strip_loc_type_info _a3 in
      let _a4 = strip_loc_opt_type_constr _a4 in `TyDcl (_a1, _a2, _a3, _a4)
  | `TyAbstr (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_opt_decl_params _a2 in
      let _a3 = strip_loc_opt_type_constr _a3 in `TyAbstr (_a1, _a2, _a3)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_typedecl _a1 in
      let _a2 = strip_loc_typedecl _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result272)
and strip_loc_type_constr =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_type_constr _a1 in
      let _a2 = strip_loc_type_constr _a2 in `And (_a1, _a2)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Eq (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result271)
and strip_loc_opt_type_constr =
  function
  | `Some (_a0,_a1) -> let _a1 = strip_loc_type_constr _a1 in `Some _a1
  | `None _a0 -> `None
and strip_loc_decl_param =
  function
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = strip_loc_position_flag _a1 in
      let _a2 = strip_loc_alident _a2 in `Quote (_a1, _a2)
  | `QuoteAny (_a0,_a1) ->
      let _a1 = strip_loc_position_flag _a1 in `QuoteAny _a1
  | `Any _a0 -> `Any
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result269)
and strip_loc_decl_params =
  function
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = strip_loc_position_flag _a1 in
      let _a2 = strip_loc_alident _a2 in `Quote (_a1, _a2)
  | `QuoteAny (_a0,_a1) ->
      let _a1 = strip_loc_position_flag _a1 in `QuoteAny _a1
  | `Any _a0 -> `Any
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_decl_params _a1 in
      let _a2 = strip_loc_decl_params _a2 in `Com (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result268)
and strip_loc_opt_decl_params =
  function
  | `Some (_a0,_a1) -> let _a1 = strip_loc_decl_params _a1 in `Some _a1
  | `None _a0 -> `None
and strip_loc_type_info =
  function
  | `TyMan (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_flag _a2 in
      let _a3 = strip_loc_type_repr _a3 in `TyMan (_a1, _a2, _a3)
  | `TyRepr (_a0,_a1,_a2) ->
      let _a1 = strip_loc_flag _a1 in
      let _a2 = strip_loc_type_repr _a2 in `TyRepr (_a1, _a2)
  | `TyEq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_flag _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyEq (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result266)
and strip_loc_type_repr =
  function
  | `Record (_a0,_a1) -> let _a1 = strip_loc_name_ctyp _a1 in `Record _a1
  | `Sum (_a0,_a1) -> let _a1 = strip_loc_or_ctyp _a1 in `Sum _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result265)
and strip_loc_name_ctyp =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_name_ctyp _a1 in
      let _a2 = strip_loc_name_ctyp _a2 in `Sem (_a1, _a2)
  | `TyCol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyCol (_a1, _a2)
  | `TyColMut (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyColMut (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result264)
and strip_loc_or_ctyp =
  function
  | `Bar (_a0,_a1,_a2) ->
      let _a1 = strip_loc_or_ctyp _a1 in
      let _a2 = strip_loc_or_ctyp _a2 in `Bar (_a1, _a2)
  | `TyCol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyCol (_a1, _a2)
  | `Of (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Of (_a1, _a2)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result263)
and strip_loc_of_ctyp =
  function
  | `Of (_a0,_a1,_a2) ->
      let _a1 = strip_loc_vid _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Of (_a1, _a2)
  | #vid' as _a0 -> (strip_loc_vid' _a0 :>'result262)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result262)
and strip_loc_pat =
  function
  | #vid as _a0 -> (strip_loc_vid _a0 :>'result261)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_pat _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_pat _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_pat _a2 in `Sem (_a1, _a2)
  | `Par (_a0,_a1) -> let _a1 = strip_loc_pat _a1 in `Par _a1
  | #any as _a0 -> (strip_loc_any _a0 :>'result261)
  | `Record (_a0,_a1) -> let _a1 = strip_loc_rec_pat _a1 in `Record _a1
  | #literal as _a0 -> (strip_loc_literal _a0 :>'result261)
  | `Alias (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_alident _a2 in `Alias (_a1, _a2)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = strip_loc_pat _a1 in `Array _a1
  | `LabelS (_a0,_a1) -> let _a1 = strip_loc_alident _a1 in `LabelS _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_pat _a2 in `Label (_a1, _a2)
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_pat _a2 in `OptLabl (_a1, _a2)
  | `OptLablS (_a0,_a1) -> let _a1 = strip_loc_alident _a1 in `OptLablS _a1
  | `OptLablExpr (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_pat _a2 in
      let _a3 = strip_loc_exp _a3 in `OptLablExpr (_a1, _a2, _a3)
  | `Bar (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_pat _a2 in `Bar (_a1, _a2)
  | `PaRng (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_pat _a2 in `PaRng (_a1, _a2)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Constraint (_a1, _a2)
  | `ClassPath (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `ClassPath _a1
  | `Lazy (_a0,_a1) -> let _a1 = strip_loc_pat _a1 in `Lazy _a1
  | `ModuleUnpack (_a0,_a1) ->
      let _a1 = strip_loc_auident _a1 in `ModuleUnpack _a1
  | `ModuleConstraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `ModuleConstraint (_a1, _a2)
and strip_loc_rec_pat =
  function
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_pat _a2 in `RecBind (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_pat _a1 in
      let _a2 = strip_loc_rec_pat _a2 in `Sem (_a1, _a2)
  | #any as _a0 -> (strip_loc_any _a0 :>'result260)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result260)
and strip_loc_exp =
  function
  | #vid as _a0 -> (strip_loc_vid _a0 :>'result259)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `Sem (_a1, _a2)
  | `Par (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `Par _a1
  | #any as _a0 -> (strip_loc_any _a0 :>'result259)
  | `Record (_a0,_a1) -> let _a1 = strip_loc_rec_exp _a1 in `Record _a1
  | #literal as _a0 -> (strip_loc_literal _a0 :>'result259)
  | `RecordWith (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `RecordWith (_a1, _a2)
  | `Field (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `Field (_a1, _a2)
  | `ArrayDot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `ArrayDot (_a1, _a2)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `Array _a1
  | `Assert (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `Assert _a1
  | `Assign (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `Assign (_a1, _a2)
  | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_exp _a2 in
      let _a3 = strip_loc_exp _a3 in
      let _a4 = strip_loc_flag _a4 in
      let _a5 = strip_loc_exp _a5 in `For (_a1, _a2, _a3, _a4, _a5)
  | `Fun (_a0,_a1) -> let _a1 = strip_loc_case _a1 in `Fun _a1
  | `IfThenElse (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in
      let _a3 = strip_loc_exp _a3 in `IfThenElse (_a1, _a2, _a3)
  | `IfThen (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `IfThen (_a1, _a2)
  | `LabelS (_a0,_a1) -> let _a1 = strip_loc_alident _a1 in `LabelS _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_exp _a2 in `Label (_a1, _a2)
  | `Lazy (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `Lazy _a1
  | `LetIn (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_flag _a1 in
      let _a2 = strip_loc_bind _a2 in
      let _a3 = strip_loc_exp _a3 in `LetIn (_a1, _a2, _a3)
  | `LetTryInWith (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_flag _a1 in
      let _a2 = strip_loc_bind _a2 in
      let _a3 = strip_loc_exp _a3 in
      let _a4 = strip_loc_case _a4 in `LetTryInWith (_a1, _a2, _a3, _a4)
  | `LetModule (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_mexp _a2 in
      let _a3 = strip_loc_exp _a3 in `LetModule (_a1, _a2, _a3)
  | `Match (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_case _a2 in `Match (_a1, _a2)
  | `New (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `New _a1
  | `Obj (_a0,_a1) -> let _a1 = strip_loc_clfield _a1 in `Obj _a1
  | `ObjEnd _a0 -> `ObjEnd
  | `ObjPat (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_clfield _a2 in `ObjPat (_a1, _a2)
  | `ObjPatEnd (_a0,_a1) -> let _a1 = strip_loc_pat _a1 in `ObjPatEnd _a1
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_exp _a2 in `OptLabl (_a1, _a2)
  | `OptLablS (_a0,_a1) -> let _a1 = strip_loc_alident _a1 in `OptLablS _a1
  | `OvrInst (_a0,_a1) -> let _a1 = strip_loc_rec_exp _a1 in `OvrInst _a1
  | `OvrInstEmpty _a0 -> `OvrInstEmpty
  | `Seq (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `Seq _a1
  | `Send (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_alident _a2 in `Send (_a1, _a2)
  | `StringDot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `StringDot (_a1, _a2)
  | `Try (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_case _a2 in `Try (_a1, _a2)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Constraint (_a1, _a2)
  | `Coercion (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_ctyp _a2 in
      let _a3 = strip_loc_ctyp _a3 in `Coercion (_a1, _a2, _a3)
  | `Subtype (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Subtype (_a1, _a2)
  | `While (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `While (_a1, _a2)
  | `LetOpen (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_exp _a2 in `LetOpen (_a1, _a2)
  | `LocalTypeFun (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_exp _a2 in `LocalTypeFun (_a1, _a2)
  | `Package_exp (_a0,_a1) ->
      let _a1 = strip_loc_mexp _a1 in `Package_exp _a1
and strip_loc_rec_exp =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_exp _a1 in
      let _a2 = strip_loc_rec_exp _a2 in `Sem (_a1, _a2)
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_exp _a2 in `RecBind (_a1, _a2)
  | #any as _a0 -> (strip_loc_any _a0 :>'result258)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result258)
and strip_loc_mtyp =
  function
  | #ident' as _a0 -> (strip_loc_ident' _a0 :>'result257)
  | `Sig (_a0,_a1) -> let _a1 = strip_loc_sigi _a1 in `Sig _a1
  | `SigEnd _a0 -> `SigEnd
  | `Functor (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_mtyp _a2 in
      let _a3 = strip_loc_mtyp _a3 in `Functor (_a1, _a2, _a3)
  | `With (_a0,_a1,_a2) ->
      let _a1 = strip_loc_mtyp _a1 in
      let _a2 = strip_loc_constr _a2 in `With (_a1, _a2)
  | `ModuleTypeOf (_a0,_a1) ->
      let _a1 = strip_loc_mexp _a1 in `ModuleTypeOf _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result257)
and strip_loc_sigi =
  function
  | `Val (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Val (_a1, _a2)
  | `External (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in
      let _a3 = strip_loc_strings _a3 in `External (_a1, _a2, _a3)
  | `Type (_a0,_a1) -> let _a1 = strip_loc_typedecl _a1 in `Type _a1
  | `Exception (_a0,_a1) -> let _a1 = strip_loc_of_ctyp _a1 in `Exception _a1
  | `Class (_a0,_a1) -> let _a1 = strip_loc_cltdecl _a1 in `Class _a1
  | `ClassType (_a0,_a1) -> let _a1 = strip_loc_cltdecl _a1 in `ClassType _a1
  | `Module (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_mtyp _a2 in `Module (_a1, _a2)
  | `ModuleTypeEnd (_a0,_a1) ->
      let _a1 = strip_loc_auident _a1 in `ModuleTypeEnd _a1
  | `ModuleType (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_mtyp _a2 in `ModuleType (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_sigi _a1 in
      let _a2 = strip_loc_sigi _a2 in `Sem (_a1, _a2)
  | `DirectiveSimple (_a0,_a1) ->
      let _a1 = strip_loc_alident _a1 in `DirectiveSimple _a1
  | `Directive (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_exp _a2 in `Directive (_a1, _a2)
  | `Open (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `Open _a1
  | `Include (_a0,_a1) -> let _a1 = strip_loc_mtyp _a1 in `Include _a1
  | `RecModule (_a0,_a1) -> let _a1 = strip_loc_mbind _a1 in `RecModule _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result256)
and strip_loc_mbind =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_mbind _a1 in
      let _a2 = strip_loc_mbind _a2 in `And (_a1, _a2)
  | `ModuleBind (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_mtyp _a2 in
      let _a3 = strip_loc_mexp _a3 in `ModuleBind (_a1, _a2, _a3)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_mtyp _a2 in `Constraint (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result255)
and strip_loc_constr =
  function
  | `TypeEq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TypeEq (_a1, _a2)
  | `ModuleEq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `ModuleEq (_a1, _a2)
  | `TypeEqPriv (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TypeEqPriv (_a1, _a2)
  | `TypeSubst (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TypeSubst (_a1, _a2)
  | `ModuleSubst (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `ModuleSubst (_a1, _a2)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_constr _a1 in
      let _a2 = strip_loc_constr _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result254)
and strip_loc_bind =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_bind _a1 in
      let _a2 = strip_loc_bind _a2 in `And (_a1, _a2)
  | `Bind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_exp _a2 in `Bind (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result253)
and strip_loc_case =
  function
  | `Bar (_a0,_a1,_a2) ->
      let _a1 = strip_loc_case _a1 in
      let _a2 = strip_loc_case _a2 in `Bar (_a1, _a2)
  | `Case (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_exp _a2 in `Case (_a1, _a2)
  | `CaseWhen (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_exp _a2 in
      let _a3 = strip_loc_exp _a3 in `CaseWhen (_a1, _a2, _a3)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result252)
and strip_loc_mexp =
  function
  | #vid' as _a0 -> (strip_loc_vid' _a0 :>'result251)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_mexp _a1 in
      let _a2 = strip_loc_mexp _a2 in `App (_a1, _a2)
  | `Functor (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_mtyp _a2 in
      let _a3 = strip_loc_mexp _a3 in `Functor (_a1, _a2, _a3)
  | `Struct (_a0,_a1) -> let _a1 = strip_loc_stru _a1 in `Struct _a1
  | `StructEnd _a0 -> `StructEnd
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_mexp _a1 in
      let _a2 = strip_loc_mtyp _a2 in `Constraint (_a1, _a2)
  | `PackageModule (_a0,_a1) ->
      let _a1 = strip_loc_exp _a1 in `PackageModule _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result251)
and strip_loc_stru =
  function
  | `Class (_a0,_a1) -> let _a1 = strip_loc_cldecl _a1 in `Class _a1
  | `ClassType (_a0,_a1) -> let _a1 = strip_loc_cltdecl _a1 in `ClassType _a1
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_stru _a1 in
      let _a2 = strip_loc_stru _a2 in `Sem (_a1, _a2)
  | `DirectiveSimple (_a0,_a1) ->
      let _a1 = strip_loc_alident _a1 in `DirectiveSimple _a1
  | `Directive (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_exp _a2 in `Directive (_a1, _a2)
  | `Exception (_a0,_a1) -> let _a1 = strip_loc_of_ctyp _a1 in `Exception _a1
  | `StExp (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `StExp _a1
  | `External (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in
      let _a3 = strip_loc_strings _a3 in `External (_a1, _a2, _a3)
  | `Include (_a0,_a1) -> let _a1 = strip_loc_mexp _a1 in `Include _a1
  | `Module (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_mexp _a2 in `Module (_a1, _a2)
  | `RecModule (_a0,_a1) -> let _a1 = strip_loc_mbind _a1 in `RecModule _a1
  | `ModuleType (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_mtyp _a2 in `ModuleType (_a1, _a2)
  | `Open (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `Open _a1
  | `Type (_a0,_a1) -> let _a1 = strip_loc_typedecl _a1 in `Type _a1
  | `Value (_a0,_a1,_a2) ->
      let _a1 = strip_loc_flag _a1 in
      let _a2 = strip_loc_bind _a2 in `Value (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result250)
and strip_loc_cltdecl =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_cltdecl _a1 in
      let _a2 = strip_loc_cltdecl _a2 in `And (_a1, _a2)
  | `CtDecl (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_flag _a1 in
      let _a2 = strip_loc_ident _a2 in
      let _a3 = strip_loc_type_parameters _a3 in
      let _a4 = strip_loc_cltyp _a4 in `CtDecl (_a1, _a2, _a3, _a4)
  | `CtDeclS (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_flag _a1 in
      let _a2 = strip_loc_ident _a2 in
      let _a3 = strip_loc_cltyp _a3 in `CtDeclS (_a1, _a2, _a3)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result249)
and strip_loc_cltyp =
  function
  | #vid' as _a0 -> (strip_loc_vid' _a0 :>'result248)
  | `ClApply (_a0,_a1,_a2) ->
      let _a1 = strip_loc_vid _a1 in
      let _a2 = strip_loc_type_parameters _a2 in `ClApply (_a1, _a2)
  | `CtFun (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_cltyp _a2 in `CtFun (_a1, _a2)
  | `ObjTy (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_clsigi _a2 in `ObjTy (_a1, _a2)
  | `ObjTyEnd (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `ObjTyEnd _a1
  | `Obj (_a0,_a1) -> let _a1 = strip_loc_clsigi _a1 in `Obj _a1
  | `ObjEnd _a0 -> `ObjEnd
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_cltyp _a1 in
      let _a2 = strip_loc_cltyp _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result248)
and strip_loc_clsigi =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_clsigi _a1 in
      let _a2 = strip_loc_clsigi _a2 in `Sem (_a1, _a2)
  | `SigInherit (_a0,_a1) -> let _a1 = strip_loc_cltyp _a1 in `SigInherit _a1
  | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_flag _a2 in
      let _a3 = strip_loc_flag _a3 in
      let _a4 = strip_loc_ctyp _a4 in `CgVal (_a1, _a2, _a3, _a4)
  | `Method (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `Method (_a1, _a2, _a3)
  | `VirMeth (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `VirMeth (_a1, _a2, _a3)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Eq (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result247)
and strip_loc_cldecl =
  function
  | `ClDecl (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_flag _a1 in
      let _a2 = strip_loc_ident _a2 in
      let _a3 = strip_loc_type_parameters _a3 in
      let _a4 = strip_loc_clexp _a4 in `ClDecl (_a1, _a2, _a3, _a4)
  | `ClDeclS (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_flag _a1 in
      let _a2 = strip_loc_ident _a2 in
      let _a3 = strip_loc_clexp _a3 in `ClDeclS (_a1, _a2, _a3)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_cldecl _a1 in
      let _a2 = strip_loc_cldecl _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result246)
and strip_loc_clexp =
  function
  | `CeApp (_a0,_a1,_a2) ->
      let _a1 = strip_loc_clexp _a1 in
      let _a2 = strip_loc_exp _a2 in `CeApp (_a1, _a2)
  | #vid' as _a0 -> (strip_loc_vid' _a0 :>'result245)
  | `ClApply (_a0,_a1,_a2) ->
      let _a1 = strip_loc_vid _a1 in
      let _a2 = strip_loc_type_parameters _a2 in `ClApply (_a1, _a2)
  | `CeFun (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_clexp _a2 in `CeFun (_a1, _a2)
  | `LetIn (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_flag _a1 in
      let _a2 = strip_loc_bind _a2 in
      let _a3 = strip_loc_clexp _a3 in `LetIn (_a1, _a2, _a3)
  | `Obj (_a0,_a1) -> let _a1 = strip_loc_clfield _a1 in `Obj _a1
  | `ObjEnd _a0 -> `ObjEnd
  | `ObjPat (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_clfield _a2 in `ObjPat (_a1, _a2)
  | `ObjPatEnd (_a0,_a1) -> let _a1 = strip_loc_pat _a1 in `ObjPatEnd _a1
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_clexp _a1 in
      let _a2 = strip_loc_cltyp _a2 in `Constraint (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result245)
and strip_loc_clfield =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_clfield _a1 in
      let _a2 = strip_loc_clfield _a2 in `Sem (_a1, _a2)
  | `Inherit (_a0,_a1,_a2) ->
      let _a1 = strip_loc_flag _a1 in
      let _a2 = strip_loc_clexp _a2 in `Inherit (_a1, _a2)
  | `InheritAs (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_flag _a1 in
      let _a2 = strip_loc_clexp _a2 in
      let _a3 = strip_loc_alident _a3 in `InheritAs (_a1, _a2, _a3)
  | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_flag _a2 in
      let _a3 = strip_loc_flag _a3 in
      let _a4 = strip_loc_exp _a4 in `CrVal (_a1, _a2, _a3, _a4)
  | `VirVal (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `VirVal (_a1, _a2, _a3)
  | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_flag _a2 in
      let _a3 = strip_loc_flag _a3 in
      let _a4 = strip_loc_exp _a4 in
      let _a5 = strip_loc_ctyp _a5 in `CrMth (_a1, _a2, _a3, _a4, _a5)
  | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_flag _a2 in
      let _a3 = strip_loc_flag _a3 in
      let _a4 = strip_loc_exp _a4 in `CrMthS (_a1, _a2, _a3, _a4)
  | `VirMeth (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `VirMeth (_a1, _a2, _a3)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Eq (_a1, _a2)
  | `Initializer (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `Initializer _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result244)

let rec strip_loc_ep =
  function
  | #vid as _a0 -> (strip_loc_vid _a0 :>'result278)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ep _a1 in
      let _a2 = strip_loc_ep _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ep _a1 in
      let _a2 = strip_loc_ep _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ep _a1 in
      let _a2 = strip_loc_ep _a2 in `Sem (_a1, _a2)
  | `Par (_a0,_a1) -> let _a1 = strip_loc_ep _a1 in `Par _a1
  | #any as _a0 -> (strip_loc_any _a0 :>'result278)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = strip_loc_ep _a1 in `Array _a1
  | `Record (_a0,_a1) -> let _a1 = strip_loc_rec_bind _a1 in `Record _a1
  | #literal as _a0 -> (strip_loc_literal _a0 :>'result278)
and strip_loc_rec_bind =
  function
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ep _a2 in `RecBind (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_bind _a1 in
      let _a2 = strip_loc_rec_bind _a2 in `Sem (_a1, _a2)
  | #any as _a0 -> (strip_loc_any _a0 :>'result277)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result277)

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

class reloc _loc = object  inherit  map method! loc _ = _loc end

let wildcarder =
  object (self)
    inherit  map as super
    method! pat =
      function
      | (`Lid (_loc,_) : Ast.pat) -> (`Any _loc : Ast.pat )
      | (`Alias (_loc,p,_) : Ast.pat) -> self#pat p
      | p -> super#pat p
  end