open Astfn
module type S = sig val pp_print_loc : Locf.t Formatf.t end
module Make(U:S) =
  struct
    open U
    let pp_print_ant: Format.formatter -> ant -> unit =
      fun fmt  (`Ant (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" pp_print_loc _a0
          Tokenf.pp_print_ant _a1
    let pp_print_literal: Format.formatter -> literal -> unit =
      fun fmt  ->
        function
        | `Chr _a0 ->
            Format.fprintf fmt "@[<1>(`Chr@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | `Int _a0 ->
            Format.fprintf fmt "@[<1>(`Int@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | `Int32 _a0 ->
            Format.fprintf fmt "@[<1>(`Int32@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | `Int64 _a0 ->
            Format.fprintf fmt "@[<1>(`Int64@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | `Flo _a0 ->
            Format.fprintf fmt "@[<1>(`Flo@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | `Nativeint _a0 ->
            Format.fprintf fmt "@[<1>(`Nativeint@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | `Str _a0 ->
            Format.fprintf fmt "@[<1>(`Str@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | `Bool _a0 ->
            Format.fprintf fmt "@[<1>(`Bool@ %a)@]" Format.pp_print_bool _a0
        | `Unit -> Format.fprintf fmt "`Unit"
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
        | `Str _a0 ->
            Format.fprintf fmt "@[<1>(`Str@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    let pp_print_lident: Format.formatter -> lident -> unit =
      fun fmt  (`Lid _a0)  ->
        Format.fprintf fmt "@[<1>(`Lid@ %a)@]"
          (fun fmt  -> Format.fprintf fmt "%S") _a0
    let pp_print_alident: Format.formatter -> alident -> unit =
      fun fmt  ->
        function
        | `Lid _a0 ->
            Format.fprintf fmt "@[<1>(`Lid@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    let pp_print_auident: Format.formatter -> auident -> unit =
      fun fmt  ->
        function
        | `Uid _a0 ->
            Format.fprintf fmt "@[<1>(`Uid@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    let pp_print_aident: Format.formatter -> aident -> unit =
      fun fmt  ->
        function
        | #alident as _a0 -> (pp_print_alident fmt _a0 :>unit)
        | #auident as _a0 -> (pp_print_auident fmt _a0 :>unit)
    let pp_print_astring: Format.formatter -> astring -> unit =
      fun fmt  ->
        function
        | `C _a0 ->
            Format.fprintf fmt "@[<1>(`C@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
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
        | `Lid _a0 ->
            Format.fprintf fmt "@[<1>(`Lid@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | `Uid _a0 ->
            Format.fprintf fmt "@[<1>(`Uid@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
    let rec pp_print_vid: Format.formatter -> vid -> unit =
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" pp_print_vid _a0
              pp_print_vid _a1
        | `Lid _a0 ->
            Format.fprintf fmt "@[<1>(`Lid@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | `Uid _a0 ->
            Format.fprintf fmt "@[<1>(`Uid@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    let pp_print_vid': Format.formatter -> vid' -> unit =
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" pp_print_vid _a0
              pp_print_vid _a1
        | `Lid _a0 ->
            Format.fprintf fmt "@[<1>(`Lid@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | `Uid _a0 ->
            Format.fprintf fmt "@[<1>(`Uid@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
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
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" pp_print_alident
              _a0 pp_print_ctyp _a1
        | #ident' as _a0 -> (pp_print_ident' fmt _a0 :>unit)
        | `TyObj (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyObj@ %a@ %a)@]" pp_print_name_ctyp
              _a0 pp_print_flag _a1
        | `TyObjEnd _a0 ->
            Format.fprintf fmt "@[<1>(`TyObjEnd@ %a)@]" pp_print_flag _a0
        | `TyPol (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyPol@ %a@ %a)@]" pp_print_ctyp _a0
              pp_print_ctyp _a1
        | `TyPolEnd _a0 ->
            Format.fprintf fmt "@[<1>(`TyPolEnd@ %a)@]" pp_print_ctyp _a0
        | `TyTypePol (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyTypePol@ %a@ %a)@]" pp_print_ctyp
              _a0 pp_print_ctyp _a1
        | `Quote (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]"
              pp_print_position_flag _a0 pp_print_alident _a1
        | `QuoteAny _a0 ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]"
              pp_print_position_flag _a0
        | `Par _a0 ->
            Format.fprintf fmt "@[<1>(`Par@ %a)@]" pp_print_ctyp _a0
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
            Format.fprintf fmt "@[<1>(`PolyInfSup@ %a@ %a)@]"
              pp_print_row_field _a0 pp_print_tag_names _a1
        | `Package _a0 ->
            Format.fprintf fmt "@[<1>(`Package@ %a)@]" pp_print_mtyp _a0
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    and pp_print_type_parameters: Format.formatter -> type_parameters -> unit
      =
      fun fmt  ->
        function
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]"
              pp_print_type_parameters _a0 pp_print_type_parameters _a1
        | `Ctyp _a0 ->
            Format.fprintf fmt "@[<1>(`Ctyp@ %a)@]" pp_print_ctyp _a0
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
            Format.fprintf fmt "@[<1>(`TyVrnOf@ %a@ %a)@]" pp_print_astring
              _a0 pp_print_ctyp _a1
        | `Ctyp _a0 ->
            Format.fprintf fmt "@[<1>(`Ctyp@ %a)@]" pp_print_ctyp _a0
    and pp_print_tag_names: Format.formatter -> tag_names -> unit =
      fun fmt  ->
        function
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" pp_print_tag_names _a0
              pp_print_tag_names _a1
        | `TyVrn _a0 ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a)@]" pp_print_astring _a0
    and pp_print_decl: Format.formatter -> decl -> unit =
      fun fmt  ->
        function
        | `TyDcl (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`TyDcl@ %a@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_opt_decl_params _a1
              pp_print_type_info _a2 pp_print_opt_type_constr _a3
        | `TyAbstr (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyAbstr@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_opt_decl_params _a1
              pp_print_opt_type_constr _a2
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" pp_print_decl _a0
              pp_print_decl _a1
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    and pp_print_type_constr: Format.formatter -> type_constr -> unit =
      fun fmt  ->
        function
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" pp_print_type_constr
              _a0 pp_print_type_constr _a1
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" pp_print_ctyp _a0
              pp_print_ctyp _a1
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    and pp_print_opt_type_constr: Format.formatter -> opt_type_constr -> unit
      =
      fun fmt  ->
        function
        | `Some _a0 ->
            Format.fprintf fmt "@[<1>(`Some@ %a)@]" pp_print_type_constr _a0
        | `None -> Format.fprintf fmt "`None"
    and pp_print_decl_param: Format.formatter -> decl_param -> unit =
      fun fmt  ->
        function
        | `Quote (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]"
              pp_print_position_flag _a0 pp_print_alident _a1
        | `QuoteAny _a0 ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]"
              pp_print_position_flag _a0
        | `Any -> Format.fprintf fmt "`Any"
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    and pp_print_decl_params: Format.formatter -> decl_params -> unit =
      fun fmt  ->
        function
        | `Quote (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]"
              pp_print_position_flag _a0 pp_print_alident _a1
        | `QuoteAny _a0 ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]"
              pp_print_position_flag _a0
        | `Any -> Format.fprintf fmt "`Any"
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" pp_print_decl_params
              _a0 pp_print_decl_params _a1
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    and pp_print_opt_decl_params: Format.formatter -> opt_decl_params -> unit
      =
      fun fmt  ->
        function
        | `Some _a0 ->
            Format.fprintf fmt "@[<1>(`Some@ %a)@]" pp_print_decl_params _a0
        | `None -> Format.fprintf fmt "`None"
    and pp_print_type_info: Format.formatter -> type_info -> unit =
      fun fmt  ->
        function
        | `TyMan (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyMan@ %a@ %a@ %a)@]" pp_print_ctyp
              _a0 pp_print_flag _a1 pp_print_type_repr _a2
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
        | `Sum _a0 ->
            Format.fprintf fmt "@[<1>(`Sum@ %a)@]" pp_print_or_ctyp _a0
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    and pp_print_name_ctyp: Format.formatter -> name_ctyp -> unit =
      fun fmt  ->
        function
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_name_ctyp _a0
              pp_print_name_ctyp _a1
        | `RecCol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecCol@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_ctyp _a1 pp_print_flag _a2
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
        | `Vrn _a0 ->
            Format.fprintf fmt "@[<1>(`Vrn@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
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
        | `Array _a0 ->
            Format.fprintf fmt "@[<1>(`Array@ %a)@]" pp_print_pat _a0
        | `LabelS _a0 ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a)@]" pp_print_alident _a0
        | `Label (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" pp_print_alident _a0
              pp_print_pat _a1
        | `OptLabl (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" pp_print_alident
              _a0 pp_print_pat _a1
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
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" pp_print_pat
              _a0 pp_print_ctyp _a1
        | `ClassPath _a0 ->
            Format.fprintf fmt "@[<1>(`ClassPath@ %a)@]" pp_print_ident _a0
        | `Lazy _a0 ->
            Format.fprintf fmt "@[<1>(`Lazy@ %a)@]" pp_print_pat _a0
        | `ModuleUnpack _a0 ->
            Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a)@]" pp_print_auident
              _a0
        | `ModuleConstraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a)@]"
              pp_print_auident _a0 pp_print_ctyp _a1
    and pp_print_rec_pat: Format.formatter -> rec_pat -> unit =
      fun fmt  ->
        function
        | `RecBind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" pp_print_vid _a0
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
        | `Vrn _a0 ->
            Format.fprintf fmt "@[<1>(`Vrn@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
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
            Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a)@]"
              pp_print_rec_exp _a0 pp_print_exp _a1
        | `Field (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Field@ %a@ %a)@]" pp_print_exp _a0
              pp_print_vid _a1
        | `ArrayDot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ArrayDot@ %a@ %a)@]" pp_print_exp _a0
              pp_print_exp _a1
        | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
        | `Array _a0 ->
            Format.fprintf fmt "@[<1>(`Array@ %a)@]" pp_print_exp _a0
        | `Assert _a0 ->
            Format.fprintf fmt "@[<1>(`Assert@ %a)@]" pp_print_exp _a0
        | `Assign (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Assign@ %a@ %a)@]" pp_print_exp _a0
              pp_print_exp _a1
        | `For (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_exp _a1 pp_print_exp _a2
              pp_print_flag _a3 pp_print_exp _a4
        | `Fun _a0 ->
            Format.fprintf fmt "@[<1>(`Fun@ %a)@]" pp_print_case _a0
        | `IfThenElse (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a)@]"
              pp_print_exp _a0 pp_print_exp _a1 pp_print_exp _a2
        | `IfThen (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`IfThen@ %a@ %a)@]" pp_print_exp _a0
              pp_print_exp _a1
        | `LabelS _a0 ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a)@]" pp_print_alident _a0
        | `Label (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" pp_print_alident _a0
              pp_print_exp _a1
        | `Lazy _a0 ->
            Format.fprintf fmt "@[<1>(`Lazy@ %a)@]" pp_print_exp _a0
        | `LetIn (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a)@]" pp_print_flag
              _a0 pp_print_bind _a1 pp_print_exp _a2
        | `LetTryInWith (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`LetTryInWith@ %a@ %a@ %a@ %a)@]"
              pp_print_flag _a0 pp_print_bind _a1 pp_print_exp _a2
              pp_print_case _a3
        | `LetModule (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a)@]"
              pp_print_auident _a0 pp_print_mexp _a1 pp_print_exp _a2
        | `Match (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Match@ %a@ %a)@]" pp_print_exp _a0
              pp_print_case _a1
        | `New _a0 ->
            Format.fprintf fmt "@[<1>(`New@ %a)@]" pp_print_ident _a0
        | `Obj _a0 ->
            Format.fprintf fmt "@[<1>(`Obj@ %a)@]" pp_print_clfield _a0
        | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
        | `ObjPat (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a)@]" pp_print_pat _a0
              pp_print_clfield _a1
        | `ObjPatEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a)@]" pp_print_pat _a0
        | `OptLabl (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" pp_print_alident
              _a0 pp_print_exp _a1
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
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" pp_print_exp
              _a0 pp_print_ctyp _a1
        | `Coercion (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Coercion@ %a@ %a@ %a)@]" pp_print_exp
              _a0 pp_print_ctyp _a1 pp_print_ctyp _a2
        | `Subtype (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Subtype@ %a@ %a)@]" pp_print_exp _a0
              pp_print_ctyp _a1
        | `While (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`While@ %a@ %a)@]" pp_print_exp _a0
              pp_print_exp _a1
        | `LetOpen (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a@ %a)@]" pp_print_flag
              _a0 pp_print_ident _a1 pp_print_exp _a2
        | `LocalTypeFun (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_exp _a1
        | `Package_exp _a0 ->
            Format.fprintf fmt "@[<1>(`Package_exp@ %a)@]" pp_print_mexp _a0
    and pp_print_rec_exp: Format.formatter -> rec_exp -> unit =
      fun fmt  ->
        function
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_rec_exp _a0
              pp_print_rec_exp _a1
        | `RecBind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" pp_print_vid _a0
              pp_print_exp _a1
        | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    and pp_print_mtyp: Format.formatter -> mtyp -> unit =
      fun fmt  ->
        function
        | #ident' as _a0 -> (pp_print_ident' fmt _a0 :>unit)
        | `Sig _a0 ->
            Format.fprintf fmt "@[<1>(`Sig@ %a)@]" pp_print_sigi _a0
        | `SigEnd -> Format.fprintf fmt "`SigEnd"
        | `Functor (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a)@]"
              pp_print_auident _a0 pp_print_mtyp _a1 pp_print_mtyp _a2
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
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_ctyp _a1 pp_print_strings _a2
        | `Type _a0 ->
            Format.fprintf fmt "@[<1>(`Type@ %a)@]" pp_print_decl _a0
        | `Exception _a0 ->
            Format.fprintf fmt "@[<1>(`Exception@ %a)@]" pp_print_of_ctyp _a0
        | `Class _a0 ->
            Format.fprintf fmt "@[<1>(`Class@ %a)@]" pp_print_cltdecl _a0
        | `ClassType _a0 ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a)@]" pp_print_cltdecl _a0
        | `Module (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a)@]" pp_print_auident
              _a0 pp_print_mtyp _a1
        | `ModuleTypeEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ModuleTypeEnd@ %a)@]" pp_print_auident
              _a0
        | `ModuleType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a)@]"
              pp_print_auident _a0 pp_print_mtyp _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_sigi _a0
              pp_print_sigi _a1
        | `DirectiveSimple _a0 ->
            Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a)@]"
              pp_print_alident _a0
        | `Directive (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a)@]" pp_print_alident
              _a0 pp_print_exp _a1
        | `Open (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" pp_print_flag _a0
              pp_print_ident _a1
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
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]"
              pp_print_auident _a0 pp_print_mtyp _a1
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    and pp_print_constr: Format.formatter -> constr -> unit =
      fun fmt  ->
        function
        | `TypeEq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TypeEq@ %a@ %a)@]" pp_print_ctyp _a0
              pp_print_ctyp _a1
        | `ModuleEq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleEq@ %a@ %a)@]" pp_print_ident
              _a0 pp_print_ident _a1
        | `TypeEqPriv (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TypeEqPriv@ %a@ %a)@]" pp_print_ctyp
              _a0 pp_print_ctyp _a1
        | `TypeSubst (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TypeSubst@ %a@ %a)@]" pp_print_ctyp
              _a0 pp_print_ctyp _a1
        | `ModuleSubst (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleSubst@ %a@ %a)@]" pp_print_ident
              _a0 pp_print_ident _a1
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
            Format.fprintf fmt "@[<1>(`CaseWhen@ %a@ %a@ %a)@]" pp_print_pat
              _a0 pp_print_exp _a1 pp_print_exp _a2
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    and pp_print_mexp: Format.formatter -> mexp -> unit =
      fun fmt  ->
        function
        | #vid' as _a0 -> (pp_print_vid' fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" pp_print_mexp _a0
              pp_print_mexp _a1
        | `Functor (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a)@]"
              pp_print_auident _a0 pp_print_mtyp _a1 pp_print_mexp _a2
        | `Struct _a0 ->
            Format.fprintf fmt "@[<1>(`Struct@ %a)@]" pp_print_stru _a0
        | `StructEnd -> Format.fprintf fmt "`StructEnd"
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" pp_print_mexp
              _a0 pp_print_mtyp _a1
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
            Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a)@]"
              pp_print_alident _a0
        | `Directive (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a)@]" pp_print_alident
              _a0 pp_print_exp _a1
        | `Exception _a0 ->
            Format.fprintf fmt "@[<1>(`Exception@ %a)@]" pp_print_of_ctyp _a0
        | `StExp _a0 ->
            Format.fprintf fmt "@[<1>(`StExp@ %a)@]" pp_print_exp _a0
        | `External (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_ctyp _a1 pp_print_strings _a2
        | `Include _a0 ->
            Format.fprintf fmt "@[<1>(`Include@ %a)@]" pp_print_mexp _a0
        | `Module (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a)@]" pp_print_auident
              _a0 pp_print_mexp _a1
        | `RecModule _a0 ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a)@]" pp_print_mbind _a0
        | `ModuleType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a)@]"
              pp_print_auident _a0 pp_print_mtyp _a1
        | `Open (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" pp_print_flag _a0
              pp_print_ident _a1
        | `Type _a0 ->
            Format.fprintf fmt "@[<1>(`Type@ %a)@]" pp_print_decl _a0
        | `TypeWith (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TypeWith@ %a@ %a)@]" pp_print_decl _a0
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
            Format.fprintf fmt "@[<1>(`CtDecl@ %a@ %a@ %a@ %a)@]"
              pp_print_flag _a0 pp_print_ident _a1 pp_print_type_parameters
              _a2 pp_print_cltyp _a3
        | `CtDeclS (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CtDeclS@ %a@ %a@ %a)@]" pp_print_flag
              _a0 pp_print_ident _a1 pp_print_cltyp _a2
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
        | `Obj _a0 ->
            Format.fprintf fmt "@[<1>(`Obj@ %a)@]" pp_print_clsigi _a0
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
            Format.fprintf fmt "@[<1>(`CgVal@ %a@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_flag _a1 pp_print_flag _a2
              pp_print_ctyp _a3
        | `Method (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Method@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_flag _a1 pp_print_ctyp _a2
        | `VirMeth (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`VirMeth@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_flag _a1 pp_print_ctyp _a2
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" pp_print_ctyp _a0
              pp_print_ctyp _a1
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    and pp_print_cldecl: Format.formatter -> cldecl -> unit =
      fun fmt  ->
        function
        | `ClDecl (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`ClDecl@ %a@ %a@ %a@ %a)@]"
              pp_print_flag _a0 pp_print_ident _a1 pp_print_type_parameters
              _a2 pp_print_clexp _a3
        | `ClDeclS (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ClDeclS@ %a@ %a@ %a)@]" pp_print_flag
              _a0 pp_print_ident _a1 pp_print_clexp _a2
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
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a)@]" pp_print_flag
              _a0 pp_print_bind _a1 pp_print_clexp _a2
        | `Obj _a0 ->
            Format.fprintf fmt "@[<1>(`Obj@ %a)@]" pp_print_clfield _a0
        | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
        | `ObjPat (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a)@]" pp_print_pat _a0
              pp_print_clfield _a1
        | `ObjPatEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a)@]" pp_print_pat _a0
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" pp_print_clexp
              _a0 pp_print_cltyp _a1
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
            Format.fprintf fmt "@[<1>(`InheritAs@ %a@ %a@ %a)@]"
              pp_print_flag _a0 pp_print_clexp _a1 pp_print_alident _a2
        | `CrVal (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_flag _a1 pp_print_flag _a2
              pp_print_exp _a3
        | `VirVal (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`VirVal@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_flag _a1 pp_print_ctyp _a2
        | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_flag _a1 pp_print_flag _a2
              pp_print_exp _a3 pp_print_ctyp _a4
        | `CrMthS (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CrMthS@ %a@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_flag _a1 pp_print_flag _a2
              pp_print_exp _a3
        | `VirMeth (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`VirMeth@ %a@ %a@ %a)@]"
              pp_print_alident _a0 pp_print_flag _a1 pp_print_ctyp _a2
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
        | `Vrn _a0 ->
            Format.fprintf fmt "@[<1>(`Vrn@ %a)@]"
              (fun fmt  -> Format.fprintf fmt "%S") _a0
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" pp_print_ep _a0
              pp_print_ep _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_ep _a0
              pp_print_ep _a1
        | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" pp_print_ep _a0
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" pp_print_ep _a0
              pp_print_ctyp _a1
        | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
        | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
        | `Array _a0 ->
            Format.fprintf fmt "@[<1>(`Array@ %a)@]" pp_print_ep _a0
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" pp_print_rec_bind _a0
        | #literal as _a0 -> (pp_print_literal fmt _a0 :>unit)
    and pp_print_rec_bind: Format.formatter -> rec_bind -> unit =
      fun fmt  ->
        function
        | `RecBind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" pp_print_vid _a0
              pp_print_ep _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" pp_print_rec_bind _a0
              pp_print_rec_bind _a1
        | #any as _a0 -> (pp_print_any fmt _a0 :>unit)
        | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    let dump_literal = Formatf.to_string pp_print_literal
    let dump_flag = Formatf.to_string pp_print_flag
    let dump_position_flag = Formatf.to_string pp_print_position_flag
    let dump_strings = Formatf.to_string pp_print_strings
    let dump_lident = Formatf.to_string pp_print_lident
    let dump_alident = Formatf.to_string pp_print_alident
    let dump_auident = Formatf.to_string pp_print_auident
    let dump_aident = Formatf.to_string pp_print_aident
    let dump_astring = Formatf.to_string pp_print_astring
    let dump_uident = Formatf.to_string pp_print_uident
    let dump_ident = Formatf.to_string pp_print_ident
    let dump_ident' = Formatf.to_string pp_print_ident'
    let dump_vid = Formatf.to_string pp_print_vid
    let dump_vid' = Formatf.to_string pp_print_vid'
    let dump_dupath = Formatf.to_string pp_print_dupath
    let dump_dlpath = Formatf.to_string pp_print_dlpath
    let dump_any = Formatf.to_string pp_print_any
    let dump_ctyp = Formatf.to_string pp_print_ctyp
    let dump_type_parameters = Formatf.to_string pp_print_type_parameters
    let dump_row_field = Formatf.to_string pp_print_row_field
    let dump_tag_names = Formatf.to_string pp_print_tag_names
    let dump_decl = Formatf.to_string pp_print_decl
    let dump_type_constr = Formatf.to_string pp_print_type_constr
    let dump_opt_type_constr = Formatf.to_string pp_print_opt_type_constr
    let dump_decl_param = Formatf.to_string pp_print_decl_param
    let dump_decl_params = Formatf.to_string pp_print_decl_params
    let dump_opt_decl_params = Formatf.to_string pp_print_opt_decl_params
    let dump_type_info = Formatf.to_string pp_print_type_info
    let dump_type_repr = Formatf.to_string pp_print_type_repr
    let dump_name_ctyp = Formatf.to_string pp_print_name_ctyp
    let dump_or_ctyp = Formatf.to_string pp_print_or_ctyp
    let dump_of_ctyp = Formatf.to_string pp_print_of_ctyp
    let dump_pat = Formatf.to_string pp_print_pat
    let dump_rec_pat = Formatf.to_string pp_print_rec_pat
    let dump_exp = Formatf.to_string pp_print_exp
    let dump_rec_exp = Formatf.to_string pp_print_rec_exp
    let dump_mtyp = Formatf.to_string pp_print_mtyp
    let dump_sigi = Formatf.to_string pp_print_sigi
    let dump_mbind = Formatf.to_string pp_print_mbind
    let dump_constr = Formatf.to_string pp_print_constr
    let dump_bind = Formatf.to_string pp_print_bind
    let dump_case = Formatf.to_string pp_print_case
    let dump_mexp = Formatf.to_string pp_print_mexp
    let dump_stru = Formatf.to_string pp_print_stru
    let dump_cltdecl = Formatf.to_string pp_print_cltdecl
    let dump_cltyp = Formatf.to_string pp_print_cltyp
    let dump_clsigi = Formatf.to_string pp_print_clsigi
    let dump_cldecl = Formatf.to_string pp_print_cldecl
    let dump_clexp = Formatf.to_string pp_print_clexp
    let dump_clfield = Formatf.to_string pp_print_clfield
    let dump_ep = Formatf.to_string pp_print_ep
    let dump_rec_bind = Formatf.to_string pp_print_rec_bind
  end
include Make(struct let pp_print_loc _ _ = () end)
