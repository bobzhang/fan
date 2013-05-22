open AstN

let fill_loc_ant _loc x = x

let _ = begin (); () end

class primitive =
  object 
    method int _loc (i : int) = (`Int (_loc, (string_of_int i)) : Ast.ep )
    method int32 _loc (i : int32) =
      (`Int32 (_loc, (Int32.to_string i)) : Ast.ep )
    method int64 _loc (i : int64) =
      (`Int64 (_loc, (Int64.to_string i)) : Ast.ep )
    method nativeint _loc (i : nativeint) =
      (`Nativeint (_loc, (Nativeint.to_string i)) : Ast.ep )
    method float _loc (i : float) =
      (`Flo (_loc, (FanUtil.float_repres i)) : Ast.ep )
    method string _loc (i : string) =
      (`Str (_loc, (String.escaped i)) : Ast.ep )
    method char _loc (i : char) = (`Chr (_loc, (Char.escaped i)) : Ast.ep )
    method unit _loc (_ : unit) = (`Uid (_loc, "()") : Ast.ep )
    method ant (_loc : loc) (x : ant) = (x :>Ast.ep)
    method bool _loc x =
      match x with
      | true  -> (`Lid (_loc, "true") : Ast.ep )
      | false  -> (`Lid (_loc, "false") : Ast.ep )
  end

let fill_loc_nil: FanLoc.t -> AstN.nil -> Ast.nil =
  fun loc  `Nil  -> `Nil loc

let fill_loc_literal: FanLoc.t -> AstN.literal -> Ast.literal =
  fun loc  ->
    function
    | `Chr _a0 -> `Chr (loc, _a0)
    | `Int _a0 -> `Int (loc, _a0)
    | `Int32 _a0 -> `Int32 (loc, _a0)
    | `Int64 _a0 -> `Int64 (loc, _a0)
    | `Flo _a0 -> `Flo (loc, _a0)
    | `Nativeint _a0 -> `Nativeint (loc, _a0)
    | `Str _a0 -> `Str (loc, _a0)

let fill_loc_flag: FanLoc.t -> AstN.flag -> Ast.flag =
  fun loc  ->
    function
    | `Positive -> `Positive loc
    | `Negative -> `Negative loc
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.flag)

let fill_loc_position_flag:
  FanLoc.t -> AstN.position_flag -> Ast.position_flag =
  fun loc  ->
    function
    | `Positive -> `Positive loc
    | `Negative -> `Negative loc
    | `Normal -> `Normal loc
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.position_flag)

let rec fill_loc_strings: FanLoc.t -> AstN.strings -> Ast.strings =
  fun loc  ->
    function
    | `App (_a0,_a1) ->
        let _a0 = fill_loc_strings loc _a0 in
        let _a1 = fill_loc_strings loc _a1 in `App (loc, _a0, _a1)
    | `Str _a0 -> `Str (loc, _a0)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.strings)

let fill_loc_lident: FanLoc.t -> AstN.lident -> Ast.lident =
  fun loc  (`Lid _a0)  -> `Lid (loc, _a0)

let fill_loc_alident: FanLoc.t -> AstN.alident -> Ast.alident =
  fun loc  ->
    function
    | `Lid _a0 -> `Lid (loc, _a0)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.alident)

let fill_loc_auident: FanLoc.t -> AstN.auident -> Ast.auident =
  fun loc  ->
    function
    | `Uid _a0 -> `Uid (loc, _a0)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.auident)

let fill_loc_aident: FanLoc.t -> AstN.aident -> Ast.aident =
  fun loc  ->
    function
    | #alident as _a0 -> (fill_loc_alident loc _a0 :>Ast.aident)
    | #auident as _a0 -> (fill_loc_auident loc _a0 :>Ast.aident)

let fill_loc_astring: FanLoc.t -> AstN.astring -> Ast.astring =
  fun loc  ->
    function
    | `C _a0 -> `C (loc, _a0)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.astring)

let rec fill_loc_uident: FanLoc.t -> AstN.uident -> Ast.uident =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = fill_loc_uident loc _a0 in
        let _a1 = fill_loc_uident loc _a1 in `Dot (loc, _a0, _a1)
    | `App (_a0,_a1) ->
        let _a0 = fill_loc_uident loc _a0 in
        let _a1 = fill_loc_uident loc _a1 in `App (loc, _a0, _a1)
    | #auident as _a0 -> (fill_loc_auident loc _a0 :>Ast.uident)

let rec fill_loc_ident: FanLoc.t -> AstN.ident -> Ast.ident =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = fill_loc_ident loc _a0 in
        let _a1 = fill_loc_ident loc _a1 in `Dot (loc, _a0, _a1)
    | `Apply (_a0,_a1) ->
        let _a0 = fill_loc_ident loc _a0 in
        let _a1 = fill_loc_ident loc _a1 in `Apply (loc, _a0, _a1)
    | #alident as _a0 -> (fill_loc_alident loc _a0 :>Ast.ident)
    | #auident as _a0 -> (fill_loc_auident loc _a0 :>Ast.ident)

let fill_loc_ident': FanLoc.t -> AstN.ident' -> Ast.ident' =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = fill_loc_ident loc _a0 in
        let _a1 = fill_loc_ident loc _a1 in `Dot (loc, _a0, _a1)
    | `Apply (_a0,_a1) ->
        let _a0 = fill_loc_ident loc _a0 in
        let _a1 = fill_loc_ident loc _a1 in `Apply (loc, _a0, _a1)
    | `Lid _a0 -> `Lid (loc, _a0)
    | `Uid _a0 -> `Uid (loc, _a0)

let rec fill_loc_vid: FanLoc.t -> AstN.vid -> Ast.vid =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = fill_loc_vid loc _a0 in
        let _a1 = fill_loc_vid loc _a1 in `Dot (loc, _a0, _a1)
    | `Lid _a0 -> `Lid (loc, _a0)
    | `Uid _a0 -> `Uid (loc, _a0)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.vid)

let fill_loc_vid': FanLoc.t -> AstN.vid' -> Ast.vid' =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = fill_loc_vid loc _a0 in
        let _a1 = fill_loc_vid loc _a1 in `Dot (loc, _a0, _a1)
    | `Lid _a0 -> `Lid (loc, _a0)
    | `Uid _a0 -> `Uid (loc, _a0)

let rec fill_loc_dupath: FanLoc.t -> AstN.dupath -> Ast.dupath =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = fill_loc_dupath loc _a0 in
        let _a1 = fill_loc_dupath loc _a1 in `Dot (loc, _a0, _a1)
    | #auident as _a0 -> (fill_loc_auident loc _a0 :>Ast.dupath)

let fill_loc_dlpath: FanLoc.t -> AstN.dlpath -> Ast.dlpath =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = fill_loc_dupath loc _a0 in
        let _a1 = fill_loc_alident loc _a1 in `Dot (loc, _a0, _a1)
    | #alident as _a0 -> (fill_loc_alident loc _a0 :>Ast.dlpath)

let fill_loc_any: FanLoc.t -> AstN.any -> Ast.any =
  fun loc  `Any  -> `Any loc

let rec fill_loc_ctyp: FanLoc.t -> AstN.ctyp -> Ast.ctyp =
  fun loc  ->
    function
    | `Alias (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_alident loc _a1 in `Alias (loc, _a0, _a1)
    | #any as _a0 -> (fill_loc_any loc _a0 :>Ast.ctyp)
    | `App (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `App (loc, _a0, _a1)
    | `Arrow (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Arrow (loc, _a0, _a1)
    | `ClassPath _a0 ->
        let _a0 = fill_loc_ident loc _a0 in `ClassPath (loc, _a0)
    | `Label (_a0,_a1) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Label (loc, _a0, _a1)
    | `OptLabl (_a0,_a1) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `OptLabl (loc, _a0, _a1)
    | #ident' as _a0 -> (fill_loc_ident' loc _a0 :>Ast.ctyp)
    | `TyObj (_a0,_a1) ->
        let _a0 = fill_loc_name_ctyp loc _a0 in
        let _a1 = fill_loc_flag loc _a1 in `TyObj (loc, _a0, _a1)
    | `TyObjEnd _a0 ->
        let _a0 = fill_loc_flag loc _a0 in `TyObjEnd (loc, _a0)
    | `TyPol (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `TyPol (loc, _a0, _a1)
    | `TyPolEnd _a0 ->
        let _a0 = fill_loc_ctyp loc _a0 in `TyPolEnd (loc, _a0)
    | `TyTypePol (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `TyTypePol (loc, _a0, _a1)
    | `Quote (_a0,_a1) ->
        let _a0 = fill_loc_position_flag loc _a0 in
        let _a1 = fill_loc_alident loc _a1 in `Quote (loc, _a0, _a1)
    | `QuoteAny _a0 ->
        let _a0 = fill_loc_position_flag loc _a0 in `QuoteAny (loc, _a0)
    | `Par _a0 -> let _a0 = fill_loc_ctyp loc _a0 in `Par (loc, _a0)
    | `Sta (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Sta (loc, _a0, _a1)
    | `PolyEq _a0 ->
        let _a0 = fill_loc_row_field loc _a0 in `PolyEq (loc, _a0)
    | `PolySup _a0 ->
        let _a0 = fill_loc_row_field loc _a0 in `PolySup (loc, _a0)
    | `PolyInf _a0 ->
        let _a0 = fill_loc_row_field loc _a0 in `PolyInf (loc, _a0)
    | `Com (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Com (loc, _a0, _a1)
    | `PolyInfSup (_a0,_a1) ->
        let _a0 = fill_loc_row_field loc _a0 in
        let _a1 = fill_loc_tag_names loc _a1 in `PolyInfSup (loc, _a0, _a1)
    | `Package _a0 -> let _a0 = fill_loc_mtyp loc _a0 in `Package (loc, _a0)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.ctyp)
and fill_loc_type_parameters:
  FanLoc.t -> AstN.type_parameters -> Ast.type_parameters =
  fun loc  ->
    function
    | `Com (_a0,_a1) ->
        let _a0 = fill_loc_type_parameters loc _a0 in
        let _a1 = fill_loc_type_parameters loc _a1 in `Com (loc, _a0, _a1)
    | `Ctyp _a0 -> let _a0 = fill_loc_ctyp loc _a0 in `Ctyp (loc, _a0)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.type_parameters)
and fill_loc_row_field: FanLoc.t -> AstN.row_field -> Ast.row_field =
  fun loc  ->
    function
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.row_field)
    | `Bar (_a0,_a1) ->
        let _a0 = fill_loc_row_field loc _a0 in
        let _a1 = fill_loc_row_field loc _a1 in `Bar (loc, _a0, _a1)
    | `TyVrn _a0 -> let _a0 = fill_loc_astring loc _a0 in `TyVrn (loc, _a0)
    | `TyVrnOf (_a0,_a1) ->
        let _a0 = fill_loc_astring loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `TyVrnOf (loc, _a0, _a1)
    | `Ctyp _a0 -> let _a0 = fill_loc_ctyp loc _a0 in `Ctyp (loc, _a0)
and fill_loc_tag_names: FanLoc.t -> AstN.tag_names -> Ast.tag_names =
  fun loc  ->
    function
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.tag_names)
    | `App (_a0,_a1) ->
        let _a0 = fill_loc_tag_names loc _a0 in
        let _a1 = fill_loc_tag_names loc _a1 in `App (loc, _a0, _a1)
    | `TyVrn _a0 -> let _a0 = fill_loc_astring loc _a0 in `TyVrn (loc, _a0)
and fill_loc_typedecl: FanLoc.t -> AstN.typedecl -> Ast.typedecl =
  fun loc  ->
    function
    | `TyDcl (_a0,_a1,_a2,_a3) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_opt_decl_params loc _a1 in
        let _a2 = fill_loc_type_info loc _a2 in
        let _a3 = fill_loc_opt_type_constr loc _a3 in
        `TyDcl (loc, _a0, _a1, _a2, _a3)
    | `TyAbstr (_a0,_a1,_a2) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_opt_decl_params loc _a1 in
        let _a2 = fill_loc_opt_type_constr loc _a2 in
        `TyAbstr (loc, _a0, _a1, _a2)
    | `And (_a0,_a1) ->
        let _a0 = fill_loc_typedecl loc _a0 in
        let _a1 = fill_loc_typedecl loc _a1 in `And (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.typedecl)
and fill_loc_type_constr: FanLoc.t -> AstN.type_constr -> Ast.type_constr =
  fun loc  ->
    function
    | `And (_a0,_a1) ->
        let _a0 = fill_loc_type_constr loc _a0 in
        let _a1 = fill_loc_type_constr loc _a1 in `And (loc, _a0, _a1)
    | `Eq (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Eq (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.type_constr)
and fill_loc_opt_type_constr:
  FanLoc.t -> AstN.opt_type_constr -> Ast.opt_type_constr =
  fun loc  ->
    function
    | `Some _a0 -> let _a0 = fill_loc_type_constr loc _a0 in `Some (loc, _a0)
    | `None -> `None loc
and fill_loc_decl_param: FanLoc.t -> AstN.decl_param -> Ast.decl_param =
  fun loc  ->
    function
    | `Quote (_a0,_a1) ->
        let _a0 = fill_loc_position_flag loc _a0 in
        let _a1 = fill_loc_alident loc _a1 in `Quote (loc, _a0, _a1)
    | `QuoteAny _a0 ->
        let _a0 = fill_loc_position_flag loc _a0 in `QuoteAny (loc, _a0)
    | `Any -> `Any loc
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.decl_param)
and fill_loc_decl_params: FanLoc.t -> AstN.decl_params -> Ast.decl_params =
  fun loc  ->
    function
    | `Quote (_a0,_a1) ->
        let _a0 = fill_loc_position_flag loc _a0 in
        let _a1 = fill_loc_alident loc _a1 in `Quote (loc, _a0, _a1)
    | `QuoteAny _a0 ->
        let _a0 = fill_loc_position_flag loc _a0 in `QuoteAny (loc, _a0)
    | `Any -> `Any loc
    | `Com (_a0,_a1) ->
        let _a0 = fill_loc_decl_params loc _a0 in
        let _a1 = fill_loc_decl_params loc _a1 in `Com (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.decl_params)
and fill_loc_opt_decl_params:
  FanLoc.t -> AstN.opt_decl_params -> Ast.opt_decl_params =
  fun loc  ->
    function
    | `Some _a0 -> let _a0 = fill_loc_decl_params loc _a0 in `Some (loc, _a0)
    | `None -> `None loc
and fill_loc_type_info: FanLoc.t -> AstN.type_info -> Ast.type_info =
  fun loc  ->
    function
    | `TyMan (_a0,_a1,_a2) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_flag loc _a1 in
        let _a2 = fill_loc_type_repr loc _a2 in `TyMan (loc, _a0, _a1, _a2)
    | `TyRepr (_a0,_a1) ->
        let _a0 = fill_loc_flag loc _a0 in
        let _a1 = fill_loc_type_repr loc _a1 in `TyRepr (loc, _a0, _a1)
    | `TyEq (_a0,_a1) ->
        let _a0 = fill_loc_flag loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `TyEq (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.type_info)
and fill_loc_type_repr: FanLoc.t -> AstN.type_repr -> Ast.type_repr =
  fun loc  ->
    function
    | `Record _a0 ->
        let _a0 = fill_loc_name_ctyp loc _a0 in `Record (loc, _a0)
    | `Sum _a0 -> let _a0 = fill_loc_or_ctyp loc _a0 in `Sum (loc, _a0)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.type_repr)
and fill_loc_name_ctyp: FanLoc.t -> AstN.name_ctyp -> Ast.name_ctyp =
  fun loc  ->
    function
    | `Sem (_a0,_a1) ->
        let _a0 = fill_loc_name_ctyp loc _a0 in
        let _a1 = fill_loc_name_ctyp loc _a1 in `Sem (loc, _a0, _a1)
    | `TyCol (_a0,_a1) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `TyCol (loc, _a0, _a1)
    | `TyColMut (_a0,_a1) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `TyColMut (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.name_ctyp)
and fill_loc_or_ctyp: FanLoc.t -> AstN.or_ctyp -> Ast.or_ctyp =
  fun loc  ->
    function
    | `Bar (_a0,_a1) ->
        let _a0 = fill_loc_or_ctyp loc _a0 in
        let _a1 = fill_loc_or_ctyp loc _a1 in `Bar (loc, _a0, _a1)
    | `TyCol (_a0,_a1) ->
        let _a0 = fill_loc_auident loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `TyCol (loc, _a0, _a1)
    | `Of (_a0,_a1) ->
        let _a0 = fill_loc_auident loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Of (loc, _a0, _a1)
    | #auident as _a0 -> (fill_loc_auident loc _a0 :>Ast.or_ctyp)
and fill_loc_of_ctyp: FanLoc.t -> AstN.of_ctyp -> Ast.of_ctyp =
  fun loc  ->
    function
    | `Of (_a0,_a1) ->
        let _a0 = fill_loc_vid loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Of (loc, _a0, _a1)
    | #vid' as _a0 -> (fill_loc_vid' loc _a0 :>Ast.of_ctyp)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.of_ctyp)
and fill_loc_pat: FanLoc.t -> AstN.pat -> Ast.pat =
  fun loc  ->
    function
    | #vid as _a0 -> (fill_loc_vid loc _a0 :>Ast.pat)
    | `App (_a0,_a1) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_pat loc _a1 in `App (loc, _a0, _a1)
    | `Vrn _a0 -> `Vrn (loc, _a0)
    | `Com (_a0,_a1) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_pat loc _a1 in `Com (loc, _a0, _a1)
    | `Sem (_a0,_a1) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_pat loc _a1 in `Sem (loc, _a0, _a1)
    | `Par _a0 -> let _a0 = fill_loc_pat loc _a0 in `Par (loc, _a0)
    | #any as _a0 -> (fill_loc_any loc _a0 :>Ast.pat)
    | `Record _a0 -> let _a0 = fill_loc_rec_pat loc _a0 in `Record (loc, _a0)
    | #literal as _a0 -> (fill_loc_literal loc _a0 :>Ast.pat)
    | `Alias (_a0,_a1) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_alident loc _a1 in `Alias (loc, _a0, _a1)
    | `ArrayEmpty -> `ArrayEmpty loc
    | `Array _a0 -> let _a0 = fill_loc_pat loc _a0 in `Array (loc, _a0)
    | `LabelS _a0 -> let _a0 = fill_loc_alident loc _a0 in `LabelS (loc, _a0)
    | `Label (_a0,_a1) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_pat loc _a1 in `Label (loc, _a0, _a1)
    | `OptLabl (_a0,_a1) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_pat loc _a1 in `OptLabl (loc, _a0, _a1)
    | `OptLablS _a0 ->
        let _a0 = fill_loc_alident loc _a0 in `OptLablS (loc, _a0)
    | `OptLablExpr (_a0,_a1,_a2) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_pat loc _a1 in
        let _a2 = fill_loc_exp loc _a2 in `OptLablExpr (loc, _a0, _a1, _a2)
    | `Bar (_a0,_a1) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_pat loc _a1 in `Bar (loc, _a0, _a1)
    | `PaRng (_a0,_a1) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_pat loc _a1 in `PaRng (loc, _a0, _a1)
    | `Constraint (_a0,_a1) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Constraint (loc, _a0, _a1)
    | `ClassPath _a0 ->
        let _a0 = fill_loc_ident loc _a0 in `ClassPath (loc, _a0)
    | `Lazy _a0 -> let _a0 = fill_loc_pat loc _a0 in `Lazy (loc, _a0)
    | `ModuleUnpack _a0 ->
        let _a0 = fill_loc_auident loc _a0 in `ModuleUnpack (loc, _a0)
    | `ModuleConstraint (_a0,_a1) ->
        let _a0 = fill_loc_auident loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `ModuleConstraint (loc, _a0, _a1)
and fill_loc_rec_pat: FanLoc.t -> AstN.rec_pat -> Ast.rec_pat =
  fun loc  ->
    function
    | `RecBind (_a0,_a1) ->
        let _a0 = fill_loc_ident loc _a0 in
        let _a1 = fill_loc_pat loc _a1 in `RecBind (loc, _a0, _a1)
    | `Sem (_a0,_a1) ->
        let _a0 = fill_loc_rec_pat loc _a0 in
        let _a1 = fill_loc_rec_pat loc _a1 in `Sem (loc, _a0, _a1)
    | #any as _a0 -> (fill_loc_any loc _a0 :>Ast.rec_pat)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.rec_pat)
and fill_loc_exp: FanLoc.t -> AstN.exp -> Ast.exp =
  fun loc  ->
    function
    | #vid as _a0 -> (fill_loc_vid loc _a0 :>Ast.exp)
    | `App (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `App (loc, _a0, _a1)
    | `Vrn _a0 -> `Vrn (loc, _a0)
    | `Com (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `Com (loc, _a0, _a1)
    | `Sem (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `Sem (loc, _a0, _a1)
    | `Par _a0 -> let _a0 = fill_loc_exp loc _a0 in `Par (loc, _a0)
    | #any as _a0 -> (fill_loc_any loc _a0 :>Ast.exp)
    | `Record _a0 -> let _a0 = fill_loc_rec_exp loc _a0 in `Record (loc, _a0)
    | #literal as _a0 -> (fill_loc_literal loc _a0 :>Ast.exp)
    | `RecordWith (_a0,_a1) ->
        let _a0 = fill_loc_rec_exp loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `RecordWith (loc, _a0, _a1)
    | `Field (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `Field (loc, _a0, _a1)
    | `ArrayDot (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `ArrayDot (loc, _a0, _a1)
    | `ArrayEmpty -> `ArrayEmpty loc
    | `Array _a0 -> let _a0 = fill_loc_exp loc _a0 in `Array (loc, _a0)
    | `Assert _a0 -> let _a0 = fill_loc_exp loc _a0 in `Assert (loc, _a0)
    | `Assign (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `Assign (loc, _a0, _a1)
    | `For (_a0,_a1,_a2,_a3,_a4) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in
        let _a2 = fill_loc_exp loc _a2 in
        let _a3 = fill_loc_flag loc _a3 in
        let _a4 = fill_loc_exp loc _a4 in `For (loc, _a0, _a1, _a2, _a3, _a4)
    | `Fun _a0 -> let _a0 = fill_loc_case loc _a0 in `Fun (loc, _a0)
    | `IfThenElse (_a0,_a1,_a2) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in
        let _a2 = fill_loc_exp loc _a2 in `IfThenElse (loc, _a0, _a1, _a2)
    | `IfThen (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `IfThen (loc, _a0, _a1)
    | `LabelS _a0 -> let _a0 = fill_loc_alident loc _a0 in `LabelS (loc, _a0)
    | `Label (_a0,_a1) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `Label (loc, _a0, _a1)
    | `Lazy _a0 -> let _a0 = fill_loc_exp loc _a0 in `Lazy (loc, _a0)
    | `LetIn (_a0,_a1,_a2) ->
        let _a0 = fill_loc_flag loc _a0 in
        let _a1 = fill_loc_bind loc _a1 in
        let _a2 = fill_loc_exp loc _a2 in `LetIn (loc, _a0, _a1, _a2)
    | `LetTryInWith (_a0,_a1,_a2,_a3) ->
        let _a0 = fill_loc_flag loc _a0 in
        let _a1 = fill_loc_bind loc _a1 in
        let _a2 = fill_loc_exp loc _a2 in
        let _a3 = fill_loc_case loc _a3 in
        `LetTryInWith (loc, _a0, _a1, _a2, _a3)
    | `LetModule (_a0,_a1,_a2) ->
        let _a0 = fill_loc_auident loc _a0 in
        let _a1 = fill_loc_mexp loc _a1 in
        let _a2 = fill_loc_exp loc _a2 in `LetModule (loc, _a0, _a1, _a2)
    | `Match (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_case loc _a1 in `Match (loc, _a0, _a1)
    | `New _a0 -> let _a0 = fill_loc_ident loc _a0 in `New (loc, _a0)
    | `Obj _a0 -> let _a0 = fill_loc_clfield loc _a0 in `Obj (loc, _a0)
    | `ObjEnd -> `ObjEnd loc
    | `ObjPat (_a0,_a1) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_clfield loc _a1 in `ObjPat (loc, _a0, _a1)
    | `ObjPatEnd _a0 ->
        let _a0 = fill_loc_pat loc _a0 in `ObjPatEnd (loc, _a0)
    | `OptLabl (_a0,_a1) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `OptLabl (loc, _a0, _a1)
    | `OptLablS _a0 ->
        let _a0 = fill_loc_alident loc _a0 in `OptLablS (loc, _a0)
    | `OvrInst _a0 ->
        let _a0 = fill_loc_rec_exp loc _a0 in `OvrInst (loc, _a0)
    | `OvrInstEmpty -> `OvrInstEmpty loc
    | `Seq _a0 -> let _a0 = fill_loc_exp loc _a0 in `Seq (loc, _a0)
    | `Send (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_alident loc _a1 in `Send (loc, _a0, _a1)
    | `StringDot (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `StringDot (loc, _a0, _a1)
    | `Try (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_case loc _a1 in `Try (loc, _a0, _a1)
    | `Constraint (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Constraint (loc, _a0, _a1)
    | `Coercion (_a0,_a1,_a2) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in
        let _a2 = fill_loc_ctyp loc _a2 in `Coercion (loc, _a0, _a1, _a2)
    | `Subtype (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Subtype (loc, _a0, _a1)
    | `While (_a0,_a1) ->
        let _a0 = fill_loc_exp loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `While (loc, _a0, _a1)
    | `LetOpen (_a0,_a1) ->
        let _a0 = fill_loc_ident loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `LetOpen (loc, _a0, _a1)
    | `LocalTypeFun (_a0,_a1) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `LocalTypeFun (loc, _a0, _a1)
    | `Package_exp _a0 ->
        let _a0 = fill_loc_mexp loc _a0 in `Package_exp (loc, _a0)
and fill_loc_rec_exp: FanLoc.t -> AstN.rec_exp -> Ast.rec_exp =
  fun loc  ->
    function
    | `Sem (_a0,_a1) ->
        let _a0 = fill_loc_rec_exp loc _a0 in
        let _a1 = fill_loc_rec_exp loc _a1 in `Sem (loc, _a0, _a1)
    | `RecBind (_a0,_a1) ->
        let _a0 = fill_loc_ident loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `RecBind (loc, _a0, _a1)
    | #any as _a0 -> (fill_loc_any loc _a0 :>Ast.rec_exp)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.rec_exp)
and fill_loc_mtyp: FanLoc.t -> AstN.mtyp -> Ast.mtyp =
  fun loc  ->
    function
    | #ident' as _a0 -> (fill_loc_ident' loc _a0 :>Ast.mtyp)
    | `Sig _a0 -> let _a0 = fill_loc_sigi loc _a0 in `Sig (loc, _a0)
    | `SigEnd -> `SigEnd loc
    | `Functor (_a0,_a1,_a2) ->
        let _a0 = fill_loc_auident loc _a0 in
        let _a1 = fill_loc_mtyp loc _a1 in
        let _a2 = fill_loc_mtyp loc _a2 in `Functor (loc, _a0, _a1, _a2)
    | `With (_a0,_a1) ->
        let _a0 = fill_loc_mtyp loc _a0 in
        let _a1 = fill_loc_constr loc _a1 in `With (loc, _a0, _a1)
    | `ModuleTypeOf _a0 ->
        let _a0 = fill_loc_mexp loc _a0 in `ModuleTypeOf (loc, _a0)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.mtyp)
and fill_loc_sigi: FanLoc.t -> AstN.sigi -> Ast.sigi =
  fun loc  ->
    function
    | `Val (_a0,_a1) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Val (loc, _a0, _a1)
    | `External (_a0,_a1,_a2) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in
        let _a2 = fill_loc_strings loc _a2 in `External (loc, _a0, _a1, _a2)
    | `Type _a0 -> let _a0 = fill_loc_typedecl loc _a0 in `Type (loc, _a0)
    | `Exception _a0 ->
        let _a0 = fill_loc_of_ctyp loc _a0 in `Exception (loc, _a0)
    | `Class _a0 -> let _a0 = fill_loc_cltdecl loc _a0 in `Class (loc, _a0)
    | `ClassType _a0 ->
        let _a0 = fill_loc_cltdecl loc _a0 in `ClassType (loc, _a0)
    | `Module (_a0,_a1) ->
        let _a0 = fill_loc_auident loc _a0 in
        let _a1 = fill_loc_mtyp loc _a1 in `Module (loc, _a0, _a1)
    | `ModuleTypeEnd _a0 ->
        let _a0 = fill_loc_auident loc _a0 in `ModuleTypeEnd (loc, _a0)
    | `ModuleType (_a0,_a1) ->
        let _a0 = fill_loc_auident loc _a0 in
        let _a1 = fill_loc_mtyp loc _a1 in `ModuleType (loc, _a0, _a1)
    | `Sem (_a0,_a1) ->
        let _a0 = fill_loc_sigi loc _a0 in
        let _a1 = fill_loc_sigi loc _a1 in `Sem (loc, _a0, _a1)
    | `DirectiveSimple _a0 ->
        let _a0 = fill_loc_alident loc _a0 in `DirectiveSimple (loc, _a0)
    | `Directive (_a0,_a1) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `Directive (loc, _a0, _a1)
    | `Open _a0 -> let _a0 = fill_loc_ident loc _a0 in `Open (loc, _a0)
    | `Include _a0 -> let _a0 = fill_loc_mtyp loc _a0 in `Include (loc, _a0)
    | `RecModule _a0 ->
        let _a0 = fill_loc_mbind loc _a0 in `RecModule (loc, _a0)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.sigi)
and fill_loc_mbind: FanLoc.t -> AstN.mbind -> Ast.mbind =
  fun loc  ->
    function
    | `And (_a0,_a1) ->
        let _a0 = fill_loc_mbind loc _a0 in
        let _a1 = fill_loc_mbind loc _a1 in `And (loc, _a0, _a1)
    | `ModuleBind (_a0,_a1,_a2) ->
        let _a0 = fill_loc_auident loc _a0 in
        let _a1 = fill_loc_mtyp loc _a1 in
        let _a2 = fill_loc_mexp loc _a2 in `ModuleBind (loc, _a0, _a1, _a2)
    | `Constraint (_a0,_a1) ->
        let _a0 = fill_loc_auident loc _a0 in
        let _a1 = fill_loc_mtyp loc _a1 in `Constraint (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.mbind)
and fill_loc_constr: FanLoc.t -> AstN.constr -> Ast.constr =
  fun loc  ->
    function
    | `TypeEq (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `TypeEq (loc, _a0, _a1)
    | `ModuleEq (_a0,_a1) ->
        let _a0 = fill_loc_ident loc _a0 in
        let _a1 = fill_loc_ident loc _a1 in `ModuleEq (loc, _a0, _a1)
    | `TypeEqPriv (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `TypeEqPriv (loc, _a0, _a1)
    | `TypeSubst (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `TypeSubst (loc, _a0, _a1)
    | `ModuleSubst (_a0,_a1) ->
        let _a0 = fill_loc_ident loc _a0 in
        let _a1 = fill_loc_ident loc _a1 in `ModuleSubst (loc, _a0, _a1)
    | `And (_a0,_a1) ->
        let _a0 = fill_loc_constr loc _a0 in
        let _a1 = fill_loc_constr loc _a1 in `And (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.constr)
and fill_loc_bind: FanLoc.t -> AstN.bind -> Ast.bind =
  fun loc  ->
    function
    | `And (_a0,_a1) ->
        let _a0 = fill_loc_bind loc _a0 in
        let _a1 = fill_loc_bind loc _a1 in `And (loc, _a0, _a1)
    | `Bind (_a0,_a1) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `Bind (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.bind)
and fill_loc_case: FanLoc.t -> AstN.case -> Ast.case =
  fun loc  ->
    function
    | `Bar (_a0,_a1) ->
        let _a0 = fill_loc_case loc _a0 in
        let _a1 = fill_loc_case loc _a1 in `Bar (loc, _a0, _a1)
    | `Case (_a0,_a1) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `Case (loc, _a0, _a1)
    | `CaseWhen (_a0,_a1,_a2) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in
        let _a2 = fill_loc_exp loc _a2 in `CaseWhen (loc, _a0, _a1, _a2)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.case)
and fill_loc_mexp: FanLoc.t -> AstN.mexp -> Ast.mexp =
  fun loc  ->
    function
    | #vid' as _a0 -> (fill_loc_vid' loc _a0 :>Ast.mexp)
    | `App (_a0,_a1) ->
        let _a0 = fill_loc_mexp loc _a0 in
        let _a1 = fill_loc_mexp loc _a1 in `App (loc, _a0, _a1)
    | `Functor (_a0,_a1,_a2) ->
        let _a0 = fill_loc_auident loc _a0 in
        let _a1 = fill_loc_mtyp loc _a1 in
        let _a2 = fill_loc_mexp loc _a2 in `Functor (loc, _a0, _a1, _a2)
    | `Struct _a0 -> let _a0 = fill_loc_stru loc _a0 in `Struct (loc, _a0)
    | `StructEnd -> `StructEnd loc
    | `Constraint (_a0,_a1) ->
        let _a0 = fill_loc_mexp loc _a0 in
        let _a1 = fill_loc_mtyp loc _a1 in `Constraint (loc, _a0, _a1)
    | `PackageModule _a0 ->
        let _a0 = fill_loc_exp loc _a0 in `PackageModule (loc, _a0)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.mexp)
and fill_loc_stru: FanLoc.t -> AstN.stru -> Ast.stru =
  fun loc  ->
    function
    | `Class _a0 -> let _a0 = fill_loc_cldecl loc _a0 in `Class (loc, _a0)
    | `ClassType _a0 ->
        let _a0 = fill_loc_cltdecl loc _a0 in `ClassType (loc, _a0)
    | `Sem (_a0,_a1) ->
        let _a0 = fill_loc_stru loc _a0 in
        let _a1 = fill_loc_stru loc _a1 in `Sem (loc, _a0, _a1)
    | `DirectiveSimple _a0 ->
        let _a0 = fill_loc_alident loc _a0 in `DirectiveSimple (loc, _a0)
    | `Directive (_a0,_a1) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `Directive (loc, _a0, _a1)
    | `Exception _a0 ->
        let _a0 = fill_loc_of_ctyp loc _a0 in `Exception (loc, _a0)
    | `StExp _a0 -> let _a0 = fill_loc_exp loc _a0 in `StExp (loc, _a0)
    | `External (_a0,_a1,_a2) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in
        let _a2 = fill_loc_strings loc _a2 in `External (loc, _a0, _a1, _a2)
    | `Include _a0 -> let _a0 = fill_loc_mexp loc _a0 in `Include (loc, _a0)
    | `Module (_a0,_a1) ->
        let _a0 = fill_loc_auident loc _a0 in
        let _a1 = fill_loc_mexp loc _a1 in `Module (loc, _a0, _a1)
    | `RecModule _a0 ->
        let _a0 = fill_loc_mbind loc _a0 in `RecModule (loc, _a0)
    | `ModuleType (_a0,_a1) ->
        let _a0 = fill_loc_auident loc _a0 in
        let _a1 = fill_loc_mtyp loc _a1 in `ModuleType (loc, _a0, _a1)
    | `Open _a0 -> let _a0 = fill_loc_ident loc _a0 in `Open (loc, _a0)
    | `Type _a0 -> let _a0 = fill_loc_typedecl loc _a0 in `Type (loc, _a0)
    | `TypeWith (_a0,_a1) ->
        let _a0 = fill_loc_typedecl loc _a0 in
        let _a1 = fill_loc_strings loc _a1 in `TypeWith (loc, _a0, _a1)
    | `Value (_a0,_a1) ->
        let _a0 = fill_loc_flag loc _a0 in
        let _a1 = fill_loc_bind loc _a1 in `Value (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.stru)
and fill_loc_cltdecl: FanLoc.t -> AstN.cltdecl -> Ast.cltdecl =
  fun loc  ->
    function
    | `And (_a0,_a1) ->
        let _a0 = fill_loc_cltdecl loc _a0 in
        let _a1 = fill_loc_cltdecl loc _a1 in `And (loc, _a0, _a1)
    | `CtDecl (_a0,_a1,_a2,_a3) ->
        let _a0 = fill_loc_flag loc _a0 in
        let _a1 = fill_loc_ident loc _a1 in
        let _a2 = fill_loc_type_parameters loc _a2 in
        let _a3 = fill_loc_cltyp loc _a3 in `CtDecl (loc, _a0, _a1, _a2, _a3)
    | `CtDeclS (_a0,_a1,_a2) ->
        let _a0 = fill_loc_flag loc _a0 in
        let _a1 = fill_loc_ident loc _a1 in
        let _a2 = fill_loc_cltyp loc _a2 in `CtDeclS (loc, _a0, _a1, _a2)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.cltdecl)
and fill_loc_cltyp: FanLoc.t -> AstN.cltyp -> Ast.cltyp =
  fun loc  ->
    function
    | #vid' as _a0 -> (fill_loc_vid' loc _a0 :>Ast.cltyp)
    | `ClApply (_a0,_a1) ->
        let _a0 = fill_loc_vid loc _a0 in
        let _a1 = fill_loc_type_parameters loc _a1 in
        `ClApply (loc, _a0, _a1)
    | `CtFun (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_cltyp loc _a1 in `CtFun (loc, _a0, _a1)
    | `ObjTy (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_clsigi loc _a1 in `ObjTy (loc, _a0, _a1)
    | `ObjTyEnd _a0 ->
        let _a0 = fill_loc_ctyp loc _a0 in `ObjTyEnd (loc, _a0)
    | `Obj _a0 -> let _a0 = fill_loc_clsigi loc _a0 in `Obj (loc, _a0)
    | `ObjEnd -> `ObjEnd loc
    | `And (_a0,_a1) ->
        let _a0 = fill_loc_cltyp loc _a0 in
        let _a1 = fill_loc_cltyp loc _a1 in `And (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.cltyp)
and fill_loc_clsigi: FanLoc.t -> AstN.clsigi -> Ast.clsigi =
  fun loc  ->
    function
    | `Sem (_a0,_a1) ->
        let _a0 = fill_loc_clsigi loc _a0 in
        let _a1 = fill_loc_clsigi loc _a1 in `Sem (loc, _a0, _a1)
    | `SigInherit _a0 ->
        let _a0 = fill_loc_cltyp loc _a0 in `SigInherit (loc, _a0)
    | `CgVal (_a0,_a1,_a2,_a3) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_flag loc _a1 in
        let _a2 = fill_loc_flag loc _a2 in
        let _a3 = fill_loc_ctyp loc _a3 in `CgVal (loc, _a0, _a1, _a2, _a3)
    | `Method (_a0,_a1,_a2) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_flag loc _a1 in
        let _a2 = fill_loc_ctyp loc _a2 in `Method (loc, _a0, _a1, _a2)
    | `VirMeth (_a0,_a1,_a2) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_flag loc _a1 in
        let _a2 = fill_loc_ctyp loc _a2 in `VirMeth (loc, _a0, _a1, _a2)
    | `Eq (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Eq (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.clsigi)
and fill_loc_cldecl: FanLoc.t -> AstN.cldecl -> Ast.cldecl =
  fun loc  ->
    function
    | `ClDecl (_a0,_a1,_a2,_a3) ->
        let _a0 = fill_loc_flag loc _a0 in
        let _a1 = fill_loc_ident loc _a1 in
        let _a2 = fill_loc_type_parameters loc _a2 in
        let _a3 = fill_loc_clexp loc _a3 in `ClDecl (loc, _a0, _a1, _a2, _a3)
    | `ClDeclS (_a0,_a1,_a2) ->
        let _a0 = fill_loc_flag loc _a0 in
        let _a1 = fill_loc_ident loc _a1 in
        let _a2 = fill_loc_clexp loc _a2 in `ClDeclS (loc, _a0, _a1, _a2)
    | `And (_a0,_a1) ->
        let _a0 = fill_loc_cldecl loc _a0 in
        let _a1 = fill_loc_cldecl loc _a1 in `And (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.cldecl)
and fill_loc_clexp: FanLoc.t -> AstN.clexp -> Ast.clexp =
  fun loc  ->
    function
    | `CeApp (_a0,_a1) ->
        let _a0 = fill_loc_clexp loc _a0 in
        let _a1 = fill_loc_exp loc _a1 in `CeApp (loc, _a0, _a1)
    | #vid' as _a0 -> (fill_loc_vid' loc _a0 :>Ast.clexp)
    | `ClApply (_a0,_a1) ->
        let _a0 = fill_loc_vid loc _a0 in
        let _a1 = fill_loc_type_parameters loc _a1 in
        `ClApply (loc, _a0, _a1)
    | `CeFun (_a0,_a1) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_clexp loc _a1 in `CeFun (loc, _a0, _a1)
    | `LetIn (_a0,_a1,_a2) ->
        let _a0 = fill_loc_flag loc _a0 in
        let _a1 = fill_loc_bind loc _a1 in
        let _a2 = fill_loc_clexp loc _a2 in `LetIn (loc, _a0, _a1, _a2)
    | `Obj _a0 -> let _a0 = fill_loc_clfield loc _a0 in `Obj (loc, _a0)
    | `ObjEnd -> `ObjEnd loc
    | `ObjPat (_a0,_a1) ->
        let _a0 = fill_loc_pat loc _a0 in
        let _a1 = fill_loc_clfield loc _a1 in `ObjPat (loc, _a0, _a1)
    | `ObjPatEnd _a0 ->
        let _a0 = fill_loc_pat loc _a0 in `ObjPatEnd (loc, _a0)
    | `Constraint (_a0,_a1) ->
        let _a0 = fill_loc_clexp loc _a0 in
        let _a1 = fill_loc_cltyp loc _a1 in `Constraint (loc, _a0, _a1)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.clexp)
and fill_loc_clfield: FanLoc.t -> AstN.clfield -> Ast.clfield =
  fun loc  ->
    function
    | `Sem (_a0,_a1) ->
        let _a0 = fill_loc_clfield loc _a0 in
        let _a1 = fill_loc_clfield loc _a1 in `Sem (loc, _a0, _a1)
    | `Inherit (_a0,_a1) ->
        let _a0 = fill_loc_flag loc _a0 in
        let _a1 = fill_loc_clexp loc _a1 in `Inherit (loc, _a0, _a1)
    | `InheritAs (_a0,_a1,_a2) ->
        let _a0 = fill_loc_flag loc _a0 in
        let _a1 = fill_loc_clexp loc _a1 in
        let _a2 = fill_loc_alident loc _a2 in `InheritAs (loc, _a0, _a1, _a2)
    | `CrVal (_a0,_a1,_a2,_a3) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_flag loc _a1 in
        let _a2 = fill_loc_flag loc _a2 in
        let _a3 = fill_loc_exp loc _a3 in `CrVal (loc, _a0, _a1, _a2, _a3)
    | `VirVal (_a0,_a1,_a2) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_flag loc _a1 in
        let _a2 = fill_loc_ctyp loc _a2 in `VirVal (loc, _a0, _a1, _a2)
    | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_flag loc _a1 in
        let _a2 = fill_loc_flag loc _a2 in
        let _a3 = fill_loc_exp loc _a3 in
        let _a4 = fill_loc_ctyp loc _a4 in
        `CrMth (loc, _a0, _a1, _a2, _a3, _a4)
    | `CrMthS (_a0,_a1,_a2,_a3) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_flag loc _a1 in
        let _a2 = fill_loc_flag loc _a2 in
        let _a3 = fill_loc_exp loc _a3 in `CrMthS (loc, _a0, _a1, _a2, _a3)
    | `VirMeth (_a0,_a1,_a2) ->
        let _a0 = fill_loc_alident loc _a0 in
        let _a1 = fill_loc_flag loc _a1 in
        let _a2 = fill_loc_ctyp loc _a2 in `VirMeth (loc, _a0, _a1, _a2)
    | `Eq (_a0,_a1) ->
        let _a0 = fill_loc_ctyp loc _a0 in
        let _a1 = fill_loc_ctyp loc _a1 in `Eq (loc, _a0, _a1)
    | `Initializer _a0 ->
        let _a0 = fill_loc_exp loc _a0 in `Initializer (loc, _a0)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.clfield)

let rec fill_loc_ep: FanLoc.t -> AstN.ep -> Ast.ep =
  fun loc  ->
    function
    | #vid as _a0 -> (fill_loc_vid loc _a0 :>Ast.ep)
    | `App (_a0,_a1) ->
        let _a0 = fill_loc_ep loc _a0 in
        let _a1 = fill_loc_ep loc _a1 in `App (loc, _a0, _a1)
    | `Vrn _a0 -> `Vrn (loc, _a0)
    | `Com (_a0,_a1) ->
        let _a0 = fill_loc_ep loc _a0 in
        let _a1 = fill_loc_ep loc _a1 in `Com (loc, _a0, _a1)
    | `Sem (_a0,_a1) ->
        let _a0 = fill_loc_ep loc _a0 in
        let _a1 = fill_loc_ep loc _a1 in `Sem (loc, _a0, _a1)
    | `Par _a0 -> let _a0 = fill_loc_ep loc _a0 in `Par (loc, _a0)
    | #any as _a0 -> (fill_loc_any loc _a0 :>Ast.ep)
    | `ArrayEmpty -> `ArrayEmpty loc
    | `Array _a0 -> let _a0 = fill_loc_ep loc _a0 in `Array (loc, _a0)
    | `Record _a0 ->
        let _a0 = fill_loc_rec_bind loc _a0 in `Record (loc, _a0)
    | #literal as _a0 -> (fill_loc_literal loc _a0 :>Ast.ep)
and fill_loc_rec_bind: FanLoc.t -> AstN.rec_bind -> Ast.rec_bind =
  fun loc  ->
    function
    | `RecBind (_a0,_a1) ->
        let _a0 = fill_loc_ident loc _a0 in
        let _a1 = fill_loc_ep loc _a1 in `RecBind (loc, _a0, _a1)
    | `Sem (_a0,_a1) ->
        let _a0 = fill_loc_rec_bind loc _a0 in
        let _a1 = fill_loc_rec_bind loc _a1 in `Sem (loc, _a0, _a1)
    | #any as _a0 -> (fill_loc_any loc _a0 :>Ast.rec_bind)
    | #ant as _a0 -> (fill_loc_ant loc _a0 :>Ast.rec_bind)

class meta =
  object (self : 'self_type)
    inherit  primitive
    method nil : 'loc -> nil -> Ast.ep= fun _loc  `Nil  -> `Vrn (_loc, "Nil")
    method literal : 'loc -> literal -> Ast.ep=
      fun _loc  ->
        function
        | `Chr _a0 ->
            `App (_loc, (`Vrn (_loc, "Chr")), (self#string _loc _a0))
        | `Int _a0 ->
            `App (_loc, (`Vrn (_loc, "Int")), (self#string _loc _a0))
        | `Int32 _a0 ->
            `App (_loc, (`Vrn (_loc, "Int32")), (self#string _loc _a0))
        | `Int64 _a0 ->
            `App (_loc, (`Vrn (_loc, "Int64")), (self#string _loc _a0))
        | `Flo _a0 ->
            `App (_loc, (`Vrn (_loc, "Flo")), (self#string _loc _a0))
        | `Nativeint _a0 ->
            `App (_loc, (`Vrn (_loc, "Nativeint")), (self#string _loc _a0))
        | `Str _a0 ->
            `App (_loc, (`Vrn (_loc, "Str")), (self#string _loc _a0))
    method flag : 'loc -> flag -> Ast.ep=
      fun _loc  ->
        function
        | `Positive -> `Vrn (_loc, "Positive")
        | `Negative -> `Vrn (_loc, "Negative")
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method position_flag : 'loc -> position_flag -> Ast.ep=
      fun _loc  ->
        function
        | `Positive -> `Vrn (_loc, "Positive")
        | `Negative -> `Vrn (_loc, "Negative")
        | `Normal -> `Vrn (_loc, "Normal")
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method strings : 'loc -> strings -> Ast.ep=
      fun _loc  ->
        function
        | `App (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "App")), (self#strings _loc _a0))),
                (self#strings _loc _a1))
        | `Str _a0 ->
            `App (_loc, (`Vrn (_loc, "Str")), (self#string _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method lident : 'loc -> lident -> Ast.ep=
      fun _loc  (`Lid _a0)  ->
        `App (_loc, (`Vrn (_loc, "Lid")), (self#string _loc _a0))
    method alident : 'loc -> alident -> Ast.ep=
      fun _loc  ->
        function
        | `Lid _a0 ->
            `App (_loc, (`Vrn (_loc, "Lid")), (self#string _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method auident : 'loc -> auident -> Ast.ep=
      fun _loc  ->
        function
        | `Uid _a0 ->
            `App (_loc, (`Vrn (_loc, "Uid")), (self#string _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method aident : 'loc -> aident -> Ast.ep=
      fun _loc  ->
        function
        | #alident as _a0 -> (self#alident _loc _a0 :>Ast.ep)
        | #auident as _a0 -> (self#auident _loc _a0 :>Ast.ep)
    method astring : 'loc -> astring -> Ast.ep=
      fun _loc  ->
        function
        | `C _a0 -> `App (_loc, (`Vrn (_loc, "C")), (self#string _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method uident : 'loc -> uident -> Ast.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Dot")), (self#uident _loc _a0))),
                (self#uident _loc _a1))
        | `App (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "App")), (self#uident _loc _a0))),
                (self#uident _loc _a1))
        | #auident as _a0 -> (self#auident _loc _a0 :>Ast.ep)
    method ident : 'loc -> ident -> Ast.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Dot")), (self#ident _loc _a0))),
                (self#ident _loc _a1))
        | `Apply (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Apply")), (self#ident _loc _a0))),
                (self#ident _loc _a1))
        | #alident as _a0 -> (self#alident _loc _a0 :>Ast.ep)
        | #auident as _a0 -> (self#auident _loc _a0 :>Ast.ep)
    method ident' : 'loc -> ident' -> Ast.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Dot")), (self#ident _loc _a0))),
                (self#ident _loc _a1))
        | `Apply (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Apply")), (self#ident _loc _a0))),
                (self#ident _loc _a1))
        | `Lid _a0 ->
            `App (_loc, (`Vrn (_loc, "Lid")), (self#string _loc _a0))
        | `Uid _a0 ->
            `App (_loc, (`Vrn (_loc, "Uid")), (self#string _loc _a0))
    method vid : 'loc -> vid -> Ast.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Dot")), (self#vid _loc _a0))),
                (self#vid _loc _a1))
        | `Lid _a0 ->
            `App (_loc, (`Vrn (_loc, "Lid")), (self#string _loc _a0))
        | `Uid _a0 ->
            `App (_loc, (`Vrn (_loc, "Uid")), (self#string _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method vid' : 'loc -> vid' -> Ast.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Dot")), (self#vid _loc _a0))),
                (self#vid _loc _a1))
        | `Lid _a0 ->
            `App (_loc, (`Vrn (_loc, "Lid")), (self#string _loc _a0))
        | `Uid _a0 ->
            `App (_loc, (`Vrn (_loc, "Uid")), (self#string _loc _a0))
    method dupath : 'loc -> dupath -> Ast.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Dot")), (self#dupath _loc _a0))),
                (self#dupath _loc _a1))
        | #auident as _a0 -> (self#auident _loc _a0 :>Ast.ep)
    method dlpath : 'loc -> dlpath -> Ast.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Dot")), (self#dupath _loc _a0))),
                (self#alident _loc _a1))
        | #alident as _a0 -> (self#alident _loc _a0 :>Ast.ep)
    method any : 'loc -> any -> Ast.ep= fun _loc  `Any  -> `Vrn (_loc, "Any")
    method ctyp : 'loc -> ctyp -> Ast.ep=
      fun _loc  ->
        function
        | `Alias (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Alias")), (self#ctyp _loc _a0))),
                (self#alident _loc _a1))
        | #any as _a0 -> (self#any _loc _a0 :>Ast.ep)
        | `App (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "App")), (self#ctyp _loc _a0))),
                (self#ctyp _loc _a1))
        | `Arrow (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Arrow")), (self#ctyp _loc _a0))),
                (self#ctyp _loc _a1))
        | `ClassPath _a0 ->
            `App (_loc, (`Vrn (_loc, "ClassPath")), (self#ident _loc _a0))
        | `Label (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Label")), (self#alident _loc _a0))),
                (self#ctyp _loc _a1))
        | `OptLabl (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "OptLabl")), (self#alident _loc _a0))),
                (self#ctyp _loc _a1))
        | #ident' as _a0 -> (self#ident' _loc _a0 :>Ast.ep)
        | `TyObj (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "TyObj")), (self#name_ctyp _loc _a0))),
                (self#flag _loc _a1))
        | `TyObjEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "TyObjEnd")), (self#flag _loc _a0))
        | `TyPol (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "TyPol")), (self#ctyp _loc _a0))),
                (self#ctyp _loc _a1))
        | `TyPolEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "TyPolEnd")), (self#ctyp _loc _a0))
        | `TyTypePol (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "TyTypePol")), (self#ctyp _loc _a0))),
                (self#ctyp _loc _a1))
        | `Quote (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Quote")),
                     (self#position_flag _loc _a0))),
                (self#alident _loc _a1))
        | `QuoteAny _a0 ->
            `App
              (_loc, (`Vrn (_loc, "QuoteAny")),
                (self#position_flag _loc _a0))
        | `Par _a0 -> `App (_loc, (`Vrn (_loc, "Par")), (self#ctyp _loc _a0))
        | `Sta (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sta")), (self#ctyp _loc _a0))),
                (self#ctyp _loc _a1))
        | `PolyEq _a0 ->
            `App (_loc, (`Vrn (_loc, "PolyEq")), (self#row_field _loc _a0))
        | `PolySup _a0 ->
            `App (_loc, (`Vrn (_loc, "PolySup")), (self#row_field _loc _a0))
        | `PolyInf _a0 ->
            `App (_loc, (`Vrn (_loc, "PolyInf")), (self#row_field _loc _a0))
        | `Com (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Com")), (self#ctyp _loc _a0))),
                (self#ctyp _loc _a1))
        | `PolyInfSup (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "PolyInfSup")),
                     (self#row_field _loc _a0))), (self#tag_names _loc _a1))
        | `Package _a0 ->
            `App (_loc, (`Vrn (_loc, "Package")), (self#mtyp _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method type_parameters : 'loc -> type_parameters -> Ast.ep=
      fun _loc  ->
        function
        | `Com (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Com")),
                     (self#type_parameters _loc _a0))),
                (self#type_parameters _loc _a1))
        | `Ctyp _a0 ->
            `App (_loc, (`Vrn (_loc, "Ctyp")), (self#ctyp _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method row_field : 'loc -> row_field -> Ast.ep=
      fun _loc  ->
        function
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
        | `Bar (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Bar")), (self#row_field _loc _a0))),
                (self#row_field _loc _a1))
        | `TyVrn _a0 ->
            `App (_loc, (`Vrn (_loc, "TyVrn")), (self#astring _loc _a0))
        | `TyVrnOf (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "TyVrnOf")), (self#astring _loc _a0))),
                (self#ctyp _loc _a1))
        | `Ctyp _a0 ->
            `App (_loc, (`Vrn (_loc, "Ctyp")), (self#ctyp _loc _a0))
    method tag_names : 'loc -> tag_names -> Ast.ep=
      fun _loc  ->
        function
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
        | `App (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "App")), (self#tag_names _loc _a0))),
                (self#tag_names _loc _a1))
        | `TyVrn _a0 ->
            `App (_loc, (`Vrn (_loc, "TyVrn")), (self#astring _loc _a0))
    method typedecl : 'loc -> typedecl -> Ast.ep=
      fun _loc  ->
        function
        | `TyDcl (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "TyDcl")),
                               (self#alident _loc _a0))),
                          (self#opt_decl_params _loc _a1))),
                     (self#type_info _loc _a2))),
                (self#opt_type_constr _loc _a3))
        | `TyAbstr (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyAbstr")),
                          (self#alident _loc _a0))),
                     (self#opt_decl_params _loc _a1))),
                (self#opt_type_constr _loc _a2))
        | `And (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "And")), (self#typedecl _loc _a0))),
                (self#typedecl _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method type_constr : 'loc -> type_constr -> Ast.ep=
      fun _loc  ->
        function
        | `And (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "And")), (self#type_constr _loc _a0))),
                (self#type_constr _loc _a1))
        | `Eq (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Eq")), (self#ctyp _loc _a0))),
                (self#ctyp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method opt_type_constr : 'loc -> opt_type_constr -> Ast.ep=
      fun _loc  ->
        function
        | `Some _a0 ->
            `App (_loc, (`Vrn (_loc, "Some")), (self#type_constr _loc _a0))
        | `None -> `Vrn (_loc, "None")
    method decl_param : 'loc -> decl_param -> Ast.ep=
      fun _loc  ->
        function
        | `Quote (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Quote")),
                     (self#position_flag _loc _a0))),
                (self#alident _loc _a1))
        | `QuoteAny _a0 ->
            `App
              (_loc, (`Vrn (_loc, "QuoteAny")),
                (self#position_flag _loc _a0))
        | `Any -> `Vrn (_loc, "Any")
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method decl_params : 'loc -> decl_params -> Ast.ep=
      fun _loc  ->
        function
        | `Quote (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Quote")),
                     (self#position_flag _loc _a0))),
                (self#alident _loc _a1))
        | `QuoteAny _a0 ->
            `App
              (_loc, (`Vrn (_loc, "QuoteAny")),
                (self#position_flag _loc _a0))
        | `Any -> `Vrn (_loc, "Any")
        | `Com (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Com")), (self#decl_params _loc _a0))),
                (self#decl_params _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method opt_decl_params : 'loc -> opt_decl_params -> Ast.ep=
      fun _loc  ->
        function
        | `Some _a0 ->
            `App (_loc, (`Vrn (_loc, "Some")), (self#decl_params _loc _a0))
        | `None -> `Vrn (_loc, "None")
    method type_info : 'loc -> type_info -> Ast.ep=
      fun _loc  ->
        function
        | `TyMan (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyMan")), (self#ctyp _loc _a0))),
                     (self#flag _loc _a1))), (self#type_repr _loc _a2))
        | `TyRepr (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "TyRepr")), (self#flag _loc _a0))),
                (self#type_repr _loc _a1))
        | `TyEq (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "TyEq")), (self#flag _loc _a0))),
                (self#ctyp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method type_repr : 'loc -> type_repr -> Ast.ep=
      fun _loc  ->
        function
        | `Record _a0 ->
            `App (_loc, (`Vrn (_loc, "Record")), (self#name_ctyp _loc _a0))
        | `Sum _a0 ->
            `App (_loc, (`Vrn (_loc, "Sum")), (self#or_ctyp _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method name_ctyp : 'loc -> name_ctyp -> Ast.ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sem")), (self#name_ctyp _loc _a0))),
                (self#name_ctyp _loc _a1))
        | `TyCol (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "TyCol")), (self#alident _loc _a0))),
                (self#ctyp _loc _a1))
        | `TyColMut (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "TyColMut")), (self#alident _loc _a0))),
                (self#ctyp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method or_ctyp : 'loc -> or_ctyp -> Ast.ep=
      fun _loc  ->
        function
        | `Bar (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Bar")), (self#or_ctyp _loc _a0))),
                (self#or_ctyp _loc _a1))
        | `TyCol (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "TyCol")), (self#auident _loc _a0))),
                (self#ctyp _loc _a1))
        | `Of (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Of")), (self#auident _loc _a0))),
                (self#ctyp _loc _a1))
        | #auident as _a0 -> (self#auident _loc _a0 :>Ast.ep)
    method of_ctyp : 'loc -> of_ctyp -> Ast.ep=
      fun _loc  ->
        function
        | `Of (_a0,_a1) ->
            `App
              (_loc, (`App (_loc, (`Vrn (_loc, "Of")), (self#vid _loc _a0))),
                (self#ctyp _loc _a1))
        | #vid' as _a0 -> (self#vid' _loc _a0 :>Ast.ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method pat : 'loc -> pat -> Ast.ep=
      fun _loc  ->
        function
        | #vid as _a0 -> (self#vid _loc _a0 :>Ast.ep)
        | `App (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "App")), (self#pat _loc _a0))),
                (self#pat _loc _a1))
        | `Vrn _a0 ->
            `App (_loc, (`Vrn (_loc, "Vrn")), (self#string _loc _a0))
        | `Com (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Com")), (self#pat _loc _a0))),
                (self#pat _loc _a1))
        | `Sem (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sem")), (self#pat _loc _a0))),
                (self#pat _loc _a1))
        | `Par _a0 -> `App (_loc, (`Vrn (_loc, "Par")), (self#pat _loc _a0))
        | #any as _a0 -> (self#any _loc _a0 :>Ast.ep)
        | `Record _a0 ->
            `App (_loc, (`Vrn (_loc, "Record")), (self#rec_pat _loc _a0))
        | #literal as _a0 -> (self#literal _loc _a0 :>Ast.ep)
        | `Alias (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Alias")), (self#pat _loc _a0))),
                (self#alident _loc _a1))
        | `ArrayEmpty -> `Vrn (_loc, "ArrayEmpty")
        | `Array _a0 ->
            `App (_loc, (`Vrn (_loc, "Array")), (self#pat _loc _a0))
        | `LabelS _a0 ->
            `App (_loc, (`Vrn (_loc, "LabelS")), (self#alident _loc _a0))
        | `Label (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Label")), (self#alident _loc _a0))),
                (self#pat _loc _a1))
        | `OptLabl (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "OptLabl")), (self#alident _loc _a0))),
                (self#pat _loc _a1))
        | `OptLablS _a0 ->
            `App (_loc, (`Vrn (_loc, "OptLablS")), (self#alident _loc _a0))
        | `OptLablExpr (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "OptLablExpr")),
                          (self#alident _loc _a0))), (self#pat _loc _a1))),
                (self#exp _loc _a2))
        | `Bar (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Bar")), (self#pat _loc _a0))),
                (self#pat _loc _a1))
        | `PaRng (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "PaRng")), (self#pat _loc _a0))),
                (self#pat _loc _a1))
        | `Constraint (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Constraint")), (self#pat _loc _a0))),
                (self#ctyp _loc _a1))
        | `ClassPath _a0 ->
            `App (_loc, (`Vrn (_loc, "ClassPath")), (self#ident _loc _a0))
        | `Lazy _a0 ->
            `App (_loc, (`Vrn (_loc, "Lazy")), (self#pat _loc _a0))
        | `ModuleUnpack _a0 ->
            `App
              (_loc, (`Vrn (_loc, "ModuleUnpack")), (self#auident _loc _a0))
        | `ModuleConstraint (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "ModuleConstraint")),
                     (self#auident _loc _a0))), (self#ctyp _loc _a1))
    method rec_pat : 'loc -> rec_pat -> Ast.ep=
      fun _loc  ->
        function
        | `RecBind (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "RecBind")), (self#ident _loc _a0))),
                (self#pat _loc _a1))
        | `Sem (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sem")), (self#rec_pat _loc _a0))),
                (self#rec_pat _loc _a1))
        | #any as _a0 -> (self#any _loc _a0 :>Ast.ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method exp : 'loc -> exp -> Ast.ep=
      fun _loc  ->
        function
        | #vid as _a0 -> (self#vid _loc _a0 :>Ast.ep)
        | `App (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "App")), (self#exp _loc _a0))),
                (self#exp _loc _a1))
        | `Vrn _a0 ->
            `App (_loc, (`Vrn (_loc, "Vrn")), (self#string _loc _a0))
        | `Com (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Com")), (self#exp _loc _a0))),
                (self#exp _loc _a1))
        | `Sem (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sem")), (self#exp _loc _a0))),
                (self#exp _loc _a1))
        | `Par _a0 -> `App (_loc, (`Vrn (_loc, "Par")), (self#exp _loc _a0))
        | #any as _a0 -> (self#any _loc _a0 :>Ast.ep)
        | `Record _a0 ->
            `App (_loc, (`Vrn (_loc, "Record")), (self#rec_exp _loc _a0))
        | #literal as _a0 -> (self#literal _loc _a0 :>Ast.ep)
        | `RecordWith (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "RecordWith")),
                     (self#rec_exp _loc _a0))), (self#exp _loc _a1))
        | `Field (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Field")), (self#exp _loc _a0))),
                (self#exp _loc _a1))
        | `ArrayDot (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ArrayDot")), (self#exp _loc _a0))),
                (self#exp _loc _a1))
        | `ArrayEmpty -> `Vrn (_loc, "ArrayEmpty")
        | `Array _a0 ->
            `App (_loc, (`Vrn (_loc, "Array")), (self#exp _loc _a0))
        | `Assert _a0 ->
            `App (_loc, (`Vrn (_loc, "Assert")), (self#exp _loc _a0))
        | `Assign (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Assign")), (self#exp _loc _a0))),
                (self#exp _loc _a1))
        | `For (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc, (`Vrn (_loc, "For")),
                                    (self#alident _loc _a0))),
                               (self#exp _loc _a1))), (self#exp _loc _a2))),
                     (self#flag _loc _a3))), (self#exp _loc _a4))
        | `Fun _a0 -> `App (_loc, (`Vrn (_loc, "Fun")), (self#case _loc _a0))
        | `IfThenElse (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "IfThenElse")),
                          (self#exp _loc _a0))), (self#exp _loc _a1))),
                (self#exp _loc _a2))
        | `IfThen (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "IfThen")), (self#exp _loc _a0))),
                (self#exp _loc _a1))
        | `LabelS _a0 ->
            `App (_loc, (`Vrn (_loc, "LabelS")), (self#alident _loc _a0))
        | `Label (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Label")), (self#alident _loc _a0))),
                (self#exp _loc _a1))
        | `Lazy _a0 ->
            `App (_loc, (`Vrn (_loc, "Lazy")), (self#exp _loc _a0))
        | `LetIn (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "LetIn")), (self#flag _loc _a0))),
                     (self#bind _loc _a1))), (self#exp _loc _a2))
        | `LetTryInWith (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "LetTryInWith")),
                               (self#flag _loc _a0))), (self#bind _loc _a1))),
                     (self#exp _loc _a2))), (self#case _loc _a3))
        | `LetModule (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "LetModule")),
                          (self#auident _loc _a0))), (self#mexp _loc _a1))),
                (self#exp _loc _a2))
        | `Match (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Match")), (self#exp _loc _a0))),
                (self#case _loc _a1))
        | `New _a0 ->
            `App (_loc, (`Vrn (_loc, "New")), (self#ident _loc _a0))
        | `Obj _a0 ->
            `App (_loc, (`Vrn (_loc, "Obj")), (self#clfield _loc _a0))
        | `ObjEnd -> `Vrn (_loc, "ObjEnd")
        | `ObjPat (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ObjPat")), (self#pat _loc _a0))),
                (self#clfield _loc _a1))
        | `ObjPatEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "ObjPatEnd")), (self#pat _loc _a0))
        | `OptLabl (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "OptLabl")), (self#alident _loc _a0))),
                (self#exp _loc _a1))
        | `OptLablS _a0 ->
            `App (_loc, (`Vrn (_loc, "OptLablS")), (self#alident _loc _a0))
        | `OvrInst _a0 ->
            `App (_loc, (`Vrn (_loc, "OvrInst")), (self#rec_exp _loc _a0))
        | `OvrInstEmpty -> `Vrn (_loc, "OvrInstEmpty")
        | `Seq _a0 -> `App (_loc, (`Vrn (_loc, "Seq")), (self#exp _loc _a0))
        | `Send (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Send")), (self#exp _loc _a0))),
                (self#alident _loc _a1))
        | `StringDot (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "StringDot")), (self#exp _loc _a0))),
                (self#exp _loc _a1))
        | `Try (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Try")), (self#exp _loc _a0))),
                (self#case _loc _a1))
        | `Constraint (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Constraint")), (self#exp _loc _a0))),
                (self#ctyp _loc _a1))
        | `Coercion (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Coercion")),
                          (self#exp _loc _a0))), (self#ctyp _loc _a1))),
                (self#ctyp _loc _a2))
        | `Subtype (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Subtype")), (self#exp _loc _a0))),
                (self#ctyp _loc _a1))
        | `While (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "While")), (self#exp _loc _a0))),
                (self#exp _loc _a1))
        | `LetOpen (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "LetOpen")), (self#ident _loc _a0))),
                (self#exp _loc _a1))
        | `LocalTypeFun (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "LocalTypeFun")),
                     (self#alident _loc _a0))), (self#exp _loc _a1))
        | `Package_exp _a0 ->
            `App (_loc, (`Vrn (_loc, "Package_exp")), (self#mexp _loc _a0))
    method rec_exp : 'loc -> rec_exp -> Ast.ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sem")), (self#rec_exp _loc _a0))),
                (self#rec_exp _loc _a1))
        | `RecBind (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "RecBind")), (self#ident _loc _a0))),
                (self#exp _loc _a1))
        | #any as _a0 -> (self#any _loc _a0 :>Ast.ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method mtyp : 'loc -> mtyp -> Ast.ep=
      fun _loc  ->
        function
        | #ident' as _a0 -> (self#ident' _loc _a0 :>Ast.ep)
        | `Sig _a0 -> `App (_loc, (`Vrn (_loc, "Sig")), (self#sigi _loc _a0))
        | `SigEnd -> `Vrn (_loc, "SigEnd")
        | `Functor (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Functor")),
                          (self#auident _loc _a0))), (self#mtyp _loc _a1))),
                (self#mtyp _loc _a2))
        | `With (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "With")), (self#mtyp _loc _a0))),
                (self#constr _loc _a1))
        | `ModuleTypeOf _a0 ->
            `App (_loc, (`Vrn (_loc, "ModuleTypeOf")), (self#mexp _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method sigi : 'loc -> sigi -> Ast.ep=
      fun _loc  ->
        function
        | `Val (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Val")), (self#alident _loc _a0))),
                (self#ctyp _loc _a1))
        | `External (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "External")),
                          (self#alident _loc _a0))), (self#ctyp _loc _a1))),
                (self#strings _loc _a2))
        | `Type _a0 ->
            `App (_loc, (`Vrn (_loc, "Type")), (self#typedecl _loc _a0))
        | `Exception _a0 ->
            `App (_loc, (`Vrn (_loc, "Exception")), (self#of_ctyp _loc _a0))
        | `Class _a0 ->
            `App (_loc, (`Vrn (_loc, "Class")), (self#cltdecl _loc _a0))
        | `ClassType _a0 ->
            `App (_loc, (`Vrn (_loc, "ClassType")), (self#cltdecl _loc _a0))
        | `Module (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Module")), (self#auident _loc _a0))),
                (self#mtyp _loc _a1))
        | `ModuleTypeEnd _a0 ->
            `App
              (_loc, (`Vrn (_loc, "ModuleTypeEnd")), (self#auident _loc _a0))
        | `ModuleType (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "ModuleType")),
                     (self#auident _loc _a0))), (self#mtyp _loc _a1))
        | `Sem (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sem")), (self#sigi _loc _a0))),
                (self#sigi _loc _a1))
        | `DirectiveSimple _a0 ->
            `App
              (_loc, (`Vrn (_loc, "DirectiveSimple")),
                (self#alident _loc _a0))
        | `Directive (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Directive")),
                     (self#alident _loc _a0))), (self#exp _loc _a1))
        | `Open _a0 ->
            `App (_loc, (`Vrn (_loc, "Open")), (self#ident _loc _a0))
        | `Include _a0 ->
            `App (_loc, (`Vrn (_loc, "Include")), (self#mtyp _loc _a0))
        | `RecModule _a0 ->
            `App (_loc, (`Vrn (_loc, "RecModule")), (self#mbind _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method mbind : 'loc -> mbind -> Ast.ep=
      fun _loc  ->
        function
        | `And (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "And")), (self#mbind _loc _a0))),
                (self#mbind _loc _a1))
        | `ModuleBind (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ModuleBind")),
                          (self#auident _loc _a0))), (self#mtyp _loc _a1))),
                (self#mexp _loc _a2))
        | `Constraint (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Constraint")),
                     (self#auident _loc _a0))), (self#mtyp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method constr : 'loc -> constr -> Ast.ep=
      fun _loc  ->
        function
        | `TypeEq (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "TypeEq")), (self#ctyp _loc _a0))),
                (self#ctyp _loc _a1))
        | `ModuleEq (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "ModuleEq")), (self#ident _loc _a0))),
                (self#ident _loc _a1))
        | `TypeEqPriv (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "TypeEqPriv")), (self#ctyp _loc _a0))),
                (self#ctyp _loc _a1))
        | `TypeSubst (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "TypeSubst")), (self#ctyp _loc _a0))),
                (self#ctyp _loc _a1))
        | `ModuleSubst (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "ModuleSubst")),
                     (self#ident _loc _a0))), (self#ident _loc _a1))
        | `And (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "And")), (self#constr _loc _a0))),
                (self#constr _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method bind : 'loc -> bind -> Ast.ep=
      fun _loc  ->
        function
        | `And (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "And")), (self#bind _loc _a0))),
                (self#bind _loc _a1))
        | `Bind (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Bind")), (self#pat _loc _a0))),
                (self#exp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method case : 'loc -> case -> Ast.ep=
      fun _loc  ->
        function
        | `Bar (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Bar")), (self#case _loc _a0))),
                (self#case _loc _a1))
        | `Case (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Case")), (self#pat _loc _a0))),
                (self#exp _loc _a1))
        | `CaseWhen (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "CaseWhen")),
                          (self#pat _loc _a0))), (self#exp _loc _a1))),
                (self#exp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method mexp : 'loc -> mexp -> Ast.ep=
      fun _loc  ->
        function
        | #vid' as _a0 -> (self#vid' _loc _a0 :>Ast.ep)
        | `App (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "App")), (self#mexp _loc _a0))),
                (self#mexp _loc _a1))
        | `Functor (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Functor")),
                          (self#auident _loc _a0))), (self#mtyp _loc _a1))),
                (self#mexp _loc _a2))
        | `Struct _a0 ->
            `App (_loc, (`Vrn (_loc, "Struct")), (self#stru _loc _a0))
        | `StructEnd -> `Vrn (_loc, "StructEnd")
        | `Constraint (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Constraint")), (self#mexp _loc _a0))),
                (self#mtyp _loc _a1))
        | `PackageModule _a0 ->
            `App (_loc, (`Vrn (_loc, "PackageModule")), (self#exp _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method stru : 'loc -> stru -> Ast.ep=
      fun _loc  ->
        function
        | `Class _a0 ->
            `App (_loc, (`Vrn (_loc, "Class")), (self#cldecl _loc _a0))
        | `ClassType _a0 ->
            `App (_loc, (`Vrn (_loc, "ClassType")), (self#cltdecl _loc _a0))
        | `Sem (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sem")), (self#stru _loc _a0))),
                (self#stru _loc _a1))
        | `DirectiveSimple _a0 ->
            `App
              (_loc, (`Vrn (_loc, "DirectiveSimple")),
                (self#alident _loc _a0))
        | `Directive (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Directive")),
                     (self#alident _loc _a0))), (self#exp _loc _a1))
        | `Exception _a0 ->
            `App (_loc, (`Vrn (_loc, "Exception")), (self#of_ctyp _loc _a0))
        | `StExp _a0 ->
            `App (_loc, (`Vrn (_loc, "StExp")), (self#exp _loc _a0))
        | `External (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "External")),
                          (self#alident _loc _a0))), (self#ctyp _loc _a1))),
                (self#strings _loc _a2))
        | `Include _a0 ->
            `App (_loc, (`Vrn (_loc, "Include")), (self#mexp _loc _a0))
        | `Module (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Module")), (self#auident _loc _a0))),
                (self#mexp _loc _a1))
        | `RecModule _a0 ->
            `App (_loc, (`Vrn (_loc, "RecModule")), (self#mbind _loc _a0))
        | `ModuleType (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "ModuleType")),
                     (self#auident _loc _a0))), (self#mtyp _loc _a1))
        | `Open _a0 ->
            `App (_loc, (`Vrn (_loc, "Open")), (self#ident _loc _a0))
        | `Type _a0 ->
            `App (_loc, (`Vrn (_loc, "Type")), (self#typedecl _loc _a0))
        | `TypeWith (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "TypeWith")),
                     (self#typedecl _loc _a0))), (self#strings _loc _a1))
        | `Value (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Value")), (self#flag _loc _a0))),
                (self#bind _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method cltdecl : 'loc -> cltdecl -> Ast.ep=
      fun _loc  ->
        function
        | `And (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "And")), (self#cltdecl _loc _a0))),
                (self#cltdecl _loc _a1))
        | `CtDecl (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "CtDecl")),
                               (self#flag _loc _a0))), (self#ident _loc _a1))),
                     (self#type_parameters _loc _a2))),
                (self#cltyp _loc _a3))
        | `CtDeclS (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "CtDeclS")),
                          (self#flag _loc _a0))), (self#ident _loc _a1))),
                (self#cltyp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method cltyp : 'loc -> cltyp -> Ast.ep=
      fun _loc  ->
        function
        | #vid' as _a0 -> (self#vid' _loc _a0 :>Ast.ep)
        | `ClApply (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ClApply")), (self#vid _loc _a0))),
                (self#type_parameters _loc _a1))
        | `CtFun (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "CtFun")), (self#ctyp _loc _a0))),
                (self#cltyp _loc _a1))
        | `ObjTy (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ObjTy")), (self#ctyp _loc _a0))),
                (self#clsigi _loc _a1))
        | `ObjTyEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "ObjTyEnd")), (self#ctyp _loc _a0))
        | `Obj _a0 ->
            `App (_loc, (`Vrn (_loc, "Obj")), (self#clsigi _loc _a0))
        | `ObjEnd -> `Vrn (_loc, "ObjEnd")
        | `And (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "And")), (self#cltyp _loc _a0))),
                (self#cltyp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method clsigi : 'loc -> clsigi -> Ast.ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sem")), (self#clsigi _loc _a0))),
                (self#clsigi _loc _a1))
        | `SigInherit _a0 ->
            `App (_loc, (`Vrn (_loc, "SigInherit")), (self#cltyp _loc _a0))
        | `CgVal (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "CgVal")),
                               (self#alident _loc _a0))),
                          (self#flag _loc _a1))), (self#flag _loc _a2))),
                (self#ctyp _loc _a3))
        | `Method (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Method")),
                          (self#alident _loc _a0))), (self#flag _loc _a1))),
                (self#ctyp _loc _a2))
        | `VirMeth (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "VirMeth")),
                          (self#alident _loc _a0))), (self#flag _loc _a1))),
                (self#ctyp _loc _a2))
        | `Eq (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Eq")), (self#ctyp _loc _a0))),
                (self#ctyp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method cldecl : 'loc -> cldecl -> Ast.ep=
      fun _loc  ->
        function
        | `ClDecl (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "ClDecl")),
                               (self#flag _loc _a0))), (self#ident _loc _a1))),
                     (self#type_parameters _loc _a2))),
                (self#clexp _loc _a3))
        | `ClDeclS (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ClDeclS")),
                          (self#flag _loc _a0))), (self#ident _loc _a1))),
                (self#clexp _loc _a2))
        | `And (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "And")), (self#cldecl _loc _a0))),
                (self#cldecl _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method clexp : 'loc -> clexp -> Ast.ep=
      fun _loc  ->
        function
        | `CeApp (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "CeApp")), (self#clexp _loc _a0))),
                (self#exp _loc _a1))
        | #vid' as _a0 -> (self#vid' _loc _a0 :>Ast.ep)
        | `ClApply (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ClApply")), (self#vid _loc _a0))),
                (self#type_parameters _loc _a1))
        | `CeFun (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "CeFun")), (self#pat _loc _a0))),
                (self#clexp _loc _a1))
        | `LetIn (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "LetIn")), (self#flag _loc _a0))),
                     (self#bind _loc _a1))), (self#clexp _loc _a2))
        | `Obj _a0 ->
            `App (_loc, (`Vrn (_loc, "Obj")), (self#clfield _loc _a0))
        | `ObjEnd -> `Vrn (_loc, "ObjEnd")
        | `ObjPat (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ObjPat")), (self#pat _loc _a0))),
                (self#clfield _loc _a1))
        | `ObjPatEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "ObjPatEnd")), (self#pat _loc _a0))
        | `Constraint (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Constraint")), (self#clexp _loc _a0))),
                (self#cltyp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method clfield : 'loc -> clfield -> Ast.ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sem")), (self#clfield _loc _a0))),
                (self#clfield _loc _a1))
        | `Inherit (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Inherit")), (self#flag _loc _a0))),
                (self#clexp _loc _a1))
        | `InheritAs (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "InheritAs")),
                          (self#flag _loc _a0))), (self#clexp _loc _a1))),
                (self#alident _loc _a2))
        | `CrVal (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "CrVal")),
                               (self#alident _loc _a0))),
                          (self#flag _loc _a1))), (self#flag _loc _a2))),
                (self#exp _loc _a3))
        | `VirVal (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "VirVal")),
                          (self#alident _loc _a0))), (self#flag _loc _a1))),
                (self#ctyp _loc _a2))
        | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc, (`Vrn (_loc, "CrMth")),
                                    (self#alident _loc _a0))),
                               (self#flag _loc _a1))), (self#flag _loc _a2))),
                     (self#exp _loc _a3))), (self#ctyp _loc _a4))
        | `CrMthS (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "CrMthS")),
                               (self#alident _loc _a0))),
                          (self#flag _loc _a1))), (self#flag _loc _a2))),
                (self#exp _loc _a3))
        | `VirMeth (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "VirMeth")),
                          (self#alident _loc _a0))), (self#flag _loc _a1))),
                (self#ctyp _loc _a2))
        | `Eq (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Eq")), (self#ctyp _loc _a0))),
                (self#ctyp _loc _a1))
        | `Initializer _a0 ->
            `App (_loc, (`Vrn (_loc, "Initializer")), (self#exp _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
    method ep : 'loc -> ep -> Ast.ep=
      fun _loc  ->
        function
        | #vid as _a0 -> (self#vid _loc _a0 :>Ast.ep)
        | `App (_a0,_a1) ->
            `App
              (_loc, (`App (_loc, (`Vrn (_loc, "App")), (self#ep _loc _a0))),
                (self#ep _loc _a1))
        | `Vrn _a0 ->
            `App (_loc, (`Vrn (_loc, "Vrn")), (self#string _loc _a0))
        | `Com (_a0,_a1) ->
            `App
              (_loc, (`App (_loc, (`Vrn (_loc, "Com")), (self#ep _loc _a0))),
                (self#ep _loc _a1))
        | `Sem (_a0,_a1) ->
            `App
              (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (self#ep _loc _a0))),
                (self#ep _loc _a1))
        | `Par _a0 -> `App (_loc, (`Vrn (_loc, "Par")), (self#ep _loc _a0))
        | #any as _a0 -> (self#any _loc _a0 :>Ast.ep)
        | `ArrayEmpty -> `Vrn (_loc, "ArrayEmpty")
        | `Array _a0 ->
            `App (_loc, (`Vrn (_loc, "Array")), (self#ep _loc _a0))
        | `Record _a0 ->
            `App (_loc, (`Vrn (_loc, "Record")), (self#rec_bind _loc _a0))
        | #literal as _a0 -> (self#literal _loc _a0 :>Ast.ep)
    method rec_bind : 'loc -> rec_bind -> Ast.ep=
      fun _loc  ->
        function
        | `RecBind (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "RecBind")), (self#ident _loc _a0))),
                (self#ep _loc _a1))
        | `Sem (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sem")), (self#rec_bind _loc _a0))),
                (self#rec_bind _loc _a1))
        | #any as _a0 -> (self#any _loc _a0 :>Ast.ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>Ast.ep)
  end