include AstN

let fill_loc_ant _loc x = x

let _ = (); ()

let _ = ()

let fill_loc_nil loc `Nil = `Nil loc

let fill_loc_literal loc =
  function
  | `Chr _a0 -> `Chr (loc, _a0)
  | `Int _a0 -> `Int (loc, _a0)
  | `Int32 _a0 -> `Int32 (loc, _a0)
  | `Int64 _a0 -> `Int64 (loc, _a0)
  | `Flo _a0 -> `Flo (loc, _a0)
  | `Nativeint _a0 -> `Nativeint (loc, _a0)
  | `Str _a0 -> `Str (loc, _a0)

let fill_loc_rec_flag loc =
  function
  | `Recursive -> `Recursive loc
  | `ReNil -> `ReNil loc
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result2)

let fill_loc_direction_flag loc =
  function
  | `To -> `To loc
  | `Downto -> `Downto loc
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result3)

let fill_loc_mutable_flag loc =
  function
  | `Mutable -> `Mutable loc
  | `MuNil -> `MuNil loc
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result4)

let fill_loc_private_flag loc =
  function
  | `Private -> `Private loc
  | `PrNil -> `PrNil loc
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result5)

let fill_loc_virtual_flag loc =
  function
  | `Virtual -> `Virtual loc
  | `ViNil -> `ViNil loc
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result6)

let fill_loc_override_flag loc =
  function
  | `Override -> `Override loc
  | `OvNil -> `OvNil loc
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result7)

let fill_loc_row_var_flag loc =
  function
  | `RowVar -> `RowVar loc
  | `RvNil -> `RvNil loc
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result8)

let fill_loc_position_flag loc =
  function
  | `Positive -> `Positive loc
  | `Negative -> `Negative loc
  | `Normal -> `Normal loc
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result9)

let rec fill_loc_strings loc =
  function
  | `App (_a0,_a1) ->
      let _a0 = fill_loc_strings loc _a0 in
      let _a1 = fill_loc_strings loc _a1 in `App (loc, _a0, _a1)
  | `Str _a0 -> `Str (loc, _a0)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result10)

let fill_loc_lident loc (`Lid _a0) = `Lid (loc, _a0)

let fill_loc_alident loc =
  function
  | `Lid _a0 -> `Lid (loc, _a0)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result12)

let fill_loc_auident loc =
  function
  | `Uid _a0 -> `Uid (loc, _a0)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result13)

let fill_loc_aident loc =
  function
  | #alident as _a0 -> (fill_loc_alident loc _a0 :>'result14)
  | #auident as _a0 -> (fill_loc_auident loc _a0 :>'result14)

let fill_loc_astring loc =
  function
  | `C _a0 -> `C (loc, _a0)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result15)

let rec fill_loc_uident loc =
  function
  | `Dot (_a0,_a1) ->
      let _a0 = fill_loc_uident loc _a0 in
      let _a1 = fill_loc_uident loc _a1 in `Dot (loc, _a0, _a1)
  | `App (_a0,_a1) ->
      let _a0 = fill_loc_uident loc _a0 in
      let _a1 = fill_loc_uident loc _a1 in `App (loc, _a0, _a1)
  | #auident as _a0 -> (fill_loc_auident loc _a0 :>'result16)

let rec fill_loc_ident loc =
  function
  | `Dot (_a0,_a1) ->
      let _a0 = fill_loc_ident loc _a0 in
      let _a1 = fill_loc_ident loc _a1 in `Dot (loc, _a0, _a1)
  | `Apply (_a0,_a1) ->
      let _a0 = fill_loc_ident loc _a0 in
      let _a1 = fill_loc_ident loc _a1 in `Apply (loc, _a0, _a1)
  | #alident as _a0 -> (fill_loc_alident loc _a0 :>'result17)
  | #auident as _a0 -> (fill_loc_auident loc _a0 :>'result17)

let fill_loc_ident' loc =
  function
  | `Dot (_a0,_a1) ->
      let _a0 = fill_loc_ident loc _a0 in
      let _a1 = fill_loc_ident loc _a1 in `Dot (loc, _a0, _a1)
  | `Apply (_a0,_a1) ->
      let _a0 = fill_loc_ident loc _a0 in
      let _a1 = fill_loc_ident loc _a1 in `Apply (loc, _a0, _a1)
  | `Lid _a0 -> `Lid (loc, _a0)
  | `Uid _a0 -> `Uid (loc, _a0)

let rec fill_loc_vid loc =
  function
  | `Dot (_a0,_a1) ->
      let _a0 = fill_loc_vid loc _a0 in
      let _a1 = fill_loc_vid loc _a1 in `Dot (loc, _a0, _a1)
  | `Lid _a0 -> `Lid (loc, _a0)
  | `Uid _a0 -> `Uid (loc, _a0)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result19)

let fill_loc_vid' loc =
  function
  | `Dot (_a0,_a1) ->
      let _a0 = fill_loc_vid loc _a0 in
      let _a1 = fill_loc_vid loc _a1 in `Dot (loc, _a0, _a1)
  | `Lid _a0 -> `Lid (loc, _a0)
  | `Uid _a0 -> `Uid (loc, _a0)

let rec fill_loc_dupath loc =
  function
  | `Dot (_a0,_a1) ->
      let _a0 = fill_loc_dupath loc _a0 in
      let _a1 = fill_loc_dupath loc _a1 in `Dot (loc, _a0, _a1)
  | #auident as _a0 -> (fill_loc_auident loc _a0 :>'result21)

let fill_loc_dlpath loc =
  function
  | `Dot (_a0,_a1) ->
      let _a0 = fill_loc_dupath loc _a0 in
      let _a1 = fill_loc_alident loc _a1 in `Dot (loc, _a0, _a1)
  | #alident as _a0 -> (fill_loc_alident loc _a0 :>'result22)

let fill_loc_any loc `Any = `Any loc

let rec fill_loc_ctyp loc =
  function
  | `Alias (_a0,_a1) ->
      let _a0 = fill_loc_ctyp loc _a0 in
      let _a1 = fill_loc_alident loc _a1 in `Alias (loc, _a0, _a1)
  | #any as _a0 -> (fill_loc_any loc _a0 :>'result56)
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
  | #ident' as _a0 -> (fill_loc_ident' loc _a0 :>'result56)
  | `TyObj (_a0,_a1) ->
      let _a0 = fill_loc_name_ctyp loc _a0 in
      let _a1 = fill_loc_row_var_flag loc _a1 in `TyObj (loc, _a0, _a1)
  | `TyObjEnd _a0 ->
      let _a0 = fill_loc_row_var_flag loc _a0 in `TyObjEnd (loc, _a0)
  | `TyPol (_a0,_a1) ->
      let _a0 = fill_loc_ctyp loc _a0 in
      let _a1 = fill_loc_ctyp loc _a1 in `TyPol (loc, _a0, _a1)
  | `TyPolEnd _a0 -> let _a0 = fill_loc_ctyp loc _a0 in `TyPolEnd (loc, _a0)
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
  | `PolyEq _a0 -> let _a0 = fill_loc_row_field loc _a0 in `PolyEq (loc, _a0)
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
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result56)
and fill_loc_type_parameters loc =
  function
  | `Com (_a0,_a1) ->
      let _a0 = fill_loc_type_parameters loc _a0 in
      let _a1 = fill_loc_type_parameters loc _a1 in `Com (loc, _a0, _a1)
  | `Ctyp _a0 -> let _a0 = fill_loc_ctyp loc _a0 in `Ctyp (loc, _a0)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result55)
and fill_loc_row_field loc =
  function
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result54)
  | `Bar (_a0,_a1) ->
      let _a0 = fill_loc_row_field loc _a0 in
      let _a1 = fill_loc_row_field loc _a1 in `Bar (loc, _a0, _a1)
  | `TyVrn _a0 -> let _a0 = fill_loc_astring loc _a0 in `TyVrn (loc, _a0)
  | `TyVrnOf (_a0,_a1) ->
      let _a0 = fill_loc_astring loc _a0 in
      let _a1 = fill_loc_ctyp loc _a1 in `TyVrnOf (loc, _a0, _a1)
  | `Ctyp _a0 -> let _a0 = fill_loc_ctyp loc _a0 in `Ctyp (loc, _a0)
and fill_loc_tag_names loc =
  function
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result53)
  | `App (_a0,_a1) ->
      let _a0 = fill_loc_tag_names loc _a0 in
      let _a1 = fill_loc_tag_names loc _a1 in `App (loc, _a0, _a1)
  | `TyVrn _a0 -> let _a0 = fill_loc_astring loc _a0 in `TyVrn (loc, _a0)
and fill_loc_typedecl loc =
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
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result52)
and fill_loc_type_constr loc =
  function
  | `And (_a0,_a1) ->
      let _a0 = fill_loc_type_constr loc _a0 in
      let _a1 = fill_loc_type_constr loc _a1 in `And (loc, _a0, _a1)
  | `Eq (_a0,_a1) ->
      let _a0 = fill_loc_ctyp loc _a0 in
      let _a1 = fill_loc_ctyp loc _a1 in `Eq (loc, _a0, _a1)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result51)
and fill_loc_opt_type_constr loc =
  function
  | `Some _a0 -> let _a0 = fill_loc_type_constr loc _a0 in `Some (loc, _a0)
  | `None -> `None loc
and fill_loc_decl_param loc =
  function
  | `Quote (_a0,_a1) ->
      let _a0 = fill_loc_position_flag loc _a0 in
      let _a1 = fill_loc_alident loc _a1 in `Quote (loc, _a0, _a1)
  | `QuoteAny _a0 ->
      let _a0 = fill_loc_position_flag loc _a0 in `QuoteAny (loc, _a0)
  | `Any -> `Any loc
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result49)
and fill_loc_decl_params loc =
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
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result48)
and fill_loc_opt_decl_params loc =
  function
  | `Some _a0 -> let _a0 = fill_loc_decl_params loc _a0 in `Some (loc, _a0)
  | `None -> `None loc
and fill_loc_type_info loc =
  function
  | `TyMan (_a0,_a1,_a2) ->
      let _a0 = fill_loc_ctyp loc _a0 in
      let _a1 = fill_loc_private_flag loc _a1 in
      let _a2 = fill_loc_type_repr loc _a2 in `TyMan (loc, _a0, _a1, _a2)
  | `TyRepr (_a0,_a1) ->
      let _a0 = fill_loc_private_flag loc _a0 in
      let _a1 = fill_loc_type_repr loc _a1 in `TyRepr (loc, _a0, _a1)
  | `TyEq (_a0,_a1) ->
      let _a0 = fill_loc_private_flag loc _a0 in
      let _a1 = fill_loc_ctyp loc _a1 in `TyEq (loc, _a0, _a1)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result46)
and fill_loc_type_repr loc =
  function
  | `Record _a0 -> let _a0 = fill_loc_name_ctyp loc _a0 in `Record (loc, _a0)
  | `Sum _a0 -> let _a0 = fill_loc_or_ctyp loc _a0 in `Sum (loc, _a0)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result45)
and fill_loc_name_ctyp loc =
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
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result44)
and fill_loc_or_ctyp loc =
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
  | #auident as _a0 -> (fill_loc_auident loc _a0 :>'result43)
and fill_loc_of_ctyp loc =
  function
  | `Of (_a0,_a1) ->
      let _a0 = fill_loc_vid loc _a0 in
      let _a1 = fill_loc_ctyp loc _a1 in `Of (loc, _a0, _a1)
  | #vid' as _a0 -> (fill_loc_vid' loc _a0 :>'result42)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result42)
and fill_loc_pat loc =
  function
  | #vid as _a0 -> (fill_loc_vid loc _a0 :>'result41)
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
  | #any as _a0 -> (fill_loc_any loc _a0 :>'result41)
  | `Record _a0 -> let _a0 = fill_loc_rec_pat loc _a0 in `Record (loc, _a0)
  | #literal as _a0 -> (fill_loc_literal loc _a0 :>'result41)
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
and fill_loc_rec_pat loc =
  function
  | `RecBind (_a0,_a1) ->
      let _a0 = fill_loc_ident loc _a0 in
      let _a1 = fill_loc_pat loc _a1 in `RecBind (loc, _a0, _a1)
  | `Sem (_a0,_a1) ->
      let _a0 = fill_loc_rec_pat loc _a0 in
      let _a1 = fill_loc_rec_pat loc _a1 in `Sem (loc, _a0, _a1)
  | #any as _a0 -> (fill_loc_any loc _a0 :>'result40)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result40)
and fill_loc_exp loc =
  function
  | #vid as _a0 -> (fill_loc_vid loc _a0 :>'result39)
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
  | #any as _a0 -> (fill_loc_any loc _a0 :>'result39)
  | `Record _a0 -> let _a0 = fill_loc_rec_exp loc _a0 in `Record (loc, _a0)
  | #literal as _a0 -> (fill_loc_literal loc _a0 :>'result39)
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
      let _a3 = fill_loc_direction_flag loc _a3 in
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
      let _a0 = fill_loc_rec_flag loc _a0 in
      let _a1 = fill_loc_bind loc _a1 in
      let _a2 = fill_loc_exp loc _a2 in `LetIn (loc, _a0, _a1, _a2)
  | `LetTryInWith (_a0,_a1,_a2,_a3) ->
      let _a0 = fill_loc_rec_flag loc _a0 in
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
  | `ObjPatEnd _a0 -> let _a0 = fill_loc_pat loc _a0 in `ObjPatEnd (loc, _a0)
  | `OptLabl (_a0,_a1) ->
      let _a0 = fill_loc_alident loc _a0 in
      let _a1 = fill_loc_exp loc _a1 in `OptLabl (loc, _a0, _a1)
  | `OptLablS _a0 ->
      let _a0 = fill_loc_alident loc _a0 in `OptLablS (loc, _a0)
  | `OvrInst _a0 -> let _a0 = fill_loc_rec_exp loc _a0 in `OvrInst (loc, _a0)
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
and fill_loc_rec_exp loc =
  function
  | `Sem (_a0,_a1) ->
      let _a0 = fill_loc_rec_exp loc _a0 in
      let _a1 = fill_loc_rec_exp loc _a1 in `Sem (loc, _a0, _a1)
  | `RecBind (_a0,_a1) ->
      let _a0 = fill_loc_ident loc _a0 in
      let _a1 = fill_loc_exp loc _a1 in `RecBind (loc, _a0, _a1)
  | #any as _a0 -> (fill_loc_any loc _a0 :>'result38)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result38)
and fill_loc_mtyp loc =
  function
  | #ident' as _a0 -> (fill_loc_ident' loc _a0 :>'result37)
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
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result37)
and fill_loc_sigi loc =
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
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result36)
and fill_loc_mbind loc =
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
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result35)
and fill_loc_constr loc =
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
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result34)
and fill_loc_bind loc =
  function
  | `And (_a0,_a1) ->
      let _a0 = fill_loc_bind loc _a0 in
      let _a1 = fill_loc_bind loc _a1 in `And (loc, _a0, _a1)
  | `Bind (_a0,_a1) ->
      let _a0 = fill_loc_pat loc _a0 in
      let _a1 = fill_loc_exp loc _a1 in `Bind (loc, _a0, _a1)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result33)
and fill_loc_case loc =
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
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result32)
and fill_loc_mexp loc =
  function
  | #vid' as _a0 -> (fill_loc_vid' loc _a0 :>'result31)
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
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result31)
and fill_loc_stru loc =
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
  | `Value (_a0,_a1) ->
      let _a0 = fill_loc_rec_flag loc _a0 in
      let _a1 = fill_loc_bind loc _a1 in `Value (loc, _a0, _a1)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result30)
and fill_loc_cltdecl loc =
  function
  | `And (_a0,_a1) ->
      let _a0 = fill_loc_cltdecl loc _a0 in
      let _a1 = fill_loc_cltdecl loc _a1 in `And (loc, _a0, _a1)
  | `CtDecl (_a0,_a1,_a2,_a3) ->
      let _a0 = fill_loc_virtual_flag loc _a0 in
      let _a1 = fill_loc_ident loc _a1 in
      let _a2 = fill_loc_type_parameters loc _a2 in
      let _a3 = fill_loc_cltyp loc _a3 in `CtDecl (loc, _a0, _a1, _a2, _a3)
  | `CtDeclS (_a0,_a1,_a2) ->
      let _a0 = fill_loc_virtual_flag loc _a0 in
      let _a1 = fill_loc_ident loc _a1 in
      let _a2 = fill_loc_cltyp loc _a2 in `CtDeclS (loc, _a0, _a1, _a2)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result29)
and fill_loc_cltyp loc =
  function
  | #vid' as _a0 -> (fill_loc_vid' loc _a0 :>'result28)
  | `ClApply (_a0,_a1) ->
      let _a0 = fill_loc_vid loc _a0 in
      let _a1 = fill_loc_type_parameters loc _a1 in `ClApply (loc, _a0, _a1)
  | `CtFun (_a0,_a1) ->
      let _a0 = fill_loc_ctyp loc _a0 in
      let _a1 = fill_loc_cltyp loc _a1 in `CtFun (loc, _a0, _a1)
  | `ObjTy (_a0,_a1) ->
      let _a0 = fill_loc_ctyp loc _a0 in
      let _a1 = fill_loc_clsigi loc _a1 in `ObjTy (loc, _a0, _a1)
  | `ObjTyEnd _a0 -> let _a0 = fill_loc_ctyp loc _a0 in `ObjTyEnd (loc, _a0)
  | `Obj _a0 -> let _a0 = fill_loc_clsigi loc _a0 in `Obj (loc, _a0)
  | `ObjEnd -> `ObjEnd loc
  | `And (_a0,_a1) ->
      let _a0 = fill_loc_cltyp loc _a0 in
      let _a1 = fill_loc_cltyp loc _a1 in `And (loc, _a0, _a1)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result28)
and fill_loc_clsigi loc =
  function
  | `Sem (_a0,_a1) ->
      let _a0 = fill_loc_clsigi loc _a0 in
      let _a1 = fill_loc_clsigi loc _a1 in `Sem (loc, _a0, _a1)
  | `SigInherit _a0 ->
      let _a0 = fill_loc_cltyp loc _a0 in `SigInherit (loc, _a0)
  | `CgVal (_a0,_a1,_a2,_a3) ->
      let _a0 = fill_loc_alident loc _a0 in
      let _a1 = fill_loc_mutable_flag loc _a1 in
      let _a2 = fill_loc_virtual_flag loc _a2 in
      let _a3 = fill_loc_ctyp loc _a3 in `CgVal (loc, _a0, _a1, _a2, _a3)
  | `Method (_a0,_a1,_a2) ->
      let _a0 = fill_loc_alident loc _a0 in
      let _a1 = fill_loc_private_flag loc _a1 in
      let _a2 = fill_loc_ctyp loc _a2 in `Method (loc, _a0, _a1, _a2)
  | `VirMeth (_a0,_a1,_a2) ->
      let _a0 = fill_loc_alident loc _a0 in
      let _a1 = fill_loc_private_flag loc _a1 in
      let _a2 = fill_loc_ctyp loc _a2 in `VirMeth (loc, _a0, _a1, _a2)
  | `Eq (_a0,_a1) ->
      let _a0 = fill_loc_ctyp loc _a0 in
      let _a1 = fill_loc_ctyp loc _a1 in `Eq (loc, _a0, _a1)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result27)
and fill_loc_cldecl loc =
  function
  | `ClDecl (_a0,_a1,_a2,_a3) ->
      let _a0 = fill_loc_virtual_flag loc _a0 in
      let _a1 = fill_loc_ident loc _a1 in
      let _a2 = fill_loc_type_parameters loc _a2 in
      let _a3 = fill_loc_clexp loc _a3 in `ClDecl (loc, _a0, _a1, _a2, _a3)
  | `ClDeclS (_a0,_a1,_a2) ->
      let _a0 = fill_loc_virtual_flag loc _a0 in
      let _a1 = fill_loc_ident loc _a1 in
      let _a2 = fill_loc_clexp loc _a2 in `ClDeclS (loc, _a0, _a1, _a2)
  | `And (_a0,_a1) ->
      let _a0 = fill_loc_cldecl loc _a0 in
      let _a1 = fill_loc_cldecl loc _a1 in `And (loc, _a0, _a1)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result26)
and fill_loc_clexp loc =
  function
  | `CeApp (_a0,_a1) ->
      let _a0 = fill_loc_clexp loc _a0 in
      let _a1 = fill_loc_exp loc _a1 in `CeApp (loc, _a0, _a1)
  | #vid' as _a0 -> (fill_loc_vid' loc _a0 :>'result25)
  | `ClApply (_a0,_a1) ->
      let _a0 = fill_loc_vid loc _a0 in
      let _a1 = fill_loc_type_parameters loc _a1 in `ClApply (loc, _a0, _a1)
  | `CeFun (_a0,_a1) ->
      let _a0 = fill_loc_pat loc _a0 in
      let _a1 = fill_loc_clexp loc _a1 in `CeFun (loc, _a0, _a1)
  | `LetIn (_a0,_a1,_a2) ->
      let _a0 = fill_loc_rec_flag loc _a0 in
      let _a1 = fill_loc_bind loc _a1 in
      let _a2 = fill_loc_clexp loc _a2 in `LetIn (loc, _a0, _a1, _a2)
  | `Obj _a0 -> let _a0 = fill_loc_clfield loc _a0 in `Obj (loc, _a0)
  | `ObjEnd -> `ObjEnd loc
  | `ObjPat (_a0,_a1) ->
      let _a0 = fill_loc_pat loc _a0 in
      let _a1 = fill_loc_clfield loc _a1 in `ObjPat (loc, _a0, _a1)
  | `ObjPatEnd _a0 -> let _a0 = fill_loc_pat loc _a0 in `ObjPatEnd (loc, _a0)
  | `Constraint (_a0,_a1) ->
      let _a0 = fill_loc_clexp loc _a0 in
      let _a1 = fill_loc_cltyp loc _a1 in `Constraint (loc, _a0, _a1)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result25)
and fill_loc_clfield loc =
  function
  | `Sem (_a0,_a1) ->
      let _a0 = fill_loc_clfield loc _a0 in
      let _a1 = fill_loc_clfield loc _a1 in `Sem (loc, _a0, _a1)
  | `Inherit (_a0,_a1) ->
      let _a0 = fill_loc_override_flag loc _a0 in
      let _a1 = fill_loc_clexp loc _a1 in `Inherit (loc, _a0, _a1)
  | `InheritAs (_a0,_a1,_a2) ->
      let _a0 = fill_loc_override_flag loc _a0 in
      let _a1 = fill_loc_clexp loc _a1 in
      let _a2 = fill_loc_alident loc _a2 in `InheritAs (loc, _a0, _a1, _a2)
  | `CrVal (_a0,_a1,_a2,_a3) ->
      let _a0 = fill_loc_alident loc _a0 in
      let _a1 = fill_loc_override_flag loc _a1 in
      let _a2 = fill_loc_mutable_flag loc _a2 in
      let _a3 = fill_loc_exp loc _a3 in `CrVal (loc, _a0, _a1, _a2, _a3)
  | `VirVal (_a0,_a1,_a2) ->
      let _a0 = fill_loc_alident loc _a0 in
      let _a1 = fill_loc_mutable_flag loc _a1 in
      let _a2 = fill_loc_ctyp loc _a2 in `VirVal (loc, _a0, _a1, _a2)
  | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
      let _a0 = fill_loc_alident loc _a0 in
      let _a1 = fill_loc_override_flag loc _a1 in
      let _a2 = fill_loc_private_flag loc _a2 in
      let _a3 = fill_loc_exp loc _a3 in
      let _a4 = fill_loc_ctyp loc _a4 in
      `CrMth (loc, _a0, _a1, _a2, _a3, _a4)
  | `CrMthS (_a0,_a1,_a2,_a3) ->
      let _a0 = fill_loc_alident loc _a0 in
      let _a1 = fill_loc_override_flag loc _a1 in
      let _a2 = fill_loc_private_flag loc _a2 in
      let _a3 = fill_loc_exp loc _a3 in `CrMthS (loc, _a0, _a1, _a2, _a3)
  | `VirMeth (_a0,_a1,_a2) ->
      let _a0 = fill_loc_alident loc _a0 in
      let _a1 = fill_loc_private_flag loc _a1 in
      let _a2 = fill_loc_ctyp loc _a2 in `VirMeth (loc, _a0, _a1, _a2)
  | `Eq (_a0,_a1) ->
      let _a0 = fill_loc_ctyp loc _a0 in
      let _a1 = fill_loc_ctyp loc _a1 in `Eq (loc, _a0, _a1)
  | `Initializer _a0 ->
      let _a0 = fill_loc_exp loc _a0 in `Initializer (loc, _a0)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result24)

let rec fill_loc_ep loc =
  function
  | #vid as _a0 -> (fill_loc_vid loc _a0 :>'result58)
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
  | #any as _a0 -> (fill_loc_any loc _a0 :>'result58)
  | `ArrayEmpty -> `ArrayEmpty loc
  | `Array _a0 -> let _a0 = fill_loc_ep loc _a0 in `Array (loc, _a0)
  | `Record _a0 -> let _a0 = fill_loc_rec_bind loc _a0 in `Record (loc, _a0)
  | #literal as _a0 -> (fill_loc_literal loc _a0 :>'result58)
and fill_loc_rec_bind loc =
  function
  | `RecBind (_a0,_a1) ->
      let _a0 = fill_loc_ident loc _a0 in
      let _a1 = fill_loc_ep loc _a1 in `RecBind (loc, _a0, _a1)
  | `Sem (_a0,_a1) ->
      let _a0 = fill_loc_rec_bind loc _a0 in
      let _a1 = fill_loc_rec_bind loc _a1 in `Sem (loc, _a0, _a1)
  | #any as _a0 -> (fill_loc_any loc _a0 :>'result57)
  | #ant as _a0 -> (fill_loc_ant loc _a0 :>'result57)