open Ast
let strip_loc_list f lst =  List.map f lst 

let strip_loc_ant: ant -> 'result182 = fun (`Ant (_a0,_a1))  -> `Ant _a1
let strip_loc_literal: literal -> 'result183 =
  function
  | `Chr (_a0,_a1) -> `Chr _a1
  | `Int (_a0,_a1) -> `Int _a1
  | `Int32 (_a0,_a1) -> `Int32 _a1
  | `Int64 (_a0,_a1) -> `Int64 _a1
  | `Flo (_a0,_a1) -> `Flo _a1
  | `NativeInt (_a0,_a1) -> `NativeInt _a1
  | `Str (_a0,_a1) -> `Str _a1
let strip_loc_rec_flag: rec_flag -> 'result184 =
  function
  | `Recursive _a0 -> `Recursive
  | `ReNil _a0 -> `ReNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result184)
let strip_loc_direction_flag: direction_flag -> 'result185 =
  function
  | `To _a0 -> `To
  | `Downto _a0 -> `Downto
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result185)
let strip_loc_mutable_flag: mutable_flag -> 'result186 =
  function
  | `Mutable _a0 -> `Mutable
  | `MuNil _a0 -> `MuNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result186)
let strip_loc_private_flag: private_flag -> 'result187 =
  function
  | `Private _a0 -> `Private
  | `PrNil _a0 -> `PrNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result187)
let strip_loc_virtual_flag: virtual_flag -> 'result188 =
  function
  | `Virtual _a0 -> `Virtual
  | `ViNil _a0 -> `ViNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result188)
let strip_loc_override_flag: override_flag -> 'result189 =
  function
  | `Override _a0 -> `Override
  | `OvNil _a0 -> `OvNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result189)
let strip_loc_row_var_flag: row_var_flag -> 'result190 =
  function
  | `RowVar _a0 -> `RowVar
  | `RvNil _a0 -> `RvNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result190)
let strip_loc_position_flag: position_flag -> 'result191 =
  function
  | `Positive _a0 -> `Positive
  | `Negative _a0 -> `Negative
  | `Normal _a0 -> `Normal
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result191)
let strip_loc_meta_bool: meta_bool -> 'result192 =
  function
  | `True _a0 -> `True
  | `False _a0 -> `False
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result192)
let strip_loc_meta_option (* : *)
  (* 'all_a0 . ('all_a0 -> 'result193) -> 'all_a0 meta_option -> 'result193 *)=
  fun mf_a  ->
    function
    | `None _a0 -> `None
    | `Some (_a0,_a1) -> let _a1 = mf_a _a1 in `Some _a1
    | #ant as _a0 -> (strip_loc_ant _a0 :>'result193)
let rec strip_loc_meta_list :
  'all_a0 . ('all_a0 -> 'result194) -> 'all_a0 meta_list -> 'result194=
  fun mf_a  ->
    function
    | `LNil _a0 -> `LNil
    | `LCons (_a0,_a1,_a2) ->
        let _a1 = mf_a _a1 in
        let _a2 = strip_loc_meta_list mf_a _a2 in `LCons (_a1, _a2)
    | #ant as _a0 -> (strip_loc_ant _a0 :>'result194)
let strip_loc_alident: alident -> 'result195 =
  function
  | `Lid (_a0,_a1) -> `Lid _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result195)
let strip_loc_auident: auident -> 'result196 =
  function
  | `Uid (_a0,_a1) -> `Uid _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result196)
let strip_loc_aident: aident -> 'result197 =
  function
  | #alident as _a0 -> (strip_loc_alident _a0 :>'result197)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result197)
let strip_loc_astring: astring -> 'result198 =
  function
  | `C (_a0,_a1) -> `C _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result198)
let rec strip_loc_ident: ident -> 'result199 =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `Dot (_a1, _a2)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `App (_a1, _a2)
  | #alident as _a0 -> (strip_loc_alident _a0 :>'result199)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result199)
let rec strip_loc_ctyp: ctyp -> 'result215 =
  function
  | `Nil _a0 -> `Nil
  | `Alias (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Alias (_a1, _a2)
  | `Any _a0 -> `Any
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
  | `Id (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `Id _a1
  | `TyMan (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyMan (_a1, _a2)
  | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_list strip_loc_ctyp _a2 in
      let _a3 = strip_loc_ctyp _a3 in
      let _a4 =
        strip_loc_list
          (fun (_a0,_a1)  ->
             let _a0 = strip_loc_ctyp _a0 in
             let _a1 = strip_loc_ctyp _a1 in (_a0, _a1)) _a4 in
      `TyDcl (_a1, _a2, _a3, _a4)
  | `TyObj (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_row_var_flag _a2 in `TyObj (_a1, _a2)
  | `TyOlb (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyOlb (_a1, _a2)
  | `TyPol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyPol (_a1, _a2)
  | `TyTypePol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyTypePol (_a1, _a2)
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = strip_loc_position_flag _a1 in
      let _a2 = strip_loc_meta_option strip_loc_alident _a2 in
      `Quote (_a1, _a2)
  | `TyRec (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `TyRec _a1
  | `TyCol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyCol (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Sem (_a1, _a2)
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Com (_a1, _a2)
  | `Sum (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Sum _a1
  | `Of (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Of (_a1, _a2)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `And (_a1, _a2)
  | `Or (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Or (_a1, _a2)
  | `Priv (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Priv _a1
  | `Mut (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Mut _a1
  | `Tup (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Tup _a1
  | `Sta (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Sta (_a1, _a2)
  | `TyVrn (_a0,_a1) -> let _a1 = strip_loc_astring _a1 in `TyVrn _a1
  | `TyVrnEq (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `TyVrnEq _a1
  | `TyVrnSup (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `TyVrnSup _a1
  | `TyVrnInf (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `TyVrnInf _a1
  | `TyVrnInfSup (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyVrnInfSup (_a1, _a2)
  | `Amp (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Amp (_a1, _a2)
  | `TyOfAmp (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyOfAmp (_a1, _a2)
  | `Package (_a0,_a1) -> let _a1 = strip_loc_module_type _a1 in `Package _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result215)
and strip_loc_patt: patt -> 'result214 =
  function
  | `Nil _a0 -> `Nil
  | `Id (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `Id _a1
  | `Alias (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_alident _a2 in `Alias (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result214)
  | `Any _a0 -> `Any
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_patt _a2 in `App (_a1, _a2)
  | `Array (_a0,_a1) -> let _a1 = strip_loc_patt _a1 in `Array _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_patt _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_patt _a2 in `Sem (_a1, _a2)
  | #literal as _a0 -> (strip_loc_literal _a0 :>'result214)
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_patt _a2 in `Label (_a1, _a2)
  | `PaOlbi (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_patt _a2 in
      let _a3 = strip_loc_meta_option strip_loc_expr _a3 in
      `PaOlbi (_a1, _a2, _a3)
  | `Or (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_patt _a2 in `Or (_a1, _a2)
  | `PaRng (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_patt _a2 in `PaRng (_a1, _a2)
  | `PaRec (_a0,_a1) -> let _a1 = strip_loc_patt _a1 in `PaRec _a1
  | `PaEq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_patt _a2 in `PaEq (_a1, _a2)
  | `Tup (_a0,_a1) -> let _a1 = strip_loc_patt _a1 in `Tup _a1
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Constraint (_a1, _a2)
  | `ClassPath (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `ClassPath _a1
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Lazy (_a0,_a1) -> let _a1 = strip_loc_patt _a1 in `Lazy _a1
  | `ModuleUnpack (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_meta_option strip_loc_ctyp _a2 in
      `ModuleUnpack (_a1, _a2)
and strip_loc_expr: expr -> 'result213 =
  function
  | `Nil _a0 -> `Nil
  | `Id (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `Id _a1
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `Dot (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result213)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `App (_a1, _a2)
  | `ArrayDot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `ArrayDot (_a1, _a2)
  | `Array (_a0,_a1) -> let _a1 = strip_loc_expr _a1 in `Array _a1
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `Sem (_a1, _a2)
  | `ExAsf _a0 -> `ExAsf
  | `ExAsr (_a0,_a1) -> let _a1 = strip_loc_expr _a1 in `ExAsr _a1
  | `Assign (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `Assign (_a1, _a2)
  | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_expr _a2 in
      let _a3 = strip_loc_expr _a3 in
      let _a4 = strip_loc_direction_flag _a4 in
      let _a5 = strip_loc_expr _a5 in `For (_a1, _a2, _a3, _a4, _a5)
  | `Fun (_a0,_a1) -> let _a1 = strip_loc_match_case _a1 in `Fun _a1
  | `IfThenElse (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in
      let _a3 = strip_loc_expr _a3 in `IfThenElse (_a1, _a2, _a3)
  | `IfThen (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `IfThen (_a1, _a2)
  | #literal as _a0 -> (strip_loc_literal _a0 :>'result213)
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_expr _a2 in `Label (_a1, _a2)
  | `Lazy (_a0,_a1) -> let _a1 = strip_loc_expr _a1 in `Lazy _a1
  | `LetIn (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_rec_flag _a1 in
      let _a2 = strip_loc_binding _a2 in
      let _a3 = strip_loc_expr _a3 in `LetIn (_a1, _a2, _a3)
  | `LetModule (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_expr _a2 in
      let _a3 = strip_loc_expr _a3 in `LetModule (_a1, _a2, _a3)
  | `Match (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_match_case _a2 in `Match (_a1, _a2)
  | `New (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `New _a1
  | `Obj (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_class_str_item _a2 in `Obj (_a1, _a2)
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_expr _a2 in `OptLabl (_a1, _a2)
  | `OvrInst (_a0,_a1) -> let _a1 = strip_loc_rec_binding _a1 in `OvrInst _a1
  | `Record (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_binding _a1 in
      let _a2 = strip_loc_expr _a2 in `Record (_a1, _a2)
  | `Seq (_a0,_a1) -> let _a1 = strip_loc_expr _a1 in `Seq _a1
  | `Send (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_alident _a2 in `Send (_a1, _a2)
  | `StringDot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `StringDot (_a1, _a2)
  | `Try (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_match_case _a2 in `Try (_a1, _a2)
  | `Tup (_a0,_a1) -> let _a1 = strip_loc_expr _a1 in `Tup _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `Com (_a1, _a2)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Constraint (_a1, _a2)
  | `Coercion (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_ctyp _a2 in
      let _a3 = strip_loc_ctyp _a3 in `Coercion (_a1, _a2, _a3)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `While (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `While (_a1, _a2)
  | `LetOpen (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_expr _a2 in `LetOpen (_a1, _a2)
  | `LocalTypeFun (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_expr _a2 in `LocalTypeFun (_a1, _a2)
  | `Package_expr (_a0,_a1) ->
      let _a1 = strip_loc_module_expr _a1 in `Package_expr _a1
and strip_loc_module_type: module_type -> 'result212 =
  function
  | `Nil _a0 -> `Nil
  | `Id (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `Id _a1
  | `MtFun (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in
      let _a3 = strip_loc_module_type _a3 in `MtFun (_a1, _a2, _a3)
  | `Sig (_a0,_a1) -> let _a1 = strip_loc_sig_item _a1 in `Sig _a1
  | `With (_a0,_a1,_a2) ->
      let _a1 = strip_loc_module_type _a1 in
      let _a2 = strip_loc_with_constr _a2 in `With (_a1, _a2)
  | `ModuleTypeOf (_a0,_a1) ->
      let _a1 = strip_loc_module_expr _a1 in `ModuleTypeOf _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result212)
and strip_loc_sig_item: sig_item -> 'result211 =
  function
  | `Nil _a0 -> `Nil
  | `Class (_a0,_a1) -> let _a1 = strip_loc_class_type _a1 in `Class _a1
  | `ClassType (_a0,_a1) ->
      let _a1 = strip_loc_class_type _a1 in `ClassType _a1
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_sig_item _a1 in
      let _a2 = strip_loc_sig_item _a2 in `Sem (_a1, _a2)
  | `Directive (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_expr _a2 in `Directive (_a1, _a2)
  | `Exception (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Exception _a1
  | `External (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `External (_a1, _a2, _a3)
  | `Include (_a0,_a1) -> let _a1 = strip_loc_module_type _a1 in `Include _a1
  | `Module (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in `Module (_a1, _a2)
  | `RecModule (_a0,_a1) ->
      let _a1 = strip_loc_module_binding _a1 in `RecModule _a1
  | `ModuleType (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in `ModuleType (_a1, _a2)
  | `Open (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `Open _a1
  | `Type (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Type _a1
  | `Val (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Val (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result211)
and strip_loc_with_constr: with_constr -> 'result210 =
  function
  | `Nil _a0 -> `Nil
  | `TypeEq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TypeEq (_a1, _a2)
  | `ModuleEq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `ModuleEq (_a1, _a2)
  | `TypeSubst (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TypeSubst (_a1, _a2)
  | `ModuleSubst (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `ModuleSubst (_a1, _a2)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_with_constr _a1 in
      let _a2 = strip_loc_with_constr _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result210)
and strip_loc_binding: binding -> 'result209 =
  function
  | `Nil _a0 -> `Nil
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_binding _a1 in
      let _a2 = strip_loc_binding _a2 in `And (_a1, _a2)
  | `Bind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_expr _a2 in `Bind (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result209)
and strip_loc_rec_binding: rec_binding -> 'result208 =
  function
  | `Nil _a0 -> `Nil
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_binding _a1 in
      let _a2 = strip_loc_rec_binding _a2 in `Sem (_a1, _a2)
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_expr _a2 in `RecBind (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result208)
and strip_loc_module_binding: module_binding -> 'result207 =
  function
  | `Nil _a0 -> `Nil
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_module_binding _a1 in
      let _a2 = strip_loc_module_binding _a2 in `And (_a1, _a2)
  | `ModuleBind (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in
      let _a3 = strip_loc_module_expr _a3 in `ModuleBind (_a1, _a2, _a3)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in `Constraint (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result207)
and strip_loc_match_case: match_case -> 'result206 =
  function
  | `Nil _a0 -> `Nil
  | `Or (_a0,_a1,_a2) ->
      let _a1 = strip_loc_match_case _a1 in
      let _a2 = strip_loc_match_case _a2 in `Or (_a1, _a2)
  | `Case (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_expr _a2 in
      let _a3 = strip_loc_expr _a3 in `Case (_a1, _a2, _a3)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result206)
and strip_loc_module_expr: module_expr -> 'result205 =
  function
  | `Nil _a0 -> `Nil
  | `Id (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `Id _a1
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_module_expr _a1 in
      let _a2 = strip_loc_module_expr _a2 in `App (_a1, _a2)
  | `Functor (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in
      let _a3 = strip_loc_module_expr _a3 in `Functor (_a1, _a2, _a3)
  | `Struct (_a0,_a1) -> let _a1 = strip_loc_str_item _a1 in `Struct _a1
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_module_expr _a1 in
      let _a2 = strip_loc_module_type _a2 in `Constraint (_a1, _a2)
  | `PackageModule (_a0,_a1) ->
      let _a1 = strip_loc_expr _a1 in `PackageModule _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result205)
and strip_loc_str_item: str_item -> 'result204 =
  function
  | `Nil _a0 -> `Nil
  | `Class (_a0,_a1) -> let _a1 = strip_loc_class_expr _a1 in `Class _a1
  | `ClassType (_a0,_a1) ->
      let _a1 = strip_loc_class_type _a1 in `ClassType _a1
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_str_item _a1 in
      let _a2 = strip_loc_str_item _a2 in `Sem (_a1, _a2)
  | `Directive (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_expr _a2 in `Directive (_a1, _a2)
  | `Exception (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Exception _a1
  | `StExp (_a0,_a1) -> let _a1 = strip_loc_expr _a1 in `StExp _a1
  | `External (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `External (_a1, _a2, _a3)
  | `Include (_a0,_a1) -> let _a1 = strip_loc_module_expr _a1 in `Include _a1
  | `Module (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_expr _a2 in `Module (_a1, _a2)
  | `RecModule (_a0,_a1) ->
      let _a1 = strip_loc_module_binding _a1 in `RecModule _a1
  | `ModuleType (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in `ModuleType (_a1, _a2)
  | `Open (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `Open _a1
  | `Type (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Type _a1
  | `Value (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_flag _a1 in
      let _a2 = strip_loc_binding _a2 in `Value (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result204)
and strip_loc_class_type: class_type -> 'result203 =
  function
  | `Nil _a0 -> `Nil
  | `CtCon (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_virtual_flag _a1 in
      let _a2 = strip_loc_ident _a2 in
      let _a3 = strip_loc_ctyp _a3 in `CtCon (_a1, _a2, _a3)
  | `CtFun (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_class_type _a2 in `CtFun (_a1, _a2)
  | `CtSig (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_class_sig_item _a2 in `CtSig (_a1, _a2)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_type _a1 in
      let _a2 = strip_loc_class_type _a2 in `And (_a1, _a2)
  | `CtCol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_type _a1 in
      let _a2 = strip_loc_class_type _a2 in `CtCol (_a1, _a2)
  | `CtEq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_type _a1 in
      let _a2 = strip_loc_class_type _a2 in `CtEq (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result203)
and strip_loc_class_sig_item: class_sig_item -> 'result202 =
  function
  | `Nil _a0 -> `Nil
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Eq (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_sig_item _a1 in
      let _a2 = strip_loc_class_sig_item _a2 in `Sem (_a1, _a2)
  | `SigInherit (_a0,_a1) ->
      let _a1 = strip_loc_class_type _a1 in `SigInherit _a1
  | `Method (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_private_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `Method (_a1, _a2, _a3)
  | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_mutable_flag _a2 in
      let _a3 = strip_loc_virtual_flag _a3 in
      let _a4 = strip_loc_ctyp _a4 in `CgVal (_a1, _a2, _a3, _a4)
  | `CgVir (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_private_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `CgVir (_a1, _a2, _a3)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result202)
and strip_loc_class_expr: class_expr -> 'result201 =
  function
  | `Nil _a0 -> `Nil
  | `CeApp (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `CeApp (_a1, _a2)
  | `CeCon (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_virtual_flag _a1 in
      let _a2 = strip_loc_ident _a2 in
      let _a3 = strip_loc_ctyp _a3 in `CeCon (_a1, _a2, _a3)
  | `CeFun (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_class_expr _a2 in `CeFun (_a1, _a2)
  | `CeLet (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_rec_flag _a1 in
      let _a2 = strip_loc_binding _a2 in
      let _a3 = strip_loc_class_expr _a3 in `CeLet (_a1, _a2, _a3)
  | `Obj (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_class_str_item _a2 in `Obj (_a1, _a2)
  | `CeTyc (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_expr _a1 in
      let _a2 = strip_loc_class_type _a2 in `CeTyc (_a1, _a2)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_expr _a1 in
      let _a2 = strip_loc_class_expr _a2 in `And (_a1, _a2)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_expr _a1 in
      let _a2 = strip_loc_class_expr _a2 in `Eq (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result201)
and strip_loc_class_str_item: class_str_item -> 'result200 =
  function
  | `Nil _a0 -> `Nil
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_str_item _a1 in
      let _a2 = strip_loc_class_str_item _a2 in `Sem (_a1, _a2)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Eq (_a1, _a2)
  | `Inherit (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_override_flag _a1 in
      let _a2 = strip_loc_class_expr _a2 in
      let _a3 = strip_loc_meta_option strip_loc_alident _a3 in
      `Inherit (_a1, _a2, _a3)
  | `Initializer (_a0,_a1) ->
      let _a1 = strip_loc_expr _a1 in `Initializer _a1
  | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_override_flag _a2 in
      let _a3 = strip_loc_private_flag _a3 in
      let _a4 = strip_loc_expr _a4 in
      let _a5 = strip_loc_ctyp _a5 in `CrMth (_a1, _a2, _a3, _a4, _a5)
  | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_override_flag _a2 in
      let _a3 = strip_loc_mutable_flag _a3 in
      let _a4 = strip_loc_expr _a4 in `CrVal (_a1, _a2, _a3, _a4)
  | `CrVir (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_private_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `CrVir (_a1, _a2, _a3)
  | `CrVvr (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_mutable_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `CrVvr (_a1, _a2, _a3)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result200)



















