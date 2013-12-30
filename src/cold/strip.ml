open Astf
let ant x = x
let literal: Astf.literal -> Astfn.literal =
  function
  | `Chr (_a0,_a1) -> `Chr _a1
  | `Int (_a0,_a1) -> `Int _a1
  | `Int32 (_a0,_a1) -> `Int32 _a1
  | `Int64 (_a0,_a1) -> `Int64 _a1
  | `Flo (_a0,_a1) -> `Flo _a1
  | `Nativeint (_a0,_a1) -> `Nativeint _a1
  | `Str (_a0,_a1) -> `Str _a1
  | `Bool (_a0,_a1) -> `Bool _a1
  | `Unit _a0 -> `Unit
let flag: Astf.flag -> Astfn.flag =
  function
  | `Positive _a0 -> `Positive
  | `Negative _a0 -> `Negative
  | #ant as _a0 -> (ant _a0 :>Astfn.flag)
let position_flag: Astf.position_flag -> Astfn.position_flag =
  function
  | `Positive _a0 -> `Positive
  | `Negative _a0 -> `Negative
  | `Normal _a0 -> `Normal
  | #ant as _a0 -> (ant _a0 :>Astfn.position_flag)
let rec strings: Astf.strings -> Astfn.strings =
  function
  | `App (_a0,_a1,_a2) ->
      let _a1 = strings _a1 in let _a2 = strings _a2 in `App (_a1, _a2)
  | `Str (_a0,_a1) -> `Str _a1
  | #ant as _a0 -> (ant _a0 :>Astfn.strings)
let lident: Astf.lident -> Astfn.lident = fun (`Lid (_a0,_a1))  -> `Lid _a1
let alident: Astf.alident -> Astfn.alident =
  function
  | `Lid (_a0,_a1) -> `Lid _a1
  | #ant as _a0 -> (ant _a0 :>Astfn.alident)
let auident: Astf.auident -> Astfn.auident =
  function
  | `Uid (_a0,_a1) -> `Uid _a1
  | #ant as _a0 -> (ant _a0 :>Astfn.auident)
let aident: Astf.aident -> Astfn.aident =
  function
  | #alident as _a0 -> (alident _a0 :>Astfn.aident)
  | #auident as _a0 -> (auident _a0 :>Astfn.aident)
let astring: Astf.astring -> Astfn.astring =
  function
  | `C (_a0,_a1) -> `C _a1
  | #ant as _a0 -> (ant _a0 :>Astfn.astring)
let rec uident: Astf.uident -> Astfn.uident =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = uident _a1 in let _a2 = uident _a2 in `Dot (_a1, _a2)
  | `App (_a0,_a1,_a2) ->
      let _a1 = uident _a1 in let _a2 = uident _a2 in `App (_a1, _a2)
  | #auident as _a0 -> (auident _a0 :>Astfn.uident)
let rec ident: Astf.ident -> Astfn.ident =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = ident _a1 in let _a2 = ident _a2 in `Dot (_a1, _a2)
  | `Apply (_a0,_a1,_a2) ->
      let _a1 = ident _a1 in let _a2 = ident _a2 in `Apply (_a1, _a2)
  | #alident as _a0 -> (alident _a0 :>Astfn.ident)
  | #auident as _a0 -> (auident _a0 :>Astfn.ident)
let ident': Astf.ident' -> Astfn.ident' =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = ident _a1 in let _a2 = ident _a2 in `Dot (_a1, _a2)
  | `Apply (_a0,_a1,_a2) ->
      let _a1 = ident _a1 in let _a2 = ident _a2 in `Apply (_a1, _a2)
  | `Lid (_a0,_a1) -> `Lid _a1
  | `Uid (_a0,_a1) -> `Uid _a1
let rec vid: Astf.vid -> Astfn.vid =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = vid _a1 in let _a2 = vid _a2 in `Dot (_a1, _a2)
  | `Lid (_a0,_a1) -> `Lid _a1
  | `Uid (_a0,_a1) -> `Uid _a1
  | #ant as _a0 -> (ant _a0 :>Astfn.vid)
let vid': Astf.vid' -> Astfn.vid' =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = vid _a1 in let _a2 = vid _a2 in `Dot (_a1, _a2)
  | `Lid (_a0,_a1) -> `Lid _a1
  | `Uid (_a0,_a1) -> `Uid _a1
let rec dupath: Astf.dupath -> Astfn.dupath =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = dupath _a1 in let _a2 = dupath _a2 in `Dot (_a1, _a2)
  | #auident as _a0 -> (auident _a0 :>Astfn.dupath)
let dlpath: Astf.dlpath -> Astfn.dlpath =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = dupath _a1 in let _a2 = alident _a2 in `Dot (_a1, _a2)
  | #alident as _a0 -> (alident _a0 :>Astfn.dlpath)
let any: Astf.any -> Astfn.any = fun (`Any _a0)  -> `Any
let rec ctyp: Astf.ctyp -> Astfn.ctyp =
  function
  | `Alias (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = alident _a2 in `Alias (_a1, _a2)
  | #any as _a0 -> (any _a0 :>Astfn.ctyp)
  | `App (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = ctyp _a2 in `App (_a1, _a2)
  | `Arrow (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = ctyp _a2 in `Arrow (_a1, _a2)
  | `ClassPath (_a0,_a1) -> let _a1 = ident _a1 in `ClassPath _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = alident _a1 in let _a2 = ctyp _a2 in `Label (_a1, _a2)
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = alident _a1 in let _a2 = ctyp _a2 in `OptLabl (_a1, _a2)
  | #ident' as _a0 -> (ident' _a0 :>Astfn.ctyp)
  | `TyObj (_a0,_a1,_a2) ->
      let _a1 = name_ctyp _a1 in let _a2 = flag _a2 in `TyObj (_a1, _a2)
  | `TyObjEnd (_a0,_a1) -> let _a1 = flag _a1 in `TyObjEnd _a1
  | `TyPol (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = ctyp _a2 in `TyPol (_a1, _a2)
  | `TyPolEnd (_a0,_a1) -> let _a1 = ctyp _a1 in `TyPolEnd _a1
  | `TyTypePol (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = ctyp _a2 in `TyTypePol (_a1, _a2)
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = position_flag _a1 in
      let _a2 = alident _a2 in `Quote (_a1, _a2)
  | `QuoteAny (_a0,_a1) -> let _a1 = position_flag _a1 in `QuoteAny _a1
  | `Par (_a0,_a1) -> let _a1 = ctyp _a1 in `Par _a1
  | `Sta (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = ctyp _a2 in `Sta (_a1, _a2)
  | `PolyEq (_a0,_a1) -> let _a1 = row_field _a1 in `PolyEq _a1
  | `PolySup (_a0,_a1) -> let _a1 = row_field _a1 in `PolySup _a1
  | `PolyInf (_a0,_a1) -> let _a1 = row_field _a1 in `PolyInf _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = ctyp _a2 in `Com (_a1, _a2)
  | `PolyInfSup (_a0,_a1,_a2) ->
      let _a1 = row_field _a1 in
      let _a2 = tag_names _a2 in `PolyInfSup (_a1, _a2)
  | `Package (_a0,_a1) -> let _a1 = mtyp _a1 in `Package _a1
  | #ant as _a0 -> (ant _a0 :>Astfn.ctyp)
and type_parameters: Astf.type_parameters -> Astfn.type_parameters =
  function
  | `Com (_a0,_a1,_a2) ->
      let _a1 = type_parameters _a1 in
      let _a2 = type_parameters _a2 in `Com (_a1, _a2)
  | `Ctyp (_a0,_a1) -> let _a1 = ctyp _a1 in `Ctyp _a1
  | #ant as _a0 -> (ant _a0 :>Astfn.type_parameters)
and row_field: Astf.row_field -> Astfn.row_field =
  function
  | #ant as _a0 -> (ant _a0 :>Astfn.row_field)
  | `Bar (_a0,_a1,_a2) ->
      let _a1 = row_field _a1 in let _a2 = row_field _a2 in `Bar (_a1, _a2)
  | `TyVrn (_a0,_a1) -> let _a1 = astring _a1 in `TyVrn _a1
  | `TyVrnOf (_a0,_a1,_a2) ->
      let _a1 = astring _a1 in let _a2 = ctyp _a2 in `TyVrnOf (_a1, _a2)
  | `Ctyp (_a0,_a1) -> let _a1 = ctyp _a1 in `Ctyp _a1
and tag_names: Astf.tag_names -> Astfn.tag_names =
  function
  | #ant as _a0 -> (ant _a0 :>Astfn.tag_names)
  | `App (_a0,_a1,_a2) ->
      let _a1 = tag_names _a1 in let _a2 = tag_names _a2 in `App (_a1, _a2)
  | `TyVrn (_a0,_a1) -> let _a1 = astring _a1 in `TyVrn _a1
and decl: Astf.decl -> Astfn.decl =
  function
  | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = alident _a1 in
      let _a2 = opt_decl_params _a2 in
      let _a3 = type_info _a3 in
      let _a4 = opt_type_constr _a4 in `TyDcl (_a1, _a2, _a3, _a4)
  | `TyAbstr (_a0,_a1,_a2,_a3) ->
      let _a1 = alident _a1 in
      let _a2 = opt_decl_params _a2 in
      let _a3 = opt_type_constr _a3 in `TyAbstr (_a1, _a2, _a3)
  | `And (_a0,_a1,_a2) ->
      let _a1 = decl _a1 in let _a2 = decl _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.decl)
and type_constr: Astf.type_constr -> Astfn.type_constr =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = type_constr _a1 in
      let _a2 = type_constr _a2 in `And (_a1, _a2)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = ctyp _a2 in `Eq (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.type_constr)
and opt_type_constr: Astf.opt_type_constr -> Astfn.opt_type_constr =
  function
  | `Some (_a0,_a1) -> let _a1 = type_constr _a1 in `Some _a1
  | `None _a0 -> `None
and decl_param: Astf.decl_param -> Astfn.decl_param =
  function
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = position_flag _a1 in
      let _a2 = alident _a2 in `Quote (_a1, _a2)
  | `QuoteAny (_a0,_a1) -> let _a1 = position_flag _a1 in `QuoteAny _a1
  | `Any _a0 -> `Any
  | #ant as _a0 -> (ant _a0 :>Astfn.decl_param)
and decl_params: Astf.decl_params -> Astfn.decl_params =
  function
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = position_flag _a1 in
      let _a2 = alident _a2 in `Quote (_a1, _a2)
  | `QuoteAny (_a0,_a1) -> let _a1 = position_flag _a1 in `QuoteAny _a1
  | `Any _a0 -> `Any
  | `Com (_a0,_a1,_a2) ->
      let _a1 = decl_params _a1 in
      let _a2 = decl_params _a2 in `Com (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.decl_params)
and opt_decl_params: Astf.opt_decl_params -> Astfn.opt_decl_params =
  function
  | `Some (_a0,_a1) -> let _a1 = decl_params _a1 in `Some _a1
  | `None _a0 -> `None
and type_info: Astf.type_info -> Astfn.type_info =
  function
  | `TyMan (_a0,_a1,_a2,_a3) ->
      let _a1 = ctyp _a1 in
      let _a2 = flag _a2 in let _a3 = type_repr _a3 in `TyMan (_a1, _a2, _a3)
  | `TyRepr (_a0,_a1,_a2) ->
      let _a1 = flag _a1 in let _a2 = type_repr _a2 in `TyRepr (_a1, _a2)
  | `TyEq (_a0,_a1,_a2) ->
      let _a1 = flag _a1 in let _a2 = ctyp _a2 in `TyEq (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.type_info)
and type_repr: Astf.type_repr -> Astfn.type_repr =
  function
  | `Record (_a0,_a1) -> let _a1 = name_ctyp _a1 in `Record _a1
  | `Sum (_a0,_a1) -> let _a1 = or_ctyp _a1 in `Sum _a1
  | #ant as _a0 -> (ant _a0 :>Astfn.type_repr)
and name_ctyp: Astf.name_ctyp -> Astfn.name_ctyp =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = name_ctyp _a1 in let _a2 = name_ctyp _a2 in `Sem (_a1, _a2)
  | `TyCol (_a0,_a1,_a2) ->
      let _a1 = alident _a1 in let _a2 = ctyp _a2 in `TyCol (_a1, _a2)
  | `TyColMut (_a0,_a1,_a2) ->
      let _a1 = alident _a1 in let _a2 = ctyp _a2 in `TyColMut (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.name_ctyp)
and or_ctyp: Astf.or_ctyp -> Astfn.or_ctyp =
  function
  | `Bar (_a0,_a1,_a2) ->
      let _a1 = or_ctyp _a1 in let _a2 = or_ctyp _a2 in `Bar (_a1, _a2)
  | `TyCol (_a0,_a1,_a2) ->
      let _a1 = auident _a1 in let _a2 = ctyp _a2 in `TyCol (_a1, _a2)
  | `Of (_a0,_a1,_a2) ->
      let _a1 = auident _a1 in let _a2 = ctyp _a2 in `Of (_a1, _a2)
  | #auident as _a0 -> (auident _a0 :>Astfn.or_ctyp)
and of_ctyp: Astf.of_ctyp -> Astfn.of_ctyp =
  function
  | `Of (_a0,_a1,_a2) ->
      let _a1 = vid _a1 in let _a2 = ctyp _a2 in `Of (_a1, _a2)
  | #vid' as _a0 -> (vid' _a0 :>Astfn.of_ctyp)
  | #ant as _a0 -> (ant _a0 :>Astfn.of_ctyp)
and pat: Astf.pat -> Astfn.pat =
  function
  | #vid as _a0 -> (vid _a0 :>Astfn.pat)
  | `App (_a0,_a1,_a2) ->
      let _a1 = pat _a1 in let _a2 = pat _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = pat _a1 in let _a2 = pat _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = pat _a1 in let _a2 = pat _a2 in `Sem (_a1, _a2)
  | `Par (_a0,_a1) -> let _a1 = pat _a1 in `Par _a1
  | #any as _a0 -> (any _a0 :>Astfn.pat)
  | `Record (_a0,_a1) -> let _a1 = rec_pat _a1 in `Record _a1
  | #literal as _a0 -> (literal _a0 :>Astfn.pat)
  | `Alias (_a0,_a1,_a2) ->
      let _a1 = pat _a1 in let _a2 = alident _a2 in `Alias (_a1, _a2)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = pat _a1 in `Array _a1
  | `LabelS (_a0,_a1) -> let _a1 = alident _a1 in `LabelS _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = alident _a1 in let _a2 = pat _a2 in `Label (_a1, _a2)
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = alident _a1 in let _a2 = pat _a2 in `OptLabl (_a1, _a2)
  | `OptLablS (_a0,_a1) -> let _a1 = alident _a1 in `OptLablS _a1
  | `OptLablExpr (_a0,_a1,_a2,_a3) ->
      let _a1 = alident _a1 in
      let _a2 = pat _a2 in let _a3 = exp _a3 in `OptLablExpr (_a1, _a2, _a3)
  | `Bar (_a0,_a1,_a2) ->
      let _a1 = pat _a1 in let _a2 = pat _a2 in `Bar (_a1, _a2)
  | `PaRng (_a0,_a1,_a2) ->
      let _a1 = pat _a1 in let _a2 = pat _a2 in `PaRng (_a1, _a2)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = pat _a1 in let _a2 = ctyp _a2 in `Constraint (_a1, _a2)
  | `ClassPath (_a0,_a1) -> let _a1 = ident _a1 in `ClassPath _a1
  | `Lazy (_a0,_a1) -> let _a1 = pat _a1 in `Lazy _a1
  | `ModuleUnpack (_a0,_a1) -> let _a1 = auident _a1 in `ModuleUnpack _a1
  | `ModuleConstraint (_a0,_a1,_a2) ->
      let _a1 = auident _a1 in
      let _a2 = ctyp _a2 in `ModuleConstraint (_a1, _a2)
and rec_pat: Astf.rec_pat -> Astfn.rec_pat =
  function
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = vid _a1 in let _a2 = pat _a2 in `RecBind (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = rec_pat _a1 in let _a2 = rec_pat _a2 in `Sem (_a1, _a2)
  | #any as _a0 -> (any _a0 :>Astfn.rec_pat)
  | #ant as _a0 -> (ant _a0 :>Astfn.rec_pat)
and exp: Astf.exp -> Astfn.exp =
  function
  | #vid as _a0 -> (vid _a0 :>Astfn.exp)
  | `App (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = exp _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = exp _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = exp _a2 in `Sem (_a1, _a2)
  | `Par (_a0,_a1) -> let _a1 = exp _a1 in `Par _a1
  | #any as _a0 -> (any _a0 :>Astfn.exp)
  | `Record (_a0,_a1) -> let _a1 = rec_exp _a1 in `Record _a1
  | #literal as _a0 -> (literal _a0 :>Astfn.exp)
  | `RecordWith (_a0,_a1,_a2) ->
      let _a1 = rec_exp _a1 in let _a2 = exp _a2 in `RecordWith (_a1, _a2)
  | `Field (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = vid _a2 in `Field (_a1, _a2)
  | `ArrayDot (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = exp _a2 in `ArrayDot (_a1, _a2)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = exp _a1 in `Array _a1
  | `Assert (_a0,_a1) -> let _a1 = exp _a1 in `Assert _a1
  | `Assign (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = exp _a2 in `Assign (_a1, _a2)
  | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
      let _a1 = alident _a1 in
      let _a2 = exp _a2 in
      let _a3 = exp _a3 in
      let _a4 = flag _a4 in
      let _a5 = exp _a5 in `For (_a1, _a2, _a3, _a4, _a5)
  | `Fun (_a0,_a1) -> let _a1 = case _a1 in `Fun _a1
  | `IfThenElse (_a0,_a1,_a2,_a3) ->
      let _a1 = exp _a1 in
      let _a2 = exp _a2 in let _a3 = exp _a3 in `IfThenElse (_a1, _a2, _a3)
  | `IfThen (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = exp _a2 in `IfThen (_a1, _a2)
  | `LabelS (_a0,_a1) -> let _a1 = alident _a1 in `LabelS _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = alident _a1 in let _a2 = exp _a2 in `Label (_a1, _a2)
  | `Lazy (_a0,_a1) -> let _a1 = exp _a1 in `Lazy _a1
  | `LetIn (_a0,_a1,_a2,_a3) ->
      let _a1 = flag _a1 in
      let _a2 = bind _a2 in let _a3 = exp _a3 in `LetIn (_a1, _a2, _a3)
  | `LetTryInWith (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = flag _a1 in
      let _a2 = bind _a2 in
      let _a3 = exp _a3 in
      let _a4 = case _a4 in `LetTryInWith (_a1, _a2, _a3, _a4)
  | `LetModule (_a0,_a1,_a2,_a3) ->
      let _a1 = auident _a1 in
      let _a2 = mexp _a2 in let _a3 = exp _a3 in `LetModule (_a1, _a2, _a3)
  | `Match (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = case _a2 in `Match (_a1, _a2)
  | `New (_a0,_a1) -> let _a1 = ident _a1 in `New _a1
  | `Obj (_a0,_a1) -> let _a1 = clfield _a1 in `Obj _a1
  | `ObjEnd _a0 -> `ObjEnd
  | `ObjPat (_a0,_a1,_a2) ->
      let _a1 = pat _a1 in let _a2 = clfield _a2 in `ObjPat (_a1, _a2)
  | `ObjPatEnd (_a0,_a1) -> let _a1 = pat _a1 in `ObjPatEnd _a1
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = alident _a1 in let _a2 = exp _a2 in `OptLabl (_a1, _a2)
  | `OptLablS (_a0,_a1) -> let _a1 = alident _a1 in `OptLablS _a1
  | `OvrInst (_a0,_a1) -> let _a1 = rec_exp _a1 in `OvrInst _a1
  | `OvrInstEmpty _a0 -> `OvrInstEmpty
  | `Seq (_a0,_a1) -> let _a1 = exp _a1 in `Seq _a1
  | `Send (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = alident _a2 in `Send (_a1, _a2)
  | `StringDot (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = exp _a2 in `StringDot (_a1, _a2)
  | `Try (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = case _a2 in `Try (_a1, _a2)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = ctyp _a2 in `Constraint (_a1, _a2)
  | `Coercion (_a0,_a1,_a2,_a3) ->
      let _a1 = exp _a1 in
      let _a2 = ctyp _a2 in let _a3 = ctyp _a3 in `Coercion (_a1, _a2, _a3)
  | `Subtype (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = ctyp _a2 in `Subtype (_a1, _a2)
  | `While (_a0,_a1,_a2) ->
      let _a1 = exp _a1 in let _a2 = exp _a2 in `While (_a1, _a2)
  | `LetOpen (_a0,_a1,_a2,_a3) ->
      let _a1 = flag _a1 in
      let _a2 = ident _a2 in let _a3 = exp _a3 in `LetOpen (_a1, _a2, _a3)
  | `LocalTypeFun (_a0,_a1,_a2) ->
      let _a1 = alident _a1 in let _a2 = exp _a2 in `LocalTypeFun (_a1, _a2)
  | `Package_exp (_a0,_a1) -> let _a1 = mexp _a1 in `Package_exp _a1
and rec_exp: Astf.rec_exp -> Astfn.rec_exp =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = rec_exp _a1 in let _a2 = rec_exp _a2 in `Sem (_a1, _a2)
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = vid _a1 in let _a2 = exp _a2 in `RecBind (_a1, _a2)
  | #any as _a0 -> (any _a0 :>Astfn.rec_exp)
  | #ant as _a0 -> (ant _a0 :>Astfn.rec_exp)
and mtyp: Astf.mtyp -> Astfn.mtyp =
  function
  | #ident' as _a0 -> (ident' _a0 :>Astfn.mtyp)
  | `Sig (_a0,_a1) -> let _a1 = sigi _a1 in `Sig _a1
  | `SigEnd _a0 -> `SigEnd
  | `Functor (_a0,_a1,_a2,_a3) ->
      let _a1 = auident _a1 in
      let _a2 = mtyp _a2 in let _a3 = mtyp _a3 in `Functor (_a1, _a2, _a3)
  | `With (_a0,_a1,_a2) ->
      let _a1 = mtyp _a1 in let _a2 = constr _a2 in `With (_a1, _a2)
  | `ModuleTypeOf (_a0,_a1) -> let _a1 = mexp _a1 in `ModuleTypeOf _a1
  | #ant as _a0 -> (ant _a0 :>Astfn.mtyp)
and sigi: Astf.sigi -> Astfn.sigi =
  function
  | `Val (_a0,_a1,_a2) ->
      let _a1 = alident _a1 in let _a2 = ctyp _a2 in `Val (_a1, _a2)
  | `External (_a0,_a1,_a2,_a3) ->
      let _a1 = alident _a1 in
      let _a2 = ctyp _a2 in
      let _a3 = strings _a3 in `External (_a1, _a2, _a3)
  | `Type (_a0,_a1) -> let _a1 = decl _a1 in `Type _a1
  | `Exception (_a0,_a1) -> let _a1 = of_ctyp _a1 in `Exception _a1
  | `Class (_a0,_a1) -> let _a1 = cltdecl _a1 in `Class _a1
  | `ClassType (_a0,_a1) -> let _a1 = cltdecl _a1 in `ClassType _a1
  | `Module (_a0,_a1,_a2) ->
      let _a1 = auident _a1 in let _a2 = mtyp _a2 in `Module (_a1, _a2)
  | `ModuleTypeEnd (_a0,_a1) -> let _a1 = auident _a1 in `ModuleTypeEnd _a1
  | `ModuleType (_a0,_a1,_a2) ->
      let _a1 = auident _a1 in let _a2 = mtyp _a2 in `ModuleType (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = sigi _a1 in let _a2 = sigi _a2 in `Sem (_a1, _a2)
  | `DirectiveSimple (_a0,_a1) ->
      let _a1 = alident _a1 in `DirectiveSimple _a1
  | `Directive (_a0,_a1,_a2) ->
      let _a1 = alident _a1 in let _a2 = exp _a2 in `Directive (_a1, _a2)
  | `Open (_a0,_a1,_a2) ->
      let _a1 = flag _a1 in let _a2 = ident _a2 in `Open (_a1, _a2)
  | `Include (_a0,_a1) -> let _a1 = mtyp _a1 in `Include _a1
  | `RecModule (_a0,_a1) -> let _a1 = mbind _a1 in `RecModule _a1
  | #ant as _a0 -> (ant _a0 :>Astfn.sigi)
and mbind: Astf.mbind -> Astfn.mbind =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = mbind _a1 in let _a2 = mbind _a2 in `And (_a1, _a2)
  | `ModuleBind (_a0,_a1,_a2,_a3) ->
      let _a1 = auident _a1 in
      let _a2 = mtyp _a2 in let _a3 = mexp _a3 in `ModuleBind (_a1, _a2, _a3)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = auident _a1 in let _a2 = mtyp _a2 in `Constraint (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.mbind)
and constr: Astf.constr -> Astfn.constr =
  function
  | `TypeEq (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = ctyp _a2 in `TypeEq (_a1, _a2)
  | `ModuleEq (_a0,_a1,_a2) ->
      let _a1 = ident _a1 in let _a2 = ident _a2 in `ModuleEq (_a1, _a2)
  | `TypeEqPriv (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = ctyp _a2 in `TypeEqPriv (_a1, _a2)
  | `TypeSubst (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = ctyp _a2 in `TypeSubst (_a1, _a2)
  | `ModuleSubst (_a0,_a1,_a2) ->
      let _a1 = ident _a1 in let _a2 = ident _a2 in `ModuleSubst (_a1, _a2)
  | `And (_a0,_a1,_a2) ->
      let _a1 = constr _a1 in let _a2 = constr _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.constr)
and bind: Astf.bind -> Astfn.bind =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = bind _a1 in let _a2 = bind _a2 in `And (_a1, _a2)
  | `Bind (_a0,_a1,_a2) ->
      let _a1 = pat _a1 in let _a2 = exp _a2 in `Bind (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.bind)
and case: Astf.case -> Astfn.case =
  function
  | `Bar (_a0,_a1,_a2) ->
      let _a1 = case _a1 in let _a2 = case _a2 in `Bar (_a1, _a2)
  | `Case (_a0,_a1,_a2) ->
      let _a1 = pat _a1 in let _a2 = exp _a2 in `Case (_a1, _a2)
  | `CaseWhen (_a0,_a1,_a2,_a3) ->
      let _a1 = pat _a1 in
      let _a2 = exp _a2 in let _a3 = exp _a3 in `CaseWhen (_a1, _a2, _a3)
  | #ant as _a0 -> (ant _a0 :>Astfn.case)
and mexp: Astf.mexp -> Astfn.mexp =
  function
  | #vid' as _a0 -> (vid' _a0 :>Astfn.mexp)
  | `App (_a0,_a1,_a2) ->
      let _a1 = mexp _a1 in let _a2 = mexp _a2 in `App (_a1, _a2)
  | `Functor (_a0,_a1,_a2,_a3) ->
      let _a1 = auident _a1 in
      let _a2 = mtyp _a2 in let _a3 = mexp _a3 in `Functor (_a1, _a2, _a3)
  | `Struct (_a0,_a1) -> let _a1 = stru _a1 in `Struct _a1
  | `StructEnd _a0 -> `StructEnd
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = mexp _a1 in let _a2 = mtyp _a2 in `Constraint (_a1, _a2)
  | `PackageModule (_a0,_a1) -> let _a1 = exp _a1 in `PackageModule _a1
  | #ant as _a0 -> (ant _a0 :>Astfn.mexp)
and stru: Astf.stru -> Astfn.stru =
  function
  | `Class (_a0,_a1) -> let _a1 = cldecl _a1 in `Class _a1
  | `ClassType (_a0,_a1) -> let _a1 = cltdecl _a1 in `ClassType _a1
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = stru _a1 in let _a2 = stru _a2 in `Sem (_a1, _a2)
  | `DirectiveSimple (_a0,_a1) ->
      let _a1 = alident _a1 in `DirectiveSimple _a1
  | `Directive (_a0,_a1,_a2) ->
      let _a1 = alident _a1 in let _a2 = exp _a2 in `Directive (_a1, _a2)
  | `Exception (_a0,_a1) -> let _a1 = of_ctyp _a1 in `Exception _a1
  | `StExp (_a0,_a1) -> let _a1 = exp _a1 in `StExp _a1
  | `External (_a0,_a1,_a2,_a3) ->
      let _a1 = alident _a1 in
      let _a2 = ctyp _a2 in
      let _a3 = strings _a3 in `External (_a1, _a2, _a3)
  | `Include (_a0,_a1) -> let _a1 = mexp _a1 in `Include _a1
  | `Module (_a0,_a1,_a2) ->
      let _a1 = auident _a1 in let _a2 = mexp _a2 in `Module (_a1, _a2)
  | `RecModule (_a0,_a1) -> let _a1 = mbind _a1 in `RecModule _a1
  | `ModuleType (_a0,_a1,_a2) ->
      let _a1 = auident _a1 in let _a2 = mtyp _a2 in `ModuleType (_a1, _a2)
  | `Open (_a0,_a1,_a2) ->
      let _a1 = flag _a1 in let _a2 = ident _a2 in `Open (_a1, _a2)
  | `Type (_a0,_a1) -> let _a1 = decl _a1 in `Type _a1
  | `TypeWith (_a0,_a1,_a2) ->
      let _a1 = decl _a1 in let _a2 = strings _a2 in `TypeWith (_a1, _a2)
  | `Value (_a0,_a1,_a2) ->
      let _a1 = flag _a1 in let _a2 = bind _a2 in `Value (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.stru)
and cltdecl: Astf.cltdecl -> Astfn.cltdecl =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = cltdecl _a1 in let _a2 = cltdecl _a2 in `And (_a1, _a2)
  | `CtDecl (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = flag _a1 in
      let _a2 = ident _a2 in
      let _a3 = type_parameters _a3 in
      let _a4 = cltyp _a4 in `CtDecl (_a1, _a2, _a3, _a4)
  | `CtDeclS (_a0,_a1,_a2,_a3) ->
      let _a1 = flag _a1 in
      let _a2 = ident _a2 in let _a3 = cltyp _a3 in `CtDeclS (_a1, _a2, _a3)
  | #ant as _a0 -> (ant _a0 :>Astfn.cltdecl)
and cltyp: Astf.cltyp -> Astfn.cltyp =
  function
  | #vid' as _a0 -> (vid' _a0 :>Astfn.cltyp)
  | `ClApply (_a0,_a1,_a2) ->
      let _a1 = vid _a1 in
      let _a2 = type_parameters _a2 in `ClApply (_a1, _a2)
  | `CtFun (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = cltyp _a2 in `CtFun (_a1, _a2)
  | `ObjTy (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = clsigi _a2 in `ObjTy (_a1, _a2)
  | `ObjTyEnd (_a0,_a1) -> let _a1 = ctyp _a1 in `ObjTyEnd _a1
  | `Obj (_a0,_a1) -> let _a1 = clsigi _a1 in `Obj _a1
  | `ObjEnd _a0 -> `ObjEnd
  | `And (_a0,_a1,_a2) ->
      let _a1 = cltyp _a1 in let _a2 = cltyp _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.cltyp)
and clsigi: Astf.clsigi -> Astfn.clsigi =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = clsigi _a1 in let _a2 = clsigi _a2 in `Sem (_a1, _a2)
  | `SigInherit (_a0,_a1) -> let _a1 = cltyp _a1 in `SigInherit _a1
  | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = alident _a1 in
      let _a2 = flag _a2 in
      let _a3 = flag _a3 in let _a4 = ctyp _a4 in `CgVal (_a1, _a2, _a3, _a4)
  | `Method (_a0,_a1,_a2,_a3) ->
      let _a1 = alident _a1 in
      let _a2 = flag _a2 in let _a3 = ctyp _a3 in `Method (_a1, _a2, _a3)
  | `VirMeth (_a0,_a1,_a2,_a3) ->
      let _a1 = alident _a1 in
      let _a2 = flag _a2 in let _a3 = ctyp _a3 in `VirMeth (_a1, _a2, _a3)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = ctyp _a2 in `Eq (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.clsigi)
and cldecl: Astf.cldecl -> Astfn.cldecl =
  function
  | `ClDecl (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = flag _a1 in
      let _a2 = ident _a2 in
      let _a3 = type_parameters _a3 in
      let _a4 = clexp _a4 in `ClDecl (_a1, _a2, _a3, _a4)
  | `ClDeclS (_a0,_a1,_a2,_a3) ->
      let _a1 = flag _a1 in
      let _a2 = ident _a2 in let _a3 = clexp _a3 in `ClDeclS (_a1, _a2, _a3)
  | `And (_a0,_a1,_a2) ->
      let _a1 = cldecl _a1 in let _a2 = cldecl _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.cldecl)
and clexp: Astf.clexp -> Astfn.clexp =
  function
  | `CeApp (_a0,_a1,_a2) ->
      let _a1 = clexp _a1 in let _a2 = exp _a2 in `CeApp (_a1, _a2)
  | #vid' as _a0 -> (vid' _a0 :>Astfn.clexp)
  | `ClApply (_a0,_a1,_a2) ->
      let _a1 = vid _a1 in
      let _a2 = type_parameters _a2 in `ClApply (_a1, _a2)
  | `CeFun (_a0,_a1,_a2) ->
      let _a1 = pat _a1 in let _a2 = clexp _a2 in `CeFun (_a1, _a2)
  | `LetIn (_a0,_a1,_a2,_a3) ->
      let _a1 = flag _a1 in
      let _a2 = bind _a2 in let _a3 = clexp _a3 in `LetIn (_a1, _a2, _a3)
  | `Obj (_a0,_a1) -> let _a1 = clfield _a1 in `Obj _a1
  | `ObjEnd _a0 -> `ObjEnd
  | `ObjPat (_a0,_a1,_a2) ->
      let _a1 = pat _a1 in let _a2 = clfield _a2 in `ObjPat (_a1, _a2)
  | `ObjPatEnd (_a0,_a1) -> let _a1 = pat _a1 in `ObjPatEnd _a1
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = clexp _a1 in let _a2 = cltyp _a2 in `Constraint (_a1, _a2)
  | #ant as _a0 -> (ant _a0 :>Astfn.clexp)
and clfield: Astf.clfield -> Astfn.clfield =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = clfield _a1 in let _a2 = clfield _a2 in `Sem (_a1, _a2)
  | `Inherit (_a0,_a1,_a2) ->
      let _a1 = flag _a1 in let _a2 = clexp _a2 in `Inherit (_a1, _a2)
  | `InheritAs (_a0,_a1,_a2,_a3) ->
      let _a1 = flag _a1 in
      let _a2 = clexp _a2 in
      let _a3 = alident _a3 in `InheritAs (_a1, _a2, _a3)
  | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = alident _a1 in
      let _a2 = flag _a2 in
      let _a3 = flag _a3 in let _a4 = exp _a4 in `CrVal (_a1, _a2, _a3, _a4)
  | `VirVal (_a0,_a1,_a2,_a3) ->
      let _a1 = alident _a1 in
      let _a2 = flag _a2 in let _a3 = ctyp _a3 in `VirVal (_a1, _a2, _a3)
  | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
      let _a1 = alident _a1 in
      let _a2 = flag _a2 in
      let _a3 = flag _a3 in
      let _a4 = exp _a4 in
      let _a5 = ctyp _a5 in `CrMth (_a1, _a2, _a3, _a4, _a5)
  | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = alident _a1 in
      let _a2 = flag _a2 in
      let _a3 = flag _a3 in let _a4 = exp _a4 in `CrMthS (_a1, _a2, _a3, _a4)
  | `VirMeth (_a0,_a1,_a2,_a3) ->
      let _a1 = alident _a1 in
      let _a2 = flag _a2 in let _a3 = ctyp _a3 in `VirMeth (_a1, _a2, _a3)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = ctyp _a1 in let _a2 = ctyp _a2 in `Eq (_a1, _a2)
  | `Initializer (_a0,_a1) -> let _a1 = exp _a1 in `Initializer _a1
  | #ant as _a0 -> (ant _a0 :>Astfn.clfield)
let rec ep: Astf.ep -> Astfn.ep =
  function
  | #vid as _a0 -> (vid _a0 :>Astfn.ep)
  | `App (_a0,_a1,_a2) ->
      let _a1 = ep _a1 in let _a2 = ep _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = ep _a1 in let _a2 = ep _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = ep _a1 in let _a2 = ep _a2 in `Sem (_a1, _a2)
  | `Par (_a0,_a1) -> let _a1 = ep _a1 in `Par _a1
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = ep _a1 in let _a2 = ctyp _a2 in `Constraint (_a1, _a2)
  | #any as _a0 -> (any _a0 :>Astfn.ep)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = ep _a1 in `Array _a1
  | `Record (_a0,_a1) -> let _a1 = rec_bind _a1 in `Record _a1
  | #literal as _a0 -> (literal _a0 :>Astfn.ep)
and rec_bind: Astf.rec_bind -> Astfn.rec_bind =
  function
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = vid _a1 in let _a2 = ep _a2 in `RecBind (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = rec_bind _a1 in let _a2 = rec_bind _a2 in `Sem (_a1, _a2)
  | #any as _a0 -> (any _a0 :>Astfn.rec_bind)
  | #ant as _a0 -> (ant _a0 :>Astfn.rec_bind)
