open Astfn
let ant _loc x = x
let literal: Locf.t -> Astfn.literal -> Astf.literal =
  fun loc  ->
    function
    | `Chr _a0 -> `Chr (loc, _a0)
    | `Int _a0 -> `Int (loc, _a0)
    | `Int32 _a0 -> `Int32 (loc, _a0)
    | `Int64 _a0 -> `Int64 (loc, _a0)
    | `Flo _a0 -> `Flo (loc, _a0)
    | `Nativeint _a0 -> `Nativeint (loc, _a0)
    | `Str _a0 -> `Str (loc, _a0)
    | `Bool _a0 -> `Bool (loc, _a0)
    | `Unit -> `Unit loc
let flag: Locf.t -> Astfn.flag -> Astf.flag =
  fun loc  ->
    function
    | `Positive -> `Positive loc
    | `Negative -> `Negative loc
    | #ant as _a0 -> (ant loc _a0 :>Astf.flag)
let position_flag: Locf.t -> Astfn.position_flag -> Astf.position_flag =
  fun loc  ->
    function
    | `Positive -> `Positive loc
    | `Negative -> `Negative loc
    | `Normal -> `Normal loc
    | #ant as _a0 -> (ant loc _a0 :>Astf.position_flag)
let rec strings: Locf.t -> Astfn.strings -> Astf.strings =
  fun loc  ->
    function
    | `App (_a0,_a1) ->
        let _a0 = strings loc _a0 in
        let _a1 = strings loc _a1 in `App (loc, _a0, _a1)
    | `Str _a0 -> `Str (loc, _a0)
    | #ant as _a0 -> (ant loc _a0 :>Astf.strings)
let lident: Locf.t -> Astfn.lident -> Astf.lident =
  fun loc  (`Lid _a0)  -> `Lid (loc, _a0)
let alident: Locf.t -> Astfn.alident -> Astf.alident =
  fun loc  ->
    function
    | `Lid _a0 -> `Lid (loc, _a0)
    | #ant as _a0 -> (ant loc _a0 :>Astf.alident)
let auident: Locf.t -> Astfn.auident -> Astf.auident =
  fun loc  ->
    function
    | `Uid _a0 -> `Uid (loc, _a0)
    | #ant as _a0 -> (ant loc _a0 :>Astf.auident)
let aident: Locf.t -> Astfn.aident -> Astf.aident =
  fun loc  ->
    function
    | #alident as _a0 -> (alident loc _a0 :>Astf.aident)
    | #auident as _a0 -> (auident loc _a0 :>Astf.aident)
let astring: Locf.t -> Astfn.astring -> Astf.astring =
  fun loc  ->
    function
    | `C _a0 -> `C (loc, _a0)
    | #ant as _a0 -> (ant loc _a0 :>Astf.astring)
let rec uident: Locf.t -> Astfn.uident -> Astf.uident =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = uident loc _a0 in
        let _a1 = uident loc _a1 in `Dot (loc, _a0, _a1)
    | `App (_a0,_a1) ->
        let _a0 = uident loc _a0 in
        let _a1 = uident loc _a1 in `App (loc, _a0, _a1)
    | #auident as _a0 -> (auident loc _a0 :>Astf.uident)
let rec ident: Locf.t -> Astfn.ident -> Astf.ident =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = ident loc _a0 in
        let _a1 = ident loc _a1 in `Dot (loc, _a0, _a1)
    | `Apply (_a0,_a1) ->
        let _a0 = ident loc _a0 in
        let _a1 = ident loc _a1 in `Apply (loc, _a0, _a1)
    | #alident as _a0 -> (alident loc _a0 :>Astf.ident)
    | #auident as _a0 -> (auident loc _a0 :>Astf.ident)
let ident': Locf.t -> Astfn.ident' -> Astf.ident' =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = ident loc _a0 in
        let _a1 = ident loc _a1 in `Dot (loc, _a0, _a1)
    | `Apply (_a0,_a1) ->
        let _a0 = ident loc _a0 in
        let _a1 = ident loc _a1 in `Apply (loc, _a0, _a1)
    | `Lid _a0 -> `Lid (loc, _a0)
    | `Uid _a0 -> `Uid (loc, _a0)
let rec vid: Locf.t -> Astfn.vid -> Astf.vid =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = vid loc _a0 in
        let _a1 = vid loc _a1 in `Dot (loc, _a0, _a1)
    | `Lid _a0 -> `Lid (loc, _a0)
    | `Uid _a0 -> `Uid (loc, _a0)
    | #ant as _a0 -> (ant loc _a0 :>Astf.vid)
let vid': Locf.t -> Astfn.vid' -> Astf.vid' =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = vid loc _a0 in
        let _a1 = vid loc _a1 in `Dot (loc, _a0, _a1)
    | `Lid _a0 -> `Lid (loc, _a0)
    | `Uid _a0 -> `Uid (loc, _a0)
let rec dupath: Locf.t -> Astfn.dupath -> Astf.dupath =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = dupath loc _a0 in
        let _a1 = dupath loc _a1 in `Dot (loc, _a0, _a1)
    | #auident as _a0 -> (auident loc _a0 :>Astf.dupath)
let dlpath: Locf.t -> Astfn.dlpath -> Astf.dlpath =
  fun loc  ->
    function
    | `Dot (_a0,_a1) ->
        let _a0 = dupath loc _a0 in
        let _a1 = alident loc _a1 in `Dot (loc, _a0, _a1)
    | #alident as _a0 -> (alident loc _a0 :>Astf.dlpath)
let any: Locf.t -> Astfn.any -> Astf.any = fun loc  `Any  -> `Any loc
let rec ctyp: Locf.t -> Astfn.ctyp -> Astf.ctyp =
  fun loc  ->
    function
    | `Alias (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = alident loc _a1 in `Alias (loc, _a0, _a1)
    | #any as _a0 -> (any loc _a0 :>Astf.ctyp)
    | `App (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = ctyp loc _a1 in `App (loc, _a0, _a1)
    | `Arrow (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = ctyp loc _a1 in `Arrow (loc, _a0, _a1)
    | `ClassPath _a0 -> let _a0 = ident loc _a0 in `ClassPath (loc, _a0)
    | `Label (_a0,_a1) ->
        let _a0 = alident loc _a0 in
        let _a1 = ctyp loc _a1 in `Label (loc, _a0, _a1)
    | `OptLabl (_a0,_a1) ->
        let _a0 = alident loc _a0 in
        let _a1 = ctyp loc _a1 in `OptLabl (loc, _a0, _a1)
    | #ident' as _a0 -> (ident' loc _a0 :>Astf.ctyp)
    | `TyObj (_a0,_a1) ->
        let _a0 = name_ctyp loc _a0 in
        let _a1 = flag loc _a1 in `TyObj (loc, _a0, _a1)
    | `TyObjEnd _a0 -> let _a0 = flag loc _a0 in `TyObjEnd (loc, _a0)
    | `TyPol (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = ctyp loc _a1 in `TyPol (loc, _a0, _a1)
    | `TyPolEnd _a0 -> let _a0 = ctyp loc _a0 in `TyPolEnd (loc, _a0)
    | `TyTypePol (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = ctyp loc _a1 in `TyTypePol (loc, _a0, _a1)
    | `Quote (_a0,_a1) ->
        let _a0 = position_flag loc _a0 in
        let _a1 = alident loc _a1 in `Quote (loc, _a0, _a1)
    | `QuoteAny _a0 ->
        let _a0 = position_flag loc _a0 in `QuoteAny (loc, _a0)
    | `Par _a0 -> let _a0 = ctyp loc _a0 in `Par (loc, _a0)
    | `Sta (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = ctyp loc _a1 in `Sta (loc, _a0, _a1)
    | `PolyEq _a0 -> let _a0 = row_field loc _a0 in `PolyEq (loc, _a0)
    | `PolySup _a0 -> let _a0 = row_field loc _a0 in `PolySup (loc, _a0)
    | `PolyInf _a0 -> let _a0 = row_field loc _a0 in `PolyInf (loc, _a0)
    | `Com (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = ctyp loc _a1 in `Com (loc, _a0, _a1)
    | `PolyInfSup (_a0,_a1) ->
        let _a0 = row_field loc _a0 in
        let _a1 = tag_names loc _a1 in `PolyInfSup (loc, _a0, _a1)
    | `Package _a0 -> let _a0 = mtyp loc _a0 in `Package (loc, _a0)
    | #ant as _a0 -> (ant loc _a0 :>Astf.ctyp)
and type_parameters: Locf.t -> Astfn.type_parameters -> Astf.type_parameters
  =
  fun loc  ->
    function
    | `Com (_a0,_a1) ->
        let _a0 = type_parameters loc _a0 in
        let _a1 = type_parameters loc _a1 in `Com (loc, _a0, _a1)
    | `Ctyp _a0 -> let _a0 = ctyp loc _a0 in `Ctyp (loc, _a0)
    | #ant as _a0 -> (ant loc _a0 :>Astf.type_parameters)
and row_field: Locf.t -> Astfn.row_field -> Astf.row_field =
  fun loc  ->
    function
    | #ant as _a0 -> (ant loc _a0 :>Astf.row_field)
    | `Bar (_a0,_a1) ->
        let _a0 = row_field loc _a0 in
        let _a1 = row_field loc _a1 in `Bar (loc, _a0, _a1)
    | `TyVrn _a0 -> let _a0 = astring loc _a0 in `TyVrn (loc, _a0)
    | `TyVrnOf (_a0,_a1) ->
        let _a0 = astring loc _a0 in
        let _a1 = ctyp loc _a1 in `TyVrnOf (loc, _a0, _a1)
    | `Ctyp _a0 -> let _a0 = ctyp loc _a0 in `Ctyp (loc, _a0)
and tag_names: Locf.t -> Astfn.tag_names -> Astf.tag_names =
  fun loc  ->
    function
    | #ant as _a0 -> (ant loc _a0 :>Astf.tag_names)
    | `App (_a0,_a1) ->
        let _a0 = tag_names loc _a0 in
        let _a1 = tag_names loc _a1 in `App (loc, _a0, _a1)
    | `TyVrn _a0 -> let _a0 = astring loc _a0 in `TyVrn (loc, _a0)
and decl: Locf.t -> Astfn.decl -> Astf.decl =
  fun loc  ->
    function
    | `TyDcl (_a0,_a1,_a2,_a3) ->
        let _a0 = alident loc _a0 in
        let _a1 = opt_decl_params loc _a1 in
        let _a2 = type_info loc _a2 in
        let _a3 = opt_type_constr loc _a3 in `TyDcl (loc, _a0, _a1, _a2, _a3)
    | `TyAbstr (_a0,_a1,_a2) ->
        let _a0 = alident loc _a0 in
        let _a1 = opt_decl_params loc _a1 in
        let _a2 = opt_type_constr loc _a2 in `TyAbstr (loc, _a0, _a1, _a2)
    | `And (_a0,_a1) ->
        let _a0 = decl loc _a0 in
        let _a1 = decl loc _a1 in `And (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.decl)
and type_constr: Locf.t -> Astfn.type_constr -> Astf.type_constr =
  fun loc  ->
    function
    | `And (_a0,_a1) ->
        let _a0 = type_constr loc _a0 in
        let _a1 = type_constr loc _a1 in `And (loc, _a0, _a1)
    | `Eq (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = ctyp loc _a1 in `Eq (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.type_constr)
and opt_type_constr: Locf.t -> Astfn.opt_type_constr -> Astf.opt_type_constr
  =
  fun loc  ->
    function
    | `Some _a0 -> let _a0 = type_constr loc _a0 in `Some (loc, _a0)
    | `None -> `None loc
and decl_param: Locf.t -> Astfn.decl_param -> Astf.decl_param =
  fun loc  ->
    function
    | `Quote (_a0,_a1) ->
        let _a0 = position_flag loc _a0 in
        let _a1 = alident loc _a1 in `Quote (loc, _a0, _a1)
    | `QuoteAny _a0 ->
        let _a0 = position_flag loc _a0 in `QuoteAny (loc, _a0)
    | `Any -> `Any loc
    | #ant as _a0 -> (ant loc _a0 :>Astf.decl_param)
and decl_params: Locf.t -> Astfn.decl_params -> Astf.decl_params =
  fun loc  ->
    function
    | `Quote (_a0,_a1) ->
        let _a0 = position_flag loc _a0 in
        let _a1 = alident loc _a1 in `Quote (loc, _a0, _a1)
    | `QuoteAny _a0 ->
        let _a0 = position_flag loc _a0 in `QuoteAny (loc, _a0)
    | `Any -> `Any loc
    | `Com (_a0,_a1) ->
        let _a0 = decl_params loc _a0 in
        let _a1 = decl_params loc _a1 in `Com (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.decl_params)
and opt_decl_params: Locf.t -> Astfn.opt_decl_params -> Astf.opt_decl_params
  =
  fun loc  ->
    function
    | `Some _a0 -> let _a0 = decl_params loc _a0 in `Some (loc, _a0)
    | `None -> `None loc
and type_info: Locf.t -> Astfn.type_info -> Astf.type_info =
  fun loc  ->
    function
    | `TyMan (_a0,_a1,_a2) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = flag loc _a1 in
        let _a2 = type_repr loc _a2 in `TyMan (loc, _a0, _a1, _a2)
    | `TyRepr (_a0,_a1) ->
        let _a0 = flag loc _a0 in
        let _a1 = type_repr loc _a1 in `TyRepr (loc, _a0, _a1)
    | `TyEq (_a0,_a1) ->
        let _a0 = flag loc _a0 in
        let _a1 = ctyp loc _a1 in `TyEq (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.type_info)
and type_repr: Locf.t -> Astfn.type_repr -> Astf.type_repr =
  fun loc  ->
    function
    | `Record _a0 -> let _a0 = name_ctyp loc _a0 in `Record (loc, _a0)
    | `Sum _a0 -> let _a0 = or_ctyp loc _a0 in `Sum (loc, _a0)
    | #ant as _a0 -> (ant loc _a0 :>Astf.type_repr)
and name_ctyp: Locf.t -> Astfn.name_ctyp -> Astf.name_ctyp =
  fun loc  ->
    function
    | `Sem (_a0,_a1) ->
        let _a0 = name_ctyp loc _a0 in
        let _a1 = name_ctyp loc _a1 in `Sem (loc, _a0, _a1)
    | `TyCol (_a0,_a1) ->
        let _a0 = alident loc _a0 in
        let _a1 = ctyp loc _a1 in `TyCol (loc, _a0, _a1)
    | `TyColMut (_a0,_a1) ->
        let _a0 = alident loc _a0 in
        let _a1 = ctyp loc _a1 in `TyColMut (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.name_ctyp)
and or_ctyp: Locf.t -> Astfn.or_ctyp -> Astf.or_ctyp =
  fun loc  ->
    function
    | `Bar (_a0,_a1) ->
        let _a0 = or_ctyp loc _a0 in
        let _a1 = or_ctyp loc _a1 in `Bar (loc, _a0, _a1)
    | `TyCol (_a0,_a1) ->
        let _a0 = auident loc _a0 in
        let _a1 = ctyp loc _a1 in `TyCol (loc, _a0, _a1)
    | `Of (_a0,_a1) ->
        let _a0 = auident loc _a0 in
        let _a1 = ctyp loc _a1 in `Of (loc, _a0, _a1)
    | #auident as _a0 -> (auident loc _a0 :>Astf.or_ctyp)
and of_ctyp: Locf.t -> Astfn.of_ctyp -> Astf.of_ctyp =
  fun loc  ->
    function
    | `Of (_a0,_a1) ->
        let _a0 = vid loc _a0 in
        let _a1 = ctyp loc _a1 in `Of (loc, _a0, _a1)
    | #vid' as _a0 -> (vid' loc _a0 :>Astf.of_ctyp)
    | #ant as _a0 -> (ant loc _a0 :>Astf.of_ctyp)
and pat: Locf.t -> Astfn.pat -> Astf.pat =
  fun loc  ->
    function
    | #vid as _a0 -> (vid loc _a0 :>Astf.pat)
    | `App (_a0,_a1) ->
        let _a0 = pat loc _a0 in
        let _a1 = pat loc _a1 in `App (loc, _a0, _a1)
    | `Vrn _a0 -> `Vrn (loc, _a0)
    | `Com (_a0,_a1) ->
        let _a0 = pat loc _a0 in
        let _a1 = pat loc _a1 in `Com (loc, _a0, _a1)
    | `Sem (_a0,_a1) ->
        let _a0 = pat loc _a0 in
        let _a1 = pat loc _a1 in `Sem (loc, _a0, _a1)
    | `Par _a0 -> let _a0 = pat loc _a0 in `Par (loc, _a0)
    | #any as _a0 -> (any loc _a0 :>Astf.pat)
    | `Record _a0 -> let _a0 = rec_pat loc _a0 in `Record (loc, _a0)
    | #literal as _a0 -> (literal loc _a0 :>Astf.pat)
    | `Alias (_a0,_a1) ->
        let _a0 = pat loc _a0 in
        let _a1 = alident loc _a1 in `Alias (loc, _a0, _a1)
    | `ArrayEmpty -> `ArrayEmpty loc
    | `Array _a0 -> let _a0 = pat loc _a0 in `Array (loc, _a0)
    | `LabelS _a0 -> let _a0 = alident loc _a0 in `LabelS (loc, _a0)
    | `Label (_a0,_a1) ->
        let _a0 = alident loc _a0 in
        let _a1 = pat loc _a1 in `Label (loc, _a0, _a1)
    | `OptLabl (_a0,_a1) ->
        let _a0 = alident loc _a0 in
        let _a1 = pat loc _a1 in `OptLabl (loc, _a0, _a1)
    | `OptLablS _a0 -> let _a0 = alident loc _a0 in `OptLablS (loc, _a0)
    | `OptLablExpr (_a0,_a1,_a2) ->
        let _a0 = alident loc _a0 in
        let _a1 = pat loc _a1 in
        let _a2 = exp loc _a2 in `OptLablExpr (loc, _a0, _a1, _a2)
    | `Bar (_a0,_a1) ->
        let _a0 = pat loc _a0 in
        let _a1 = pat loc _a1 in `Bar (loc, _a0, _a1)
    | `PaRng (_a0,_a1) ->
        let _a0 = pat loc _a0 in
        let _a1 = pat loc _a1 in `PaRng (loc, _a0, _a1)
    | `Constraint (_a0,_a1) ->
        let _a0 = pat loc _a0 in
        let _a1 = ctyp loc _a1 in `Constraint (loc, _a0, _a1)
    | `ClassPath _a0 -> let _a0 = ident loc _a0 in `ClassPath (loc, _a0)
    | `Lazy _a0 -> let _a0 = pat loc _a0 in `Lazy (loc, _a0)
    | `ModuleUnpack _a0 ->
        let _a0 = auident loc _a0 in `ModuleUnpack (loc, _a0)
    | `ModuleConstraint (_a0,_a1) ->
        let _a0 = auident loc _a0 in
        let _a1 = ctyp loc _a1 in `ModuleConstraint (loc, _a0, _a1)
and rec_pat: Locf.t -> Astfn.rec_pat -> Astf.rec_pat =
  fun loc  ->
    function
    | `RecBind (_a0,_a1) ->
        let _a0 = vid loc _a0 in
        let _a1 = pat loc _a1 in `RecBind (loc, _a0, _a1)
    | `Sem (_a0,_a1) ->
        let _a0 = rec_pat loc _a0 in
        let _a1 = rec_pat loc _a1 in `Sem (loc, _a0, _a1)
    | #any as _a0 -> (any loc _a0 :>Astf.rec_pat)
    | #ant as _a0 -> (ant loc _a0 :>Astf.rec_pat)
and exp: Locf.t -> Astfn.exp -> Astf.exp =
  fun loc  ->
    function
    | #vid as _a0 -> (vid loc _a0 :>Astf.exp)
    | `App (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = exp loc _a1 in `App (loc, _a0, _a1)
    | `Vrn _a0 -> `Vrn (loc, _a0)
    | `Com (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = exp loc _a1 in `Com (loc, _a0, _a1)
    | `Sem (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = exp loc _a1 in `Sem (loc, _a0, _a1)
    | `Par _a0 -> let _a0 = exp loc _a0 in `Par (loc, _a0)
    | #any as _a0 -> (any loc _a0 :>Astf.exp)
    | `Record _a0 -> let _a0 = rec_exp loc _a0 in `Record (loc, _a0)
    | #literal as _a0 -> (literal loc _a0 :>Astf.exp)
    | `RecordWith (_a0,_a1) ->
        let _a0 = rec_exp loc _a0 in
        let _a1 = exp loc _a1 in `RecordWith (loc, _a0, _a1)
    | `Field (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = vid loc _a1 in `Field (loc, _a0, _a1)
    | `ArrayDot (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = exp loc _a1 in `ArrayDot (loc, _a0, _a1)
    | `ArrayEmpty -> `ArrayEmpty loc
    | `Array _a0 -> let _a0 = exp loc _a0 in `Array (loc, _a0)
    | `Assert _a0 -> let _a0 = exp loc _a0 in `Assert (loc, _a0)
    | `Assign (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = exp loc _a1 in `Assign (loc, _a0, _a1)
    | `For (_a0,_a1,_a2,_a3,_a4) ->
        let _a0 = alident loc _a0 in
        let _a1 = exp loc _a1 in
        let _a2 = exp loc _a2 in
        let _a3 = flag loc _a3 in
        let _a4 = exp loc _a4 in `For (loc, _a0, _a1, _a2, _a3, _a4)
    | `Fun _a0 -> let _a0 = case loc _a0 in `Fun (loc, _a0)
    | `IfThenElse (_a0,_a1,_a2) ->
        let _a0 = exp loc _a0 in
        let _a1 = exp loc _a1 in
        let _a2 = exp loc _a2 in `IfThenElse (loc, _a0, _a1, _a2)
    | `IfThen (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = exp loc _a1 in `IfThen (loc, _a0, _a1)
    | `LabelS _a0 -> let _a0 = alident loc _a0 in `LabelS (loc, _a0)
    | `Label (_a0,_a1) ->
        let _a0 = alident loc _a0 in
        let _a1 = exp loc _a1 in `Label (loc, _a0, _a1)
    | `Lazy _a0 -> let _a0 = exp loc _a0 in `Lazy (loc, _a0)
    | `LetIn (_a0,_a1,_a2) ->
        let _a0 = flag loc _a0 in
        let _a1 = bind loc _a1 in
        let _a2 = exp loc _a2 in `LetIn (loc, _a0, _a1, _a2)
    | `LetTryInWith (_a0,_a1,_a2,_a3) ->
        let _a0 = flag loc _a0 in
        let _a1 = bind loc _a1 in
        let _a2 = exp loc _a2 in
        let _a3 = case loc _a3 in `LetTryInWith (loc, _a0, _a1, _a2, _a3)
    | `LetModule (_a0,_a1,_a2) ->
        let _a0 = auident loc _a0 in
        let _a1 = mexp loc _a1 in
        let _a2 = exp loc _a2 in `LetModule (loc, _a0, _a1, _a2)
    | `Match (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = case loc _a1 in `Match (loc, _a0, _a1)
    | `New _a0 -> let _a0 = ident loc _a0 in `New (loc, _a0)
    | `Obj _a0 -> let _a0 = clfield loc _a0 in `Obj (loc, _a0)
    | `ObjEnd -> `ObjEnd loc
    | `ObjPat (_a0,_a1) ->
        let _a0 = pat loc _a0 in
        let _a1 = clfield loc _a1 in `ObjPat (loc, _a0, _a1)
    | `ObjPatEnd _a0 -> let _a0 = pat loc _a0 in `ObjPatEnd (loc, _a0)
    | `OptLabl (_a0,_a1) ->
        let _a0 = alident loc _a0 in
        let _a1 = exp loc _a1 in `OptLabl (loc, _a0, _a1)
    | `OptLablS _a0 -> let _a0 = alident loc _a0 in `OptLablS (loc, _a0)
    | `OvrInst _a0 -> let _a0 = rec_exp loc _a0 in `OvrInst (loc, _a0)
    | `OvrInstEmpty -> `OvrInstEmpty loc
    | `Seq _a0 -> let _a0 = exp loc _a0 in `Seq (loc, _a0)
    | `Send (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = alident loc _a1 in `Send (loc, _a0, _a1)
    | `StringDot (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = exp loc _a1 in `StringDot (loc, _a0, _a1)
    | `Try (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = case loc _a1 in `Try (loc, _a0, _a1)
    | `Constraint (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = ctyp loc _a1 in `Constraint (loc, _a0, _a1)
    | `Coercion (_a0,_a1,_a2) ->
        let _a0 = exp loc _a0 in
        let _a1 = ctyp loc _a1 in
        let _a2 = ctyp loc _a2 in `Coercion (loc, _a0, _a1, _a2)
    | `Subtype (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = ctyp loc _a1 in `Subtype (loc, _a0, _a1)
    | `While (_a0,_a1) ->
        let _a0 = exp loc _a0 in
        let _a1 = exp loc _a1 in `While (loc, _a0, _a1)
    | `LetOpen (_a0,_a1,_a2) ->
        let _a0 = flag loc _a0 in
        let _a1 = ident loc _a1 in
        let _a2 = exp loc _a2 in `LetOpen (loc, _a0, _a1, _a2)
    | `LocalTypeFun (_a0,_a1) ->
        let _a0 = alident loc _a0 in
        let _a1 = exp loc _a1 in `LocalTypeFun (loc, _a0, _a1)
    | `Package_exp _a0 -> let _a0 = mexp loc _a0 in `Package_exp (loc, _a0)
and rec_exp: Locf.t -> Astfn.rec_exp -> Astf.rec_exp =
  fun loc  ->
    function
    | `Sem (_a0,_a1) ->
        let _a0 = rec_exp loc _a0 in
        let _a1 = rec_exp loc _a1 in `Sem (loc, _a0, _a1)
    | `RecBind (_a0,_a1) ->
        let _a0 = vid loc _a0 in
        let _a1 = exp loc _a1 in `RecBind (loc, _a0, _a1)
    | #any as _a0 -> (any loc _a0 :>Astf.rec_exp)
    | #ant as _a0 -> (ant loc _a0 :>Astf.rec_exp)
and mtyp: Locf.t -> Astfn.mtyp -> Astf.mtyp =
  fun loc  ->
    function
    | #ident' as _a0 -> (ident' loc _a0 :>Astf.mtyp)
    | `Sig _a0 -> let _a0 = sigi loc _a0 in `Sig (loc, _a0)
    | `SigEnd -> `SigEnd loc
    | `Functor (_a0,_a1,_a2) ->
        let _a0 = auident loc _a0 in
        let _a1 = mtyp loc _a1 in
        let _a2 = mtyp loc _a2 in `Functor (loc, _a0, _a1, _a2)
    | `With (_a0,_a1) ->
        let _a0 = mtyp loc _a0 in
        let _a1 = constr loc _a1 in `With (loc, _a0, _a1)
    | `ModuleTypeOf _a0 -> let _a0 = mexp loc _a0 in `ModuleTypeOf (loc, _a0)
    | #ant as _a0 -> (ant loc _a0 :>Astf.mtyp)
and sigi: Locf.t -> Astfn.sigi -> Astf.sigi =
  fun loc  ->
    function
    | `Val (_a0,_a1) ->
        let _a0 = alident loc _a0 in
        let _a1 = ctyp loc _a1 in `Val (loc, _a0, _a1)
    | `External (_a0,_a1,_a2) ->
        let _a0 = alident loc _a0 in
        let _a1 = ctyp loc _a1 in
        let _a2 = strings loc _a2 in `External (loc, _a0, _a1, _a2)
    | `Type _a0 -> let _a0 = decl loc _a0 in `Type (loc, _a0)
    | `Exception _a0 -> let _a0 = of_ctyp loc _a0 in `Exception (loc, _a0)
    | `Class _a0 -> let _a0 = cltdecl loc _a0 in `Class (loc, _a0)
    | `ClassType _a0 -> let _a0 = cltdecl loc _a0 in `ClassType (loc, _a0)
    | `Module (_a0,_a1) ->
        let _a0 = auident loc _a0 in
        let _a1 = mtyp loc _a1 in `Module (loc, _a0, _a1)
    | `ModuleTypeEnd _a0 ->
        let _a0 = auident loc _a0 in `ModuleTypeEnd (loc, _a0)
    | `ModuleType (_a0,_a1) ->
        let _a0 = auident loc _a0 in
        let _a1 = mtyp loc _a1 in `ModuleType (loc, _a0, _a1)
    | `Sem (_a0,_a1) ->
        let _a0 = sigi loc _a0 in
        let _a1 = sigi loc _a1 in `Sem (loc, _a0, _a1)
    | `DirectiveSimple _a0 ->
        let _a0 = alident loc _a0 in `DirectiveSimple (loc, _a0)
    | `Directive (_a0,_a1) ->
        let _a0 = alident loc _a0 in
        let _a1 = exp loc _a1 in `Directive (loc, _a0, _a1)
    | `Open (_a0,_a1) ->
        let _a0 = flag loc _a0 in
        let _a1 = ident loc _a1 in `Open (loc, _a0, _a1)
    | `Include _a0 -> let _a0 = mtyp loc _a0 in `Include (loc, _a0)
    | `RecModule _a0 -> let _a0 = mbind loc _a0 in `RecModule (loc, _a0)
    | #ant as _a0 -> (ant loc _a0 :>Astf.sigi)
and mbind: Locf.t -> Astfn.mbind -> Astf.mbind =
  fun loc  ->
    function
    | `And (_a0,_a1) ->
        let _a0 = mbind loc _a0 in
        let _a1 = mbind loc _a1 in `And (loc, _a0, _a1)
    | `ModuleBind (_a0,_a1,_a2) ->
        let _a0 = auident loc _a0 in
        let _a1 = mtyp loc _a1 in
        let _a2 = mexp loc _a2 in `ModuleBind (loc, _a0, _a1, _a2)
    | `Constraint (_a0,_a1) ->
        let _a0 = auident loc _a0 in
        let _a1 = mtyp loc _a1 in `Constraint (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.mbind)
and constr: Locf.t -> Astfn.constr -> Astf.constr =
  fun loc  ->
    function
    | `TypeEq (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = ctyp loc _a1 in `TypeEq (loc, _a0, _a1)
    | `ModuleEq (_a0,_a1) ->
        let _a0 = ident loc _a0 in
        let _a1 = ident loc _a1 in `ModuleEq (loc, _a0, _a1)
    | `TypeEqPriv (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = ctyp loc _a1 in `TypeEqPriv (loc, _a0, _a1)
    | `TypeSubst (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = ctyp loc _a1 in `TypeSubst (loc, _a0, _a1)
    | `ModuleSubst (_a0,_a1) ->
        let _a0 = ident loc _a0 in
        let _a1 = ident loc _a1 in `ModuleSubst (loc, _a0, _a1)
    | `And (_a0,_a1) ->
        let _a0 = constr loc _a0 in
        let _a1 = constr loc _a1 in `And (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.constr)
and bind: Locf.t -> Astfn.bind -> Astf.bind =
  fun loc  ->
    function
    | `And (_a0,_a1) ->
        let _a0 = bind loc _a0 in
        let _a1 = bind loc _a1 in `And (loc, _a0, _a1)
    | `Bind (_a0,_a1) ->
        let _a0 = pat loc _a0 in
        let _a1 = exp loc _a1 in `Bind (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.bind)
and case: Locf.t -> Astfn.case -> Astf.case =
  fun loc  ->
    function
    | `Bar (_a0,_a1) ->
        let _a0 = case loc _a0 in
        let _a1 = case loc _a1 in `Bar (loc, _a0, _a1)
    | `Case (_a0,_a1) ->
        let _a0 = pat loc _a0 in
        let _a1 = exp loc _a1 in `Case (loc, _a0, _a1)
    | `CaseWhen (_a0,_a1,_a2) ->
        let _a0 = pat loc _a0 in
        let _a1 = exp loc _a1 in
        let _a2 = exp loc _a2 in `CaseWhen (loc, _a0, _a1, _a2)
    | #ant as _a0 -> (ant loc _a0 :>Astf.case)
and mexp: Locf.t -> Astfn.mexp -> Astf.mexp =
  fun loc  ->
    function
    | #vid' as _a0 -> (vid' loc _a0 :>Astf.mexp)
    | `App (_a0,_a1) ->
        let _a0 = mexp loc _a0 in
        let _a1 = mexp loc _a1 in `App (loc, _a0, _a1)
    | `Functor (_a0,_a1,_a2) ->
        let _a0 = auident loc _a0 in
        let _a1 = mtyp loc _a1 in
        let _a2 = mexp loc _a2 in `Functor (loc, _a0, _a1, _a2)
    | `Struct _a0 -> let _a0 = stru loc _a0 in `Struct (loc, _a0)
    | `StructEnd -> `StructEnd loc
    | `Constraint (_a0,_a1) ->
        let _a0 = mexp loc _a0 in
        let _a1 = mtyp loc _a1 in `Constraint (loc, _a0, _a1)
    | `PackageModule _a0 ->
        let _a0 = exp loc _a0 in `PackageModule (loc, _a0)
    | #ant as _a0 -> (ant loc _a0 :>Astf.mexp)
and stru: Locf.t -> Astfn.stru -> Astf.stru =
  fun loc  ->
    function
    | `Class _a0 -> let _a0 = cldecl loc _a0 in `Class (loc, _a0)
    | `ClassType _a0 -> let _a0 = cltdecl loc _a0 in `ClassType (loc, _a0)
    | `Sem (_a0,_a1) ->
        let _a0 = stru loc _a0 in
        let _a1 = stru loc _a1 in `Sem (loc, _a0, _a1)
    | `DirectiveSimple _a0 ->
        let _a0 = alident loc _a0 in `DirectiveSimple (loc, _a0)
    | `Directive (_a0,_a1) ->
        let _a0 = alident loc _a0 in
        let _a1 = exp loc _a1 in `Directive (loc, _a0, _a1)
    | `Exception _a0 -> let _a0 = of_ctyp loc _a0 in `Exception (loc, _a0)
    | `StExp _a0 -> let _a0 = exp loc _a0 in `StExp (loc, _a0)
    | `External (_a0,_a1,_a2) ->
        let _a0 = alident loc _a0 in
        let _a1 = ctyp loc _a1 in
        let _a2 = strings loc _a2 in `External (loc, _a0, _a1, _a2)
    | `Include _a0 -> let _a0 = mexp loc _a0 in `Include (loc, _a0)
    | `Module (_a0,_a1) ->
        let _a0 = auident loc _a0 in
        let _a1 = mexp loc _a1 in `Module (loc, _a0, _a1)
    | `RecModule _a0 -> let _a0 = mbind loc _a0 in `RecModule (loc, _a0)
    | `ModuleType (_a0,_a1) ->
        let _a0 = auident loc _a0 in
        let _a1 = mtyp loc _a1 in `ModuleType (loc, _a0, _a1)
    | `Open (_a0,_a1) ->
        let _a0 = flag loc _a0 in
        let _a1 = ident loc _a1 in `Open (loc, _a0, _a1)
    | `Type _a0 -> let _a0 = decl loc _a0 in `Type (loc, _a0)
    | `TypeWith (_a0,_a1) ->
        let _a0 = decl loc _a0 in
        let _a1 = strings loc _a1 in `TypeWith (loc, _a0, _a1)
    | `Value (_a0,_a1) ->
        let _a0 = flag loc _a0 in
        let _a1 = bind loc _a1 in `Value (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.stru)
and cltdecl: Locf.t -> Astfn.cltdecl -> Astf.cltdecl =
  fun loc  ->
    function
    | `And (_a0,_a1) ->
        let _a0 = cltdecl loc _a0 in
        let _a1 = cltdecl loc _a1 in `And (loc, _a0, _a1)
    | `CtDecl (_a0,_a1,_a2,_a3) ->
        let _a0 = flag loc _a0 in
        let _a1 = ident loc _a1 in
        let _a2 = type_parameters loc _a2 in
        let _a3 = cltyp loc _a3 in `CtDecl (loc, _a0, _a1, _a2, _a3)
    | `CtDeclS (_a0,_a1,_a2) ->
        let _a0 = flag loc _a0 in
        let _a1 = ident loc _a1 in
        let _a2 = cltyp loc _a2 in `CtDeclS (loc, _a0, _a1, _a2)
    | #ant as _a0 -> (ant loc _a0 :>Astf.cltdecl)
and cltyp: Locf.t -> Astfn.cltyp -> Astf.cltyp =
  fun loc  ->
    function
    | #vid' as _a0 -> (vid' loc _a0 :>Astf.cltyp)
    | `ClApply (_a0,_a1) ->
        let _a0 = vid loc _a0 in
        let _a1 = type_parameters loc _a1 in `ClApply (loc, _a0, _a1)
    | `CtFun (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = cltyp loc _a1 in `CtFun (loc, _a0, _a1)
    | `ObjTy (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = clsigi loc _a1 in `ObjTy (loc, _a0, _a1)
    | `ObjTyEnd _a0 -> let _a0 = ctyp loc _a0 in `ObjTyEnd (loc, _a0)
    | `Obj _a0 -> let _a0 = clsigi loc _a0 in `Obj (loc, _a0)
    | `ObjEnd -> `ObjEnd loc
    | `And (_a0,_a1) ->
        let _a0 = cltyp loc _a0 in
        let _a1 = cltyp loc _a1 in `And (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.cltyp)
and clsigi: Locf.t -> Astfn.clsigi -> Astf.clsigi =
  fun loc  ->
    function
    | `Sem (_a0,_a1) ->
        let _a0 = clsigi loc _a0 in
        let _a1 = clsigi loc _a1 in `Sem (loc, _a0, _a1)
    | `SigInherit _a0 -> let _a0 = cltyp loc _a0 in `SigInherit (loc, _a0)
    | `CgVal (_a0,_a1,_a2,_a3) ->
        let _a0 = alident loc _a0 in
        let _a1 = flag loc _a1 in
        let _a2 = flag loc _a2 in
        let _a3 = ctyp loc _a3 in `CgVal (loc, _a0, _a1, _a2, _a3)
    | `Method (_a0,_a1,_a2) ->
        let _a0 = alident loc _a0 in
        let _a1 = flag loc _a1 in
        let _a2 = ctyp loc _a2 in `Method (loc, _a0, _a1, _a2)
    | `VirMeth (_a0,_a1,_a2) ->
        let _a0 = alident loc _a0 in
        let _a1 = flag loc _a1 in
        let _a2 = ctyp loc _a2 in `VirMeth (loc, _a0, _a1, _a2)
    | `Eq (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = ctyp loc _a1 in `Eq (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.clsigi)
and cldecl: Locf.t -> Astfn.cldecl -> Astf.cldecl =
  fun loc  ->
    function
    | `ClDecl (_a0,_a1,_a2,_a3) ->
        let _a0 = flag loc _a0 in
        let _a1 = ident loc _a1 in
        let _a2 = type_parameters loc _a2 in
        let _a3 = clexp loc _a3 in `ClDecl (loc, _a0, _a1, _a2, _a3)
    | `ClDeclS (_a0,_a1,_a2) ->
        let _a0 = flag loc _a0 in
        let _a1 = ident loc _a1 in
        let _a2 = clexp loc _a2 in `ClDeclS (loc, _a0, _a1, _a2)
    | `And (_a0,_a1) ->
        let _a0 = cldecl loc _a0 in
        let _a1 = cldecl loc _a1 in `And (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.cldecl)
and clexp: Locf.t -> Astfn.clexp -> Astf.clexp =
  fun loc  ->
    function
    | `CeApp (_a0,_a1) ->
        let _a0 = clexp loc _a0 in
        let _a1 = exp loc _a1 in `CeApp (loc, _a0, _a1)
    | #vid' as _a0 -> (vid' loc _a0 :>Astf.clexp)
    | `ClApply (_a0,_a1) ->
        let _a0 = vid loc _a0 in
        let _a1 = type_parameters loc _a1 in `ClApply (loc, _a0, _a1)
    | `CeFun (_a0,_a1) ->
        let _a0 = pat loc _a0 in
        let _a1 = clexp loc _a1 in `CeFun (loc, _a0, _a1)
    | `LetIn (_a0,_a1,_a2) ->
        let _a0 = flag loc _a0 in
        let _a1 = bind loc _a1 in
        let _a2 = clexp loc _a2 in `LetIn (loc, _a0, _a1, _a2)
    | `Obj _a0 -> let _a0 = clfield loc _a0 in `Obj (loc, _a0)
    | `ObjEnd -> `ObjEnd loc
    | `ObjPat (_a0,_a1) ->
        let _a0 = pat loc _a0 in
        let _a1 = clfield loc _a1 in `ObjPat (loc, _a0, _a1)
    | `ObjPatEnd _a0 -> let _a0 = pat loc _a0 in `ObjPatEnd (loc, _a0)
    | `Constraint (_a0,_a1) ->
        let _a0 = clexp loc _a0 in
        let _a1 = cltyp loc _a1 in `Constraint (loc, _a0, _a1)
    | #ant as _a0 -> (ant loc _a0 :>Astf.clexp)
and clfield: Locf.t -> Astfn.clfield -> Astf.clfield =
  fun loc  ->
    function
    | `Sem (_a0,_a1) ->
        let _a0 = clfield loc _a0 in
        let _a1 = clfield loc _a1 in `Sem (loc, _a0, _a1)
    | `Inherit (_a0,_a1) ->
        let _a0 = flag loc _a0 in
        let _a1 = clexp loc _a1 in `Inherit (loc, _a0, _a1)
    | `InheritAs (_a0,_a1,_a2) ->
        let _a0 = flag loc _a0 in
        let _a1 = clexp loc _a1 in
        let _a2 = alident loc _a2 in `InheritAs (loc, _a0, _a1, _a2)
    | `CrVal (_a0,_a1,_a2,_a3) ->
        let _a0 = alident loc _a0 in
        let _a1 = flag loc _a1 in
        let _a2 = flag loc _a2 in
        let _a3 = exp loc _a3 in `CrVal (loc, _a0, _a1, _a2, _a3)
    | `VirVal (_a0,_a1,_a2) ->
        let _a0 = alident loc _a0 in
        let _a1 = flag loc _a1 in
        let _a2 = ctyp loc _a2 in `VirVal (loc, _a0, _a1, _a2)
    | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
        let _a0 = alident loc _a0 in
        let _a1 = flag loc _a1 in
        let _a2 = flag loc _a2 in
        let _a3 = exp loc _a3 in
        let _a4 = ctyp loc _a4 in `CrMth (loc, _a0, _a1, _a2, _a3, _a4)
    | `CrMthS (_a0,_a1,_a2,_a3) ->
        let _a0 = alident loc _a0 in
        let _a1 = flag loc _a1 in
        let _a2 = flag loc _a2 in
        let _a3 = exp loc _a3 in `CrMthS (loc, _a0, _a1, _a2, _a3)
    | `VirMeth (_a0,_a1,_a2) ->
        let _a0 = alident loc _a0 in
        let _a1 = flag loc _a1 in
        let _a2 = ctyp loc _a2 in `VirMeth (loc, _a0, _a1, _a2)
    | `Eq (_a0,_a1) ->
        let _a0 = ctyp loc _a0 in
        let _a1 = ctyp loc _a1 in `Eq (loc, _a0, _a1)
    | `Initializer _a0 -> let _a0 = exp loc _a0 in `Initializer (loc, _a0)
    | #ant as _a0 -> (ant loc _a0 :>Astf.clfield)
let rec ep: Locf.t -> Astfn.ep -> Astf.ep =
  fun loc  ->
    function
    | #vid as _a0 -> (vid loc _a0 :>Astf.ep)
    | `App (_a0,_a1) ->
        let _a0 = ep loc _a0 in let _a1 = ep loc _a1 in `App (loc, _a0, _a1)
    | `Vrn _a0 -> `Vrn (loc, _a0)
    | `Com (_a0,_a1) ->
        let _a0 = ep loc _a0 in let _a1 = ep loc _a1 in `Com (loc, _a0, _a1)
    | `Sem (_a0,_a1) ->
        let _a0 = ep loc _a0 in let _a1 = ep loc _a1 in `Sem (loc, _a0, _a1)
    | `Par _a0 -> let _a0 = ep loc _a0 in `Par (loc, _a0)
    | `Constraint (_a0,_a1) ->
        let _a0 = ep loc _a0 in
        let _a1 = ctyp loc _a1 in `Constraint (loc, _a0, _a1)
    | #any as _a0 -> (any loc _a0 :>Astf.ep)
    | `ArrayEmpty -> `ArrayEmpty loc
    | `Array _a0 -> let _a0 = ep loc _a0 in `Array (loc, _a0)
    | `Record _a0 -> let _a0 = rec_bind loc _a0 in `Record (loc, _a0)
    | #literal as _a0 -> (literal loc _a0 :>Astf.ep)
and rec_bind: Locf.t -> Astfn.rec_bind -> Astf.rec_bind =
  fun loc  ->
    function
    | `RecBind (_a0,_a1) ->
        let _a0 = vid loc _a0 in
        let _a1 = ep loc _a1 in `RecBind (loc, _a0, _a1)
    | `Sem (_a0,_a1) ->
        let _a0 = rec_bind loc _a0 in
        let _a1 = rec_bind loc _a1 in `Sem (loc, _a0, _a1)
    | #any as _a0 -> (any loc _a0 :>Astf.rec_bind)
    | #ant as _a0 -> (ant loc _a0 :>Astf.rec_bind)
