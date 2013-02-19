include Ast
module type META_LOC = sig val meta_loc : loc -> loc -> ep end
open FanUtil
open LibUtil
let ghost = FanLoc.ghost
let _ = ()
let loc_of =
  function
  | `PaOlbi (_loc,_,_,_) -> _loc
  | `Any _loc -> _loc
  | `TyVrnInf (_loc,_) -> _loc
  | `Tup (_loc,_) -> _loc
  | `Array (_loc,_) -> _loc
  | `MtFun (_loc,_,_,_) -> _loc
  | `Id (_loc,_) -> _loc
  | `ArrayDot (_loc,_,_) -> _loc
  | `Directive (_loc,_,_) -> _loc
  | `TypeSubst (_loc,_,_) -> _loc
  | `ModuleBind (_loc,_,_,_) -> _loc
  | `Sta (_loc,_,_) -> _loc
  | `Match (_loc,_,_) -> _loc
  | `Obj (_loc,_,_) -> _loc
  | `TyPol (_loc,_,_) -> _loc
  | `Val (_loc,_,_) -> _loc
  | `C (_loc,_) -> _loc
  | `Str (_loc,_) -> _loc
  | `Or (_loc,_,_) -> _loc
  | `New (_loc,_) -> _loc
  | `Value (_loc,_,_) -> _loc
  | `Try (_loc,_,_) -> _loc
  | `Downto _loc -> _loc
  | `App (_loc,_,_) -> _loc
  | `Assign (_loc,_,_) -> _loc
  | `Normal _loc -> _loc
  | `True _loc -> _loc
  | `Sem (_loc,_,_) -> _loc
  | `Send (_loc,_,_) -> _loc
  | `Type (_loc,_) -> _loc
  | `Chr (_loc,_) -> _loc
  | `IfThenElse (_loc,_,_,_) -> _loc
  | `ClassType (_loc,_) -> _loc
  | `ModuleUnpack (_loc,_,_) -> _loc
  | `CeTyc (_loc,_,_) -> _loc
  | `CrVir (_loc,_,_,_) -> _loc
  | `Package (_loc,_) -> _loc
  | `While (_loc,_,_) -> _loc
  | `CrMth (_loc,_,_,_,_,_) -> _loc
  | `Dot (_loc,_,_) -> _loc
  | `Struct (_loc,_) -> _loc
  | `CeCon (_loc,_,_,_) -> _loc
  | `TyMan (_loc,_,_) -> _loc
  | `External (_loc,_,_,_) -> _loc
  | `CgVal (_loc,_,_,_,_) -> _loc
  | `Class (_loc,_) -> _loc
  | `LetIn (_loc,_,_,_) -> _loc
  | `Seq (_loc,_) -> _loc
  | `Quote (_loc,_,_) -> _loc
  | `TypeEq (_loc,_,_) -> _loc
  | `OptLabl (_loc,_,_) -> _loc
  | `Coercion (_loc,_,_,_) -> _loc
  | `CtFun (_loc,_,_) -> _loc
  | `Arrow (_loc,_,_) -> _loc
  | `Bind (_loc,_,_) -> _loc
  | `CtEq (_loc,_,_) -> _loc
  | `Functor (_loc,_,_,_) -> _loc
  | `With (_loc,_,_) -> _loc
  | `NativeInt (_loc,_) -> _loc
  | `Private _loc -> _loc
  | `Virtual _loc -> _loc
  | `RowVar _loc -> _loc
  | `IfThen (_loc,_,_) -> _loc
  | `RecBind (_loc,_,_) -> _loc
  | `Sig (_loc,_) -> _loc
  | `Mutable _loc -> _loc
  | `ReNil _loc -> _loc
  | `Lazy (_loc,_) -> _loc
  | `CeFun (_loc,_,_) -> _loc
  | `ClassPath (_loc,_) -> _loc
  | `False _loc -> _loc
  | `Nil _loc -> _loc
  | `Com (_loc,_,_) -> _loc
  | `Int64 (_loc,_) -> _loc
  | `TyVrnSup (_loc,_) -> _loc
  | `ModuleTypeOf (_loc,_) -> _loc
  | `To _loc -> _loc
  | `TyCol (_loc,_,_) -> _loc
  | `CgVir (_loc,_,_,_) -> _loc
  | `Initializer (_loc,_) -> _loc
  | `Amp (_loc,_,_) -> _loc
  | `ModuleEq (_loc,_,_) -> _loc
  | `Lid (_loc,_) -> _loc
  | `Record (_loc,_) -> _loc
  | `Constraint (_loc,_,_) -> _loc
  | `Ant (_loc,_) -> _loc
  | `Package_expr (_loc,_) -> _loc
  | `TyVrnEq (_loc,_) -> _loc
  | `Label (_loc,_,_) -> _loc
  | `TyTypePol (_loc,_,_) -> _loc
  | `Priv (_loc,_) -> _loc
  | `RvNil _loc -> _loc
  | `Override _loc -> _loc
  | `Include (_loc,_) -> _loc
  | `Flo (_loc,_) -> _loc
  | `Alias (_loc,_,_) -> _loc
  | `Vrn (_loc,_) -> _loc
  | `StExp (_loc,_) -> _loc
  | `Uid (_loc,_) -> _loc
  | `TyObj (_loc,_,_) -> _loc
  | `Of (_loc,_,_) -> _loc
  | `OvrInst (_loc,_) -> _loc
  | `OvNil _loc -> _loc
  | `ModuleSubst (_loc,_,_) -> _loc
  | `Mut (_loc,_) -> _loc
  | `Positive _loc -> _loc
  | `CrVal (_loc,_,_,_,_) -> _loc
  | `TyVrn (_loc,_) -> _loc
  | `Case (_loc,_,_,_) -> _loc
  | `ExAsr (_loc,_) -> _loc
  | `Exception (_loc,_) -> _loc
  | `CtCol (_loc,_,_) -> _loc
  | `RecordWith (_loc,_,_) -> _loc
  | `And (_loc,_,_) -> _loc
  | `TyDcl (_loc,_,_,_,_) -> _loc
  | `Int32 (_loc,_) -> _loc
  | `PaRng (_loc,_,_) -> _loc
  | `RecModule (_loc,_) -> _loc
  | `LocalTypeFun (_loc,_,_) -> _loc
  | `PrNil _loc -> _loc
  | `Int (_loc,_) -> _loc
  | `Negative _loc -> _loc
  | `Fun (_loc,_) -> _loc
  | `CeApp (_loc,_,_) -> _loc
  | `Eq (_loc,_,_) -> _loc
  | `LetModule (_loc,_,_,_) -> _loc
  | `LetOpen (_loc,_,_) -> _loc
  | `TyOfAmp (_loc,_,_) -> _loc
  | `StringDot (_loc,_,_) -> _loc
  | `For (_loc,_,_,_,_,_) -> _loc
  | `CrVvr (_loc,_,_,_) -> _loc
  | `CtCon (_loc,_,_,_) -> _loc
  | `Recursive _loc -> _loc
  | `TyVrnInfSup (_loc,_,_) -> _loc
  | `CtSig (_loc,_,_) -> _loc
  | `SigInherit (_loc,_) -> _loc
  | `ModuleType (_loc,_,_) -> _loc
  | `Inherit (_loc,_,_,_) -> _loc
  | `MuNil _loc -> _loc
  | `Method (_loc,_,_,_) -> _loc
  | `Module (_loc,_,_) -> _loc
  | `ExAsf _loc -> _loc
  | `PackageModule (_loc,_) -> _loc
  | `CeLet (_loc,_,_,_) -> _loc
  | `Open (_loc,_) -> _loc
  | `ViNil _loc -> _loc
  | `Sum (_loc,_) -> _loc
let list_of_list (loc : loc) =
  let rec loop top =
    function
    | [] -> `Id (ghost, (`Uid (ghost, "[]")))
    | e1::el ->
        let _loc = if top then loc else FanLoc.merge (loc_of e1) loc in
        `App
          (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e1)),
            (loop false el)) in
  loop true
let meta_int _loc i = `Int (_loc, (string_of_int i))
let meta_int32 _loc i = `Int32 (_loc, (Int32.to_string i))
let meta_int64 _loc i = `Int64 (_loc, (Int64.to_string i))
let meta_nativeint _loc i = `NativeInt (_loc, (Nativeint.to_string i))
let meta_float _loc i = `Flo (_loc, (FanUtil.float_repres i))
let meta_string _loc i = `Str (_loc, (String.escaped i))
let meta_char _loc i = `Chr (_loc, (Char.escaped i))
let meta_unit _loc _ = `Id (_loc, (`Uid (_loc, "()")))
let meta_bool _loc =
  function
  | true  -> `Id (_loc, (`Lid (_loc, "true")))
  | false  -> `Id (_loc, (`Lid (_loc, "false")))
let meta_ref mf_a _loc i =
  `Record
    (_loc,
      (`RecBind (_loc, (`Lid (_loc, "contents")), (mf_a _loc i.contents))))
let mklist loc =
  let rec loop top =
    function
    | [] -> `Id (loc, (`Uid (loc, "[]")))
    | e1::el ->
        let _loc = if top then loc else FanLoc.merge (loc_of e1) loc in
        `App
          (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e1)),
            (loop false el)) in
  loop true
let meta_list mf_a _loc ls =
  mklist _loc (List.map (fun x  -> mf_a _loc x) ls)
let meta_option mf_a _loc =
  function
  | None  -> `Id (_loc, (`Uid (_loc, "None")))
  | Some x -> `App (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), (mf_a _loc x))
let meta_arrow (type t) (_mf_a : FanLoc.t -> 'a -> t)
  (_mf_b : FanLoc.t -> 'b -> t) (_loc : FanLoc.t) (_x : 'a -> 'b) =
  invalid_arg "meta_arrow not implemented"
module Make(MetaLoc:META_LOC) =
  struct
    include MetaLoc
    let meta_ant _loc (`Ant (_a0,_a1)) = `Ant (_a0, _a1)
    let meta_literal _loc =
      function
      | `Chr (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Chr")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `Int (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Int")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `Int32 (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Int32")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `Int64 (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Int64")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `Flo (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Flo")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `NativeInt (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "NativeInt")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `Str (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Str")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
    let meta_rec_flag _loc =
      function
      | `Recursive _a0 ->
          `App (_loc, (`Vrn (_loc, "Recursive")), (meta_loc _loc _a0))
      | `ReNil _a0 ->
          `App (_loc, (`Vrn (_loc, "ReNil")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result2)
    let meta_direction_flag _loc =
      function
      | `To _a0 -> `App (_loc, (`Vrn (_loc, "To")), (meta_loc _loc _a0))
      | `Downto _a0 ->
          `App (_loc, (`Vrn (_loc, "Downto")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result3)
    let meta_mutable_flag _loc =
      function
      | `Mutable _a0 ->
          `App (_loc, (`Vrn (_loc, "Mutable")), (meta_loc _loc _a0))
      | `MuNil _a0 ->
          `App (_loc, (`Vrn (_loc, "MuNil")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result4)
    let meta_private_flag _loc =
      function
      | `Private _a0 ->
          `App (_loc, (`Vrn (_loc, "Private")), (meta_loc _loc _a0))
      | `PrNil _a0 ->
          `App (_loc, (`Vrn (_loc, "PrNil")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result5)
    let meta_virtual_flag _loc =
      function
      | `Virtual _a0 ->
          `App (_loc, (`Vrn (_loc, "Virtual")), (meta_loc _loc _a0))
      | `ViNil _a0 ->
          `App (_loc, (`Vrn (_loc, "ViNil")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result6)
    let meta_override_flag _loc =
      function
      | `Override _a0 ->
          `App (_loc, (`Vrn (_loc, "Override")), (meta_loc _loc _a0))
      | `OvNil _a0 ->
          `App (_loc, (`Vrn (_loc, "OvNil")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result7)
    let meta_row_var_flag _loc =
      function
      | `RowVar _a0 ->
          `App (_loc, (`Vrn (_loc, "RowVar")), (meta_loc _loc _a0))
      | `RvNil _a0 ->
          `App (_loc, (`Vrn (_loc, "RvNil")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result8)
    let meta_position_flag _loc =
      function
      | `Positive _a0 ->
          `App (_loc, (`Vrn (_loc, "Positive")), (meta_loc _loc _a0))
      | `Negative _a0 ->
          `App (_loc, (`Vrn (_loc, "Negative")), (meta_loc _loc _a0))
      | `Normal _a0 ->
          `App (_loc, (`Vrn (_loc, "Normal")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result9)
    let meta_meta_bool _loc =
      function
      | `True _a0 -> `App (_loc, (`Vrn (_loc, "True")), (meta_loc _loc _a0))
      | `False _a0 ->
          `App (_loc, (`Vrn (_loc, "False")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result10)
    let meta_meta_option mf_a _loc =
      function
      | `None -> `Vrn (_loc, "None")
      | `Some _a0 -> `App (_loc, (`Vrn (_loc, "Some")), (mf_a _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result11)
    let rec meta_meta_list mf_a _loc =
      function
      | `LNil -> `Vrn (_loc, "LNil")
      | `LCons (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "LCons")), (mf_a _loc _a0))),
              (meta_meta_list mf_a _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result12)
    let meta_alident _loc =
      function
      | `Lid (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Lid")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result13)
    let meta_auident _loc =
      function
      | `Uid (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Uid")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result14)
    let meta_aident _loc =
      function
      | #alident as _a0 -> (meta_alident _loc _a0 :>'result15)
      | #auident as _a0 -> (meta_auident _loc _a0 :>'result15)
    let meta_astring _loc =
      function
      | `C (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "C")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result16)
    let rec meta_ident _loc =
      function
      | `Dot (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Dot")), (meta_loc _loc _a0))),
                   (meta_ident _loc _a1))), (meta_ident _loc _a2))
      | `App (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
                   (meta_ident _loc _a1))), (meta_ident _loc _a2))
      | #alident as _a0 -> (meta_alident _loc _a0 :>'result17)
      | #auident as _a0 -> (meta_auident _loc _a0 :>'result17)
    let rec meta_ctyp _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `Alias (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Alias")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `Any _a0 -> `App (_loc, (`Vrn (_loc, "Any")), (meta_loc _loc _a0))
      | `App (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `Arrow (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Arrow")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `ClassPath (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "ClassPath")), (meta_loc _loc _a0))),
              (meta_ident _loc _a1))
      | `Label (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Label")), (meta_loc _loc _a0))),
                   (meta_alident _loc _a1))), (meta_ctyp _loc _a2))
      | `OptLabl (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "OptLabl")), (meta_loc _loc _a0))),
                   (meta_alident _loc _a1))), (meta_ctyp _loc _a2))
      | `Id (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Id")), (meta_loc _loc _a0))),
              (meta_ident _loc _a1))
      | `TyMan (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "TyMan")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc,
                             (`App
                                (_loc, (`Vrn (_loc, "TyDcl")),
                                  (meta_loc _loc _a0))),
                             (meta_alident _loc _a1))),
                        (meta_list meta_ctyp _loc _a2))),
                   (meta_ctyp _loc _a3))),
              (meta_list
                 (fun _loc  (_a0,_a1)  ->
                    `Tup
                      (_loc,
                        (`Com
                           (_loc, (meta_ctyp _loc _a0), (meta_ctyp _loc _a1)))))
                 _loc _a4))
      | `TyObj (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "TyObj")), (meta_loc _loc _a0))),
                   (meta_name_ctyp _loc _a1))), (meta_row_var_flag _loc _a2))
      | `TyPol (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "TyPol")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `TyTypePol (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "TyTypePol")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `Quote (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Quote")), (meta_loc _loc _a0))),
                   (meta_position_flag _loc _a1))),
              (meta_meta_option meta_alident _loc _a2))
      | `Record (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Record")), (meta_loc _loc _a0))),
              (meta_name_ctyp _loc _a1))
      | `TyCol (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "TyCol")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `Com (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Com")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `Sum (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Sum")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | `Of (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Of")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `And (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `Or (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Or")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `Priv (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Priv")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | `Mut (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Mut")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | `Tup (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Tup")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | `Sta (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Sta")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `TyVrn (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "TyVrn")), (meta_loc _loc _a0))),
              (meta_astring _loc _a1))
      | `TyVrnEq (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "TyVrnEq")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | `TyVrnSup (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "TyVrnSup")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | `TyVrnInf (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "TyVrnInf")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | `TyVrnInfSup (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "TyVrnInfSup")),
                        (meta_loc _loc _a0))), (meta_ctyp _loc _a1))),
              (meta_ctyp _loc _a2))
      | `Amp (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Amp")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `TyOfAmp (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "TyOfAmp")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `Package (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Package")), (meta_loc _loc _a0))),
              (meta_module_type _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result35)
    and meta_name_ctyp _loc =
      function
      | `Sem (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
                   (meta_name_ctyp _loc _a1))), (meta_name_ctyp _loc _a2))
      | `TyCol (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "TyCol")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result34)
    and meta_patt _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `Id (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Id")), (meta_loc _loc _a0))),
              (meta_ident _loc _a1))
      | `App (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_patt _loc _a2))
      | `Vrn (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Vrn")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `Com (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Com")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_patt _loc _a2))
      | `Sem (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_patt _loc _a2))
      | `Tup (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Tup")), (meta_loc _loc _a0))),
              (meta_patt _loc _a1))
      | `Any _a0 -> `App (_loc, (`Vrn (_loc, "Any")), (meta_loc _loc _a0))
      | `Record (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Record")), (meta_loc _loc _a0))),
              (meta_rec_patt _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result33)
      | #literal as _a0 -> (meta_literal _loc _a0 :>'result33)
      | `Alias (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Alias")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_alident _loc _a2))
      | `Array (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Array")), (meta_loc _loc _a0))),
              (meta_patt _loc _a1))
      | `Label (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Label")), (meta_loc _loc _a0))),
                   (meta_alident _loc _a1))), (meta_patt _loc _a2))
      | `PaOlbi (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "PaOlbi")),
                             (meta_loc _loc _a0))), (meta_alident _loc _a1))),
                   (meta_patt _loc _a2))),
              (meta_meta_option meta_expr _loc _a3))
      | `Or (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Or")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_patt _loc _a2))
      | `PaRng (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "PaRng")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_patt _loc _a2))
      | `Constraint (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "Constraint")),
                        (meta_loc _loc _a0))), (meta_patt _loc _a1))),
              (meta_ctyp _loc _a2))
      | `ClassPath (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "ClassPath")), (meta_loc _loc _a0))),
              (meta_ident _loc _a1))
      | `Lazy (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Lazy")), (meta_loc _loc _a0))),
              (meta_patt _loc _a1))
      | `ModuleUnpack (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "ModuleUnpack")),
                        (meta_loc _loc _a0))), (meta_auident _loc _a1))),
              (meta_meta_option meta_ctyp _loc _a2))
    and meta_rec_patt _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `RecBind (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "RecBind")), (meta_loc _loc _a0))),
                   (meta_ident _loc _a1))), (meta_patt _loc _a2))
      | `Sem (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
                   (meta_rec_patt _loc _a1))), (meta_rec_patt _loc _a2))
      | `Any _a0 -> `App (_loc, (`Vrn (_loc, "Any")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result32)
    and meta_expr _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `Id (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Id")), (meta_loc _loc _a0))),
              (meta_ident _loc _a1))
      | `App (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_expr _loc _a2))
      | `Vrn (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Vrn")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `Com (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Com")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_expr _loc _a2))
      | `Sem (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_expr _loc _a2))
      | `Tup (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Tup")), (meta_loc _loc _a0))),
              (meta_expr _loc _a1))
      | `Any _a0 -> `App (_loc, (`Vrn (_loc, "Any")), (meta_loc _loc _a0))
      | `Record (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Record")), (meta_loc _loc _a0))),
              (meta_rec_expr _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result31)
      | #literal as _a0 -> (meta_literal _loc _a0 :>'result31)
      | `RecordWith (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "RecordWith")),
                        (meta_loc _loc _a0))), (meta_rec_expr _loc _a1))),
              (meta_expr _loc _a2))
      | `Dot (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Dot")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_expr _loc _a2))
      | `ArrayDot (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "ArrayDot")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_expr _loc _a2))
      | `Array (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Array")), (meta_loc _loc _a0))),
              (meta_expr _loc _a1))
      | `ExAsf _a0 ->
          `App (_loc, (`Vrn (_loc, "ExAsf")), (meta_loc _loc _a0))
      | `ExAsr (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "ExAsr")), (meta_loc _loc _a0))),
              (meta_expr _loc _a1))
      | `Assign (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Assign")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_expr _loc _a2))
      | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc,
                             (`App
                                (_loc,
                                  (`App
                                     (_loc, (`Vrn (_loc, "For")),
                                       (meta_loc _loc _a0))),
                                  (meta_alident _loc _a1))),
                             (meta_expr _loc _a2))), (meta_expr _loc _a3))),
                   (meta_direction_flag _loc _a4))), (meta_expr _loc _a5))
      | `Fun (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Fun")), (meta_loc _loc _a0))),
              (meta_match_case _loc _a1))
      | `IfThenElse (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "IfThenElse")),
                             (meta_loc _loc _a0))), (meta_expr _loc _a1))),
                   (meta_expr _loc _a2))), (meta_expr _loc _a3))
      | `IfThen (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "IfThen")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_expr _loc _a2))
      | `Label (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Label")), (meta_loc _loc _a0))),
                   (meta_alident _loc _a1))), (meta_expr _loc _a2))
      | `Lazy (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Lazy")), (meta_loc _loc _a0))),
              (meta_expr _loc _a1))
      | `LetIn (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "LetIn")),
                             (meta_loc _loc _a0))), (meta_rec_flag _loc _a1))),
                   (meta_binding _loc _a2))), (meta_expr _loc _a3))
      | `LetModule (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "LetModule")),
                             (meta_loc _loc _a0))), (meta_auident _loc _a1))),
                   (meta_module_expr _loc _a2))), (meta_expr _loc _a3))
      | `Match (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Match")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_match_case _loc _a2))
      | `New (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "New")), (meta_loc _loc _a0))),
              (meta_ident _loc _a1))
      | `Obj (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Obj")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_class_str_item _loc _a2))
      | `OptLabl (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "OptLabl")), (meta_loc _loc _a0))),
                   (meta_alident _loc _a1))), (meta_expr _loc _a2))
      | `OvrInst (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "OvrInst")), (meta_loc _loc _a0))),
              (meta_rec_expr _loc _a1))
      | `Seq (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Seq")), (meta_loc _loc _a0))),
              (meta_expr _loc _a1))
      | `Send (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Send")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_alident _loc _a2))
      | `StringDot (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "StringDot")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_expr _loc _a2))
      | `Try (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Try")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_match_case _loc _a2))
      | `Constraint (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "Constraint")),
                        (meta_loc _loc _a0))), (meta_expr _loc _a1))),
              (meta_ctyp _loc _a2))
      | `Coercion (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "Coercion")),
                             (meta_loc _loc _a0))), (meta_expr _loc _a1))),
                   (meta_ctyp _loc _a2))), (meta_ctyp _loc _a3))
      | `While (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "While")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_expr _loc _a2))
      | `LetOpen (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "LetOpen")), (meta_loc _loc _a0))),
                   (meta_ident _loc _a1))), (meta_expr _loc _a2))
      | `LocalTypeFun (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "LocalTypeFun")),
                        (meta_loc _loc _a0))), (meta_alident _loc _a1))),
              (meta_expr _loc _a2))
      | `Package_expr (_a0,_a1) ->
          `App
            (_loc,
              (`App
                 (_loc, (`Vrn (_loc, "Package_expr")), (meta_loc _loc _a0))),
              (meta_module_expr _loc _a1))
    and meta_rec_expr _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `Sem (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
                   (meta_rec_expr _loc _a1))), (meta_rec_expr _loc _a2))
      | `RecBind (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "RecBind")), (meta_loc _loc _a0))),
                   (meta_ident _loc _a1))), (meta_expr _loc _a2))
      | `Any _a0 -> `App (_loc, (`Vrn (_loc, "Any")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result30)
    and meta_module_type _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `Id (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Id")), (meta_loc _loc _a0))),
              (meta_ident _loc _a1))
      | `MtFun (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "MtFun")),
                             (meta_loc _loc _a0))), (meta_auident _loc _a1))),
                   (meta_module_type _loc _a2))),
              (meta_module_type _loc _a3))
      | `Sig (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Sig")), (meta_loc _loc _a0))),
              (meta_sig_item _loc _a1))
      | `With (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "With")), (meta_loc _loc _a0))),
                   (meta_module_type _loc _a1))),
              (meta_with_constr _loc _a2))
      | `ModuleTypeOf (_a0,_a1) ->
          `App
            (_loc,
              (`App
                 (_loc, (`Vrn (_loc, "ModuleTypeOf")), (meta_loc _loc _a0))),
              (meta_module_expr _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result29)
    and meta_sig_item _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `Class (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Class")), (meta_loc _loc _a0))),
              (meta_class_type _loc _a1))
      | `ClassType (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "ClassType")), (meta_loc _loc _a0))),
              (meta_class_type _loc _a1))
      | `Sem (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
                   (meta_sig_item _loc _a1))), (meta_sig_item _loc _a2))
      | `Directive (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "Directive")), (meta_loc _loc _a0))),
                   (meta_alident _loc _a1))), (meta_expr _loc _a2))
      | `Exception (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Exception")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | `External (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "External")),
                             (meta_loc _loc _a0))), (meta_alident _loc _a1))),
                   (meta_ctyp _loc _a2))),
              (meta_meta_list meta_string _loc _a3))
      | `Include (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Include")), (meta_loc _loc _a0))),
              (meta_module_type _loc _a1))
      | `Module (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Module")), (meta_loc _loc _a0))),
                   (meta_auident _loc _a1))), (meta_module_type _loc _a2))
      | `RecModule (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "RecModule")), (meta_loc _loc _a0))),
              (meta_module_binding _loc _a1))
      | `ModuleType (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "ModuleType")),
                        (meta_loc _loc _a0))), (meta_auident _loc _a1))),
              (meta_module_type _loc _a2))
      | `Open (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Open")), (meta_loc _loc _a0))),
              (meta_ident _loc _a1))
      | `Type (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Type")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | `Val (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Val")), (meta_loc _loc _a0))),
                   (meta_alident _loc _a1))), (meta_ctyp _loc _a2))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result28)
    and meta_with_constr _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `TypeEq (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "TypeEq")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `ModuleEq (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "ModuleEq")), (meta_loc _loc _a0))),
                   (meta_ident _loc _a1))), (meta_ident _loc _a2))
      | `TypeSubst (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "TypeSubst")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `ModuleSubst (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "ModuleSubst")),
                        (meta_loc _loc _a0))), (meta_ident _loc _a1))),
              (meta_ident _loc _a2))
      | `And (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
                   (meta_with_constr _loc _a1))),
              (meta_with_constr _loc _a2))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result27)
    and meta_binding _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `And (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
                   (meta_binding _loc _a1))), (meta_binding _loc _a2))
      | `Bind (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Bind")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_expr _loc _a2))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result26)
    and meta_module_binding _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `And (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
                   (meta_module_binding _loc _a1))),
              (meta_module_binding _loc _a2))
      | `ModuleBind (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "ModuleBind")),
                             (meta_loc _loc _a0))), (meta_auident _loc _a1))),
                   (meta_module_type _loc _a2))),
              (meta_module_expr _loc _a3))
      | `Constraint (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "Constraint")),
                        (meta_loc _loc _a0))), (meta_auident _loc _a1))),
              (meta_module_type _loc _a2))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result25)
    and meta_match_case _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `Or (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Or")), (meta_loc _loc _a0))),
                   (meta_match_case _loc _a1))), (meta_match_case _loc _a2))
      | `Case (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "Case")), (meta_loc _loc _a0))),
                        (meta_patt _loc _a1))), (meta_expr _loc _a2))),
              (meta_expr _loc _a3))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result24)
    and meta_module_expr _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `Id (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Id")), (meta_loc _loc _a0))),
              (meta_ident _loc _a1))
      | `App (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
                   (meta_module_expr _loc _a1))),
              (meta_module_expr _loc _a2))
      | `Functor (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "Functor")),
                             (meta_loc _loc _a0))), (meta_auident _loc _a1))),
                   (meta_module_type _loc _a2))),
              (meta_module_expr _loc _a3))
      | `Struct (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Struct")), (meta_loc _loc _a0))),
              (meta_str_item _loc _a1))
      | `Constraint (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "Constraint")),
                        (meta_loc _loc _a0))), (meta_module_expr _loc _a1))),
              (meta_module_type _loc _a2))
      | `PackageModule (_a0,_a1) ->
          `App
            (_loc,
              (`App
                 (_loc, (`Vrn (_loc, "PackageModule")), (meta_loc _loc _a0))),
              (meta_expr _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result23)
    and meta_str_item _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `Class (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Class")), (meta_loc _loc _a0))),
              (meta_class_expr _loc _a1))
      | `ClassType (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "ClassType")), (meta_loc _loc _a0))),
              (meta_class_type _loc _a1))
      | `Sem (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
                   (meta_str_item _loc _a1))), (meta_str_item _loc _a2))
      | `Directive (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "Directive")), (meta_loc _loc _a0))),
                   (meta_alident _loc _a1))), (meta_expr _loc _a2))
      | `Exception (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Exception")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | `StExp (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "StExp")), (meta_loc _loc _a0))),
              (meta_expr _loc _a1))
      | `External (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "External")),
                             (meta_loc _loc _a0))), (meta_alident _loc _a1))),
                   (meta_ctyp _loc _a2))),
              (meta_meta_list meta_string _loc _a3))
      | `Include (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Include")), (meta_loc _loc _a0))),
              (meta_module_expr _loc _a1))
      | `Module (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Module")), (meta_loc _loc _a0))),
                   (meta_auident _loc _a1))), (meta_module_expr _loc _a2))
      | `RecModule (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "RecModule")), (meta_loc _loc _a0))),
              (meta_module_binding _loc _a1))
      | `ModuleType (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "ModuleType")),
                        (meta_loc _loc _a0))), (meta_auident _loc _a1))),
              (meta_module_type _loc _a2))
      | `Open (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Open")), (meta_loc _loc _a0))),
              (meta_ident _loc _a1))
      | `Type (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Type")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | `Value (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Value")), (meta_loc _loc _a0))),
                   (meta_rec_flag _loc _a1))), (meta_binding _loc _a2))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result22)
    and meta_class_type _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `CtCon (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "CtCon")),
                             (meta_loc _loc _a0))),
                        (meta_virtual_flag _loc _a1))),
                   (meta_ident _loc _a2))), (meta_ctyp _loc _a3))
      | `CtFun (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "CtFun")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_class_type _loc _a2))
      | `CtSig (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "CtSig")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_class_sig_item _loc _a2))
      | `And (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
                   (meta_class_type _loc _a1))), (meta_class_type _loc _a2))
      | `CtCol (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "CtCol")), (meta_loc _loc _a0))),
                   (meta_class_type _loc _a1))), (meta_class_type _loc _a2))
      | `CtEq (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "CtEq")), (meta_loc _loc _a0))),
                   (meta_class_type _loc _a1))), (meta_class_type _loc _a2))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result21)
    and meta_class_sig_item _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `Eq (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Eq")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `Sem (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
                   (meta_class_sig_item _loc _a1))),
              (meta_class_sig_item _loc _a2))
      | `SigInherit (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "SigInherit")), (meta_loc _loc _a0))),
              (meta_class_type _loc _a1))
      | `Method (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "Method")),
                             (meta_loc _loc _a0))), (meta_alident _loc _a1))),
                   (meta_private_flag _loc _a2))), (meta_ctyp _loc _a3))
      | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc,
                             (`App
                                (_loc, (`Vrn (_loc, "CgVal")),
                                  (meta_loc _loc _a0))),
                             (meta_alident _loc _a1))),
                        (meta_mutable_flag _loc _a2))),
                   (meta_virtual_flag _loc _a3))), (meta_ctyp _loc _a4))
      | `CgVir (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "CgVir")),
                             (meta_loc _loc _a0))), (meta_alident _loc _a1))),
                   (meta_private_flag _loc _a2))), (meta_ctyp _loc _a3))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result20)
    and meta_class_expr _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `CeApp (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "CeApp")), (meta_loc _loc _a0))),
                   (meta_class_expr _loc _a1))), (meta_expr _loc _a2))
      | `CeCon (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "CeCon")),
                             (meta_loc _loc _a0))),
                        (meta_virtual_flag _loc _a1))),
                   (meta_ident _loc _a2))), (meta_ctyp _loc _a3))
      | `CeFun (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "CeFun")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_class_expr _loc _a2))
      | `CeLet (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "CeLet")),
                             (meta_loc _loc _a0))), (meta_rec_flag _loc _a1))),
                   (meta_binding _loc _a2))), (meta_class_expr _loc _a3))
      | `Obj (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Obj")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_class_str_item _loc _a2))
      | `CeTyc (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "CeTyc")), (meta_loc _loc _a0))),
                   (meta_class_expr _loc _a1))), (meta_class_type _loc _a2))
      | `And (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
                   (meta_class_expr _loc _a1))), (meta_class_expr _loc _a2))
      | `Eq (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Eq")), (meta_loc _loc _a0))),
                   (meta_class_expr _loc _a1))), (meta_class_expr _loc _a2))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result19)
    and meta_class_str_item _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `Sem (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
                   (meta_class_str_item _loc _a1))),
              (meta_class_str_item _loc _a2))
      | `Eq (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Eq")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `Inherit (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "Inherit")),
                             (meta_loc _loc _a0))),
                        (meta_override_flag _loc _a1))),
                   (meta_class_expr _loc _a2))),
              (meta_meta_option meta_alident _loc _a3))
      | `Initializer (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Initializer")), (meta_loc _loc _a0))),
              (meta_expr _loc _a1))
      | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc,
                             (`App
                                (_loc,
                                  (`App
                                     (_loc, (`Vrn (_loc, "CrMth")),
                                       (meta_loc _loc _a0))),
                                  (meta_alident _loc _a1))),
                             (meta_override_flag _loc _a2))),
                        (meta_private_flag _loc _a3))), (meta_expr _loc _a4))),
              (meta_ctyp _loc _a5))
      | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc,
                             (`App
                                (_loc, (`Vrn (_loc, "CrVal")),
                                  (meta_loc _loc _a0))),
                             (meta_alident _loc _a1))),
                        (meta_override_flag _loc _a2))),
                   (meta_mutable_flag _loc _a3))), (meta_expr _loc _a4))
      | `CrVir (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "CrVir")),
                             (meta_loc _loc _a0))), (meta_alident _loc _a1))),
                   (meta_private_flag _loc _a2))), (meta_ctyp _loc _a3))
      | `CrVvr (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "CrVvr")),
                             (meta_loc _loc _a0))), (meta_alident _loc _a1))),
                   (meta_mutable_flag _loc _a2))), (meta_ctyp _loc _a3))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result18)
    let rec meta_ep _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `Id (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Id")), (meta_loc _loc _a0))),
              (meta_ident _loc _a1))
      | `App (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
                   (meta_ep _loc _a1))), (meta_ep _loc _a2))
      | `Vrn (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Vrn")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `Com (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Com")), (meta_loc _loc _a0))),
                   (meta_ep _loc _a1))), (meta_ep _loc _a2))
      | `Sem (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
                   (meta_ep _loc _a1))), (meta_ep _loc _a2))
      | `Tup (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Tup")), (meta_loc _loc _a0))),
              (meta_ep _loc _a1))
      | `Any _a0 -> `App (_loc, (`Vrn (_loc, "Any")), (meta_loc _loc _a0))
      | `Array (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Array")), (meta_loc _loc _a0))),
              (meta_ep _loc _a1))
      | `Record (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Record")), (meta_loc _loc _a0))),
              (meta_rec_bind _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result37)
      | #literal as _a0 -> (meta_literal _loc _a0 :>'result37)
    and meta_rec_bind _loc =
      function
      | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
      | `RecBind (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "RecBind")), (meta_loc _loc _a0))),
                   (meta_ident _loc _a1))), (meta_ep _loc _a2))
      | `Sem (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
                   (meta_rec_bind _loc _a1))), (meta_rec_bind _loc _a2))
      | `Any _a0 -> `App (_loc, (`Vrn (_loc, "Any")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result36)
  end
let rec is_module_longident =
  function
  | `Dot (_loc,_,i) -> is_module_longident i
  | `App (_loc,i1,i2) -> (is_module_longident i1) && (is_module_longident i2)
  | `Uid (_loc,_) -> true
  | _ -> false
let ident_of_expr =
  let error () =
    invalid_arg "ident_of_expr: this expression is not an identifier" in
  let rec self =
    function
    | `App (_loc,e1,e2) -> `App (_loc, (self e1), (self e2))
    | `Dot (_loc,e1,e2) -> `Dot (_loc, (self e1), (self e2))
    | `Id (_loc,`Lid (_,_)) -> error ()
    | `Id (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | `Id (_loc,i) -> i | `App (_loc,_,_) -> error () | t -> self t
let ident_of_ctyp =
  let error () = invalid_arg "ident_of_ctyp: this type is not an identifier" in
  let rec self =
    function
    | `App (_loc,t1,t2) -> `App (_loc, (self t1), (self t2))
    | `Id (_loc,`Lid (_,_)) -> error ()
    | `Id (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | `Id (_loc,i) -> i | t -> self t
let ident_of_patt =
  let error () =
    invalid_arg "ident_of_patt: this pattern is not an identifier" in
  let rec self =
    function
    | `App (_loc,p1,p2) -> `App (_loc, (self p1), (self p2))
    | `Id (_loc,`Lid (_,_)) -> error ()
    | `Id (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | `Id (_loc,i) -> i | p -> self p
let rec is_constructor =
  function
  | `Dot (_loc,_,i) -> is_constructor i
  | `Uid (_loc,_) -> true
  | `Lid (_loc,_)|`App (_loc,_,_) -> false
  | `Ant (_loc,_) -> assert false
let is_patt_constructor =
  function
  | `Id (_loc,i) -> is_constructor i
  | `Vrn (_loc,_) -> true
  | _ -> false
let rec is_expr_constructor =
  function
  | `Id (_loc,i) -> is_constructor i
  | `Dot (_loc,e1,e2) -> (is_expr_constructor e1) && (is_expr_constructor e2)
  | `Vrn (_loc,_) -> true
  | _ -> false
let rec or_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of t in `Or (_loc, t, (or_of_list ts))
let rec and_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of t in `And (_loc, t, (and_of_list ts))
let rec and_of_list' =
  function
  | [] -> failwithf "and_of_list' empty list"
  | t::[] -> t
  | t::ts ->
      let r = and_of_list' ts in
      let _loc = FanLoc.merge (loc_of t) (loc_of r) in `And (_loc, t, r)
let rec sem_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of t in `Sem (_loc, t, (sem_of_list ts))
let rec sem_of_list' =
  function
  | [] -> failwith "sem_of_list' empty list"
  | t::[] -> t
  | t::ts ->
      let r = sem_of_list' ts in
      let _loc = FanLoc.merge (loc_of t) (loc_of r) in `Sem (_loc, t, r)
let rec com_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of t in `Com (_loc, t, (com_of_list ts))
let rec com_of_list' =
  function
  | [] -> failwith "com_of_list' empty list"
  | t::[] -> t
  | t::ts -> let _loc = loc_of t in `Com (_loc, t, (com_of_list' ts))
let rec sta_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of t in `Sta (_loc, t, (sta_of_list ts))
let rec amp_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of t in `Amp (_loc, t, (amp_of_list ts))
let tup x = let _loc = loc_of x in `Tup (_loc, x)
let tuple_com y =
  match y with
  | [] -> failwith "tuple_com empty"
  | x::[] -> x
  | x::_ ->
      let a = loc_of x in
      let b = loc_of (List.last y) in
      let _loc = FanLoc.merge a b in `Tup (_loc, (com_of_list y))
let tuple_sta y =
  match y with
  | [] -> failwith "tuple_sta empty"
  | x::[] -> x
  | x::_ ->
      let a = loc_of x in
      let b = loc_of (List.last y) in
      let _loc = FanLoc.merge a b in `Tup (_loc, (sta_of_list y))
let rec dot_of_list' =
  function
  | [] -> assert false
  | i::[] -> i
  | i::is -> let _loc = loc_of i in `Dot (_loc, i, (dot_of_list' is))
let ty_of_stl =
  function
  | (_loc,s,[]) -> `Id (_loc, (`Uid (_loc, s)))
  | (_loc,s,tl) ->
      `Of (_loc, (`Id (_loc, (`Uid (_loc, s)))), (and_of_list tl))
let ty_of_sbt =
  function
  | (_loc,s,true ,t) ->
      `TyCol (_loc, (`Id (_loc, (`Lid (_loc, s)))), (`Mut (_loc, t)))
  | (_loc,s,false ,t) -> `TyCol (_loc, (`Id (_loc, (`Lid (_loc, s)))), t)
let bi_of_pe (p,e) = let _loc = loc_of p in `Bind (_loc, p, e)
let sum_type_of_list l = or_of_list (List.map ty_of_stl l)
let record_type_of_list l = sem_of_list (List.map ty_of_sbt l)
let binding_of_pel l = and_of_list (List.map bi_of_pe l)
let rec list_of_amp x acc =
  match x with
  | `And (_,x,y) -> list_of_amp x (list_of_amp y acc)
  | _ -> x :: acc
let rec list_of_amp' x acc =
  match x with
  | `And (_,x,y) -> list_of_amp' x (list_of_amp' y acc)
  | `Nil _ -> acc
  | _ -> x :: acc
let rec list_of_and x acc =
  match x with
  | `And (_,x,y) -> list_of_and x (list_of_and y acc)
  | _ -> x :: acc
let rec list_of_and' x acc =
  match x with
  | `And (_,x,y) -> list_of_and' x (list_of_and' y acc)
  | `Nil _ -> acc
  | _ -> x :: acc
let rec list_of_com x acc =
  match x with
  | `Com (_,x,y) -> list_of_com x (list_of_com y acc)
  | _ -> x :: acc
let rec list_of_com' x acc =
  match x with
  | `Com (_,x,y) -> list_of_com' x (list_of_com' y acc)
  | `Nil _ -> acc
  | _ -> x :: acc
let rec list_of_star' x acc =
  match x with
  | `Sta (_,x,y) -> list_of_star' x (list_of_star' y acc)
  | `Nil _ -> acc
  | _ -> x :: acc
let rec list_of_star x acc =
  match x with
  | `Sta (_,x,y) -> list_of_star x (list_of_star y acc)
  | _ -> x :: acc
let rec list_of_or x acc =
  match x with
  | `Or (_,x,y) -> list_of_or x (list_of_or y acc)
  | _ -> x :: acc
let rec list_of_or' x acc =
  match x with
  | `Or (_,x,y) -> list_of_or' x (list_of_or' y acc)
  | `Nil _ -> acc
  | _ -> x :: acc
let rec list_of_sem x acc =
  match x with
  | `Sem (_,x,y) -> list_of_sem x (list_of_sem y acc)
  | _ -> x :: acc
let rec list_of_sem' (x : 'a) acc =
  match x with
  | `Sem (_,x,y) -> list_of_sem' x (list_of_sem' y acc)
  | `Nil _ -> acc
  | y -> y :: acc
let sem a b =
  let _loc = FanLoc.merge (loc_of a) (loc_of b) in `Sem (_loc, a, b)
let com a b =
  let _loc = FanLoc.merge (loc_of a) (loc_of b) in `Com (_loc, a, b)
let app a b =
  let _loc = FanLoc.merge (loc_of a) (loc_of b) in `App (_loc, a, b)
let sta a b =
  let _loc = FanLoc.merge (loc_of a) (loc_of b) in `Sta (_loc, a, b)
let typing a b =
  let _loc = FanLoc.merge (loc_of a) (loc_of b) in `Constraint (_loc, a, b)
let seq a = let _loc = loc_of a in `Seq (_loc, a)
let seq_sem ls = seq (sem_of_list ls)
let rec list_of_app x acc =
  match x with
  | `App (_,t1,t2) -> list_of_app t1 (list_of_app t2 acc)
  | x -> x :: acc
let rec list_of_app' x acc =
  match x with
  | `App (_,t1,t2) -> list_of_app' t1 (list_of_app' t2 acc)
  | `Nil _ -> acc
  | x -> x :: acc
let rec appl_of_list x =
  match x with
  | [] -> `Nil ghost
  | x::[] -> x
  | x::y::xs -> appl_of_list ((app x y) :: xs)
let rec appl_of_list' x =
  match x with
  | [] -> failwith "appl_of_list' empty list"
  | x::[] -> x
  | x::y::xs -> appl_of_list' ((app x y) :: xs)
let rec view_app acc =
  function | `App (_,f,a) -> view_app (a :: acc) f | f -> (f, acc)
let map_expr f =
  object  inherit  Objs.map as super method! expr x = f (super#expr x) end
let map_patt f =
  object  inherit  Objs.map as super method! patt x = f (super#patt x) end
let map_ctyp f =
  object  inherit  Objs.map as super method! ctyp x = f (super#ctyp x) end
let map_str_item f =
  object 
    inherit  Objs.map as super
    method! str_item x = f (super#str_item x)
  end
let map_sig_item f =
  object 
    inherit  Objs.map as super
    method! sig_item x = f (super#sig_item x)
  end
let map_ctyp f =
  object  inherit  Objs.map as super method! ctyp x = f (super#ctyp x) end
let map_loc f =
  object  inherit  Objs.map as super method! loc x = f (super#loc x) end
class clean_ast =
  object 
    inherit  Objs.map as super
    method! with_constr wc =
      match super#with_constr wc with
      | `And (_loc,`Nil _l,wc)|`And (_loc,wc,`Nil _l) -> wc
      | wc -> wc
    method! expr e =
      match super#expr e with
      | `LetIn (_loc,_,`Nil _l,e)|`RecordWith (_loc,`Nil _l,e)
        |`Com (_loc,`Nil _l,e)|`Com (_loc,e,`Nil _l)|`Sem (_loc,`Nil _l,e)
        |`Sem (_loc,e,`Nil _l) -> e
      | e -> e
    method! patt p =
      match super#patt p with
      | `Or (_loc,`Nil _l,p)|`Or (_loc,p,`Nil _l)|`Com (_loc,`Nil _l,p)
        |`Com (_loc,p,`Nil _l)|`Sem (_loc,`Nil _l,p)|`Sem (_loc,p,`Nil _l) ->
          p
      | p -> p
    method! match_case mc =
      match super#match_case mc with
      | `Or (_loc,`Nil _l,mc)|`Or (_loc,mc,`Nil _l) -> mc
      | mc -> mc
    method! binding bi =
      match super#binding bi with
      | `And (_loc,`Nil _l,bi)|`And (_loc,bi,`Nil _l) -> bi
      | bi -> bi
    method! rec_expr rb =
      match super#rec_expr rb with
      | `Sem (_loc,`Nil _l,bi)|`Sem (_loc,bi,`Nil _l) -> bi
      | bi -> bi
    method! module_binding mb =
      match super#module_binding mb with
      | `And (_loc,`Nil _l,mb)|`And (_loc,mb,`Nil _l) -> mb
      | mb -> mb
    method! ctyp t =
      match super#ctyp t with
      | `TyPol (_loc,`Nil _l,t)|`Alias (_loc,`Nil _l,t)
        |`Alias (_loc,t,`Nil _l)|`Arrow (_loc,t,`Nil _l)
        |`Arrow (_loc,`Nil _l,t)|`Or (_loc,`Nil _l,t)|`Or (_loc,t,`Nil _l)
        |`Of (_loc,t,`Nil _l)|`And (_loc,`Nil _l,t)|`And (_loc,t,`Nil _l)
        |`Com (_loc,`Nil _l,t)|`Com (_loc,t,`Nil _l)|`Amp (_loc,t,`Nil _l)
        |`Amp (_loc,`Nil _l,t)|`Sta (_loc,`Nil _l,t)|`Sta (_loc,t,`Nil _l) ->
          t
      | t -> t
    method! sig_item sg =
      match super#sig_item sg with
      | `Sem (_loc,`Nil _l,sg)|`Sem (_loc,sg,`Nil _l) -> sg
      | `Type (_loc,`Nil _l) -> `Nil _loc
      | sg -> sg
    method! str_item st =
      match super#str_item st with
      | `Sem (_loc,`Nil _l,st)|`Sem (_loc,st,`Nil _l) -> st
      | `Type (_loc,`Nil _l) -> `Nil _loc
      | `Value (_loc,_,`Nil _l) -> `Nil _loc
      | st -> st
    method! module_type mt =
      match super#module_type mt with
      | `With (_loc,mt,`Nil _l) -> mt
      | mt -> mt
    method! class_expr ce =
      match super#class_expr ce with
      | `And (_loc,`Nil _l,ce)|`And (_loc,ce,`Nil _l) -> ce
      | ce -> ce
    method! class_type ct =
      match super#class_type ct with
      | `And (_loc,`Nil _l,ct)|`And (_loc,ct,`Nil _l) -> ct
      | ct -> ct
    method! class_sig_item csg =
      match super#class_sig_item csg with
      | `Sem (_loc,`Nil _l,csg)|`Sem (_loc,csg,`Nil _l) -> csg
      | csg -> csg
    method! class_str_item cst =
      match super#class_str_item cst with
      | `Sem (_loc,`Nil _l,cst)|`Sem (_loc,cst,`Nil _l) -> cst
      | cst -> cst
  end
class reloc _loc = object  inherit  Objs.map method! loc _ = _loc end
let wildcarder =
  object (self)
    inherit  Objs.map as super
    method! patt =
      function
      | `Id (_loc,`Lid (_,_)) -> `Any _loc
      | `Alias (_loc,p,_) -> self#patt p
      | p -> super#patt p
  end
let match_pre =
  object (self)
    inherit  Objs.map
    method! match_case =
      function
      | `Case (_loc,p,`Nil _,e) ->
          `Case
            (_loc, p, (`Nil _loc),
              (`Fun
                 (_loc,
                   (`Case
                      (_loc, (`Id (_loc, (`Uid (_loc, "()")))), (`Nil _loc),
                        e)))))
      | `Case (_loc,p,e,e1) ->
          `Case
            (_loc, p, e,
              (`Fun
                 (_loc,
                   (`Case
                      (_loc, (`Id (_loc, (`Uid (_loc, "()")))), (`Nil _loc),
                        e1)))))
      | `Or (_loc,a1,a2) ->
          `Or (_loc, (self#match_case a1), (self#match_case a2))
      | `Nil _loc -> `Nil _loc
      | `Ant (_loc,x) -> `Ant (_loc, (add_context x "lettry"))
  end
let rec is_irrefut_patt: patt -> bool =
  function
  | `Id (_loc,`Lid (_,_)) -> true
  | `Id (_loc,`Uid (_,"()")) -> true
  | `Any _loc -> true
  | `Nil _loc -> true
  | `Alias (_loc,x,_) -> is_irrefut_patt x
  | `Record (_loc,p) ->
      List.for_all
        (function | `RecBind (_,_,p) -> is_irrefut_patt p | _ -> true)
        (list_of_sem' p [])
  | `Sem (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `Com (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `Or (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `App (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `Constraint (_loc,p,_) -> is_irrefut_patt p
  | `Tup (_loc,pl) -> is_irrefut_patt pl
  | `PaOlbi (_loc,_,p,_) -> is_irrefut_patt p
  | `Label (_loc,_,`Nil _) -> true
  | `Label (_loc,_,p) -> is_irrefut_patt p
  | `Lazy (_loc,p) -> is_irrefut_patt p
  | `Id (_loc,_) -> false
  | `ModuleUnpack (_loc,_,_) -> true
  | `Vrn (_loc,_)|`Str (_loc,_)|`PaRng (_loc,_,_)|`Flo (_loc,_)
    |`NativeInt (_loc,_)|`Int64 (_loc,_)|`Int32 (_loc,_)|`Int (_loc,_)
    |`Chr (_loc,_)|`ClassPath (_loc,_)|`Array (_loc,_)|`Ant (_loc,_) -> false
let array_of_array arr =
  let items = (arr |> Array.to_list) |> sem_of_list in
  let _loc = loc_of items in `Array (_loc, items)
let meta_array mf_a _loc ls =
  array_of_array (Array.map (fun x  -> mf_a _loc x) ls)