open FanOps
open Ast
module type META_LOC = sig val meta_loc : loc -> loc -> ep end
let _ = (); ()
let _ = ()
module Make(MetaLoc:META_LOC) =
  struct
    include MetaLoc
    let meta_ant _loc (`Ant (_a0,_a1)) = `Ant (_a0, _a1)
    let meta_nil _loc (`Nil _a0) =
      `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
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
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result3)
    let meta_direction_flag _loc =
      function
      | `To _a0 -> `App (_loc, (`Vrn (_loc, "To")), (meta_loc _loc _a0))
      | `Downto _a0 ->
          `App (_loc, (`Vrn (_loc, "Downto")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result4)
    let meta_mutable_flag _loc =
      function
      | `Mutable _a0 ->
          `App (_loc, (`Vrn (_loc, "Mutable")), (meta_loc _loc _a0))
      | `MuNil _a0 ->
          `App (_loc, (`Vrn (_loc, "MuNil")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result5)
    let meta_private_flag _loc =
      function
      | `Private _a0 ->
          `App (_loc, (`Vrn (_loc, "Private")), (meta_loc _loc _a0))
      | `PrNil _a0 ->
          `App (_loc, (`Vrn (_loc, "PrNil")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result6)
    let meta_virtual_flag _loc =
      function
      | `Virtual _a0 ->
          `App (_loc, (`Vrn (_loc, "Virtual")), (meta_loc _loc _a0))
      | `ViNil _a0 ->
          `App (_loc, (`Vrn (_loc, "ViNil")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result7)
    let meta_override_flag _loc =
      function
      | `Override _a0 ->
          `App (_loc, (`Vrn (_loc, "Override")), (meta_loc _loc _a0))
      | `OvNil _a0 ->
          `App (_loc, (`Vrn (_loc, "OvNil")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result8)
    let meta_row_var_flag _loc =
      function
      | `RowVar _a0 ->
          `App (_loc, (`Vrn (_loc, "RowVar")), (meta_loc _loc _a0))
      | `RvNil _a0 ->
          `App (_loc, (`Vrn (_loc, "RvNil")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result9)
    let meta_position_flag _loc =
      function
      | `Positive _a0 ->
          `App (_loc, (`Vrn (_loc, "Positive")), (meta_loc _loc _a0))
      | `Negative _a0 ->
          `App (_loc, (`Vrn (_loc, "Negative")), (meta_loc _loc _a0))
      | `Normal _a0 ->
          `App (_loc, (`Vrn (_loc, "Normal")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result10)
    let meta_meta_bool _loc =
      function
      | `True _a0 -> `App (_loc, (`Vrn (_loc, "True")), (meta_loc _loc _a0))
      | `False _a0 ->
          `App (_loc, (`Vrn (_loc, "False")), (meta_loc _loc _a0))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result11)
    let rec meta_strings _loc =
      function
      | `App (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
                   (meta_strings _loc _a1))), (meta_strings _loc _a2))
      | `Str (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Str")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
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
    let rec meta_uident _loc =
      function
      | `Dot (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Dot")), (meta_loc _loc _a0))),
                   (meta_uident _loc _a1))), (meta_uident _loc _a2))
      | `App (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
                   (meta_uident _loc _a1))), (meta_uident _loc _a2))
      | #auident as _a0 -> (meta_auident _loc _a0 :>'result17)
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
      | #alident as _a0 -> (meta_alident _loc _a0 :>'result18)
      | #auident as _a0 -> (meta_auident _loc _a0 :>'result18)
    let rec meta_dupath _loc =
      function
      | `Dot (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Dot")), (meta_loc _loc _a0))),
                   (meta_dupath _loc _a1))), (meta_dupath _loc _a2))
      | #auident as _a0 -> (meta_auident _loc _a0 :>'result19)
    let meta_dlpath _loc =
      function
      | `Dot (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Dot")), (meta_loc _loc _a0))),
                   (meta_dupath _loc _a1))), (meta_alident _loc _a2))
      | #alident as _a0 -> (meta_alident _loc _a0 :>'result20)
    let meta_sid _loc (`Id (_a0,_a1)) =
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Id")), (meta_loc _loc _a0))),
          (meta_ident _loc _a1))
    let meta_any _loc (`Any _a0) =
      `App (_loc, (`Vrn (_loc, "Any")), (meta_loc _loc _a0))
    let rec meta_ctyp _loc =
      function
      | `Alias (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Alias")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_alident _loc _a2))
      | #any as _a0 -> (meta_any _loc _a0 :>'result48)
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
      | #sid as _a0 -> (meta_sid _loc _a0 :>'result48)
      | `TyObj (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "TyObj")), (meta_loc _loc _a0))),
                   (meta_name_ctyp _loc _a1))), (meta_row_var_flag _loc _a2))
      | `TyObjEnd (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "TyObjEnd")), (meta_loc _loc _a0))),
              (meta_row_var_flag _loc _a1))
      | `TyPol (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "TyPol")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `TyPolEnd (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "TyPolEnd")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
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
                   (meta_position_flag _loc _a1))), (meta_alident _loc _a2))
      | `QuoteAny (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "QuoteAny")), (meta_loc _loc _a0))),
              (meta_position_flag _loc _a1))
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
      | `PolyEq (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "PolyEq")), (meta_loc _loc _a0))),
              (meta_row_field _loc _a1))
      | `PolySup (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "PolySup")), (meta_loc _loc _a0))),
              (meta_row_field _loc _a1))
      | `PolyInf (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "PolyInf")), (meta_loc _loc _a0))),
              (meta_row_field _loc _a1))
      | `PolyInfSup (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "PolyInfSup")),
                        (meta_loc _loc _a0))), (meta_row_field _loc _a1))),
              (meta_tag_names _loc _a2))
      | `Package (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Package")), (meta_loc _loc _a0))),
              (meta_module_type _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result48)
    and meta_type_parameters _loc =
      function
      | `Com (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Com")), (meta_loc _loc _a0))),
                   (meta_type_parameters _loc _a1))),
              (meta_type_parameters _loc _a2))
      | `Ctyp (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Ctyp")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result47)
    and meta_row_field _loc =
      function
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result46)
      | `Or (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Or")), (meta_loc _loc _a0))),
                   (meta_row_field _loc _a1))), (meta_row_field _loc _a2))
      | `TyVrn (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "TyVrn")), (meta_loc _loc _a0))),
              (meta_astring _loc _a1))
      | `TyVrnOf (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "TyVrnOf")), (meta_loc _loc _a0))),
                   (meta_astring _loc _a1))), (meta_ctyp _loc _a2))
      | `Ctyp (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Ctyp")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
    and meta_tag_names _loc =
      function
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result45)
      | `App (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
                   (meta_tag_names _loc _a1))), (meta_tag_names _loc _a2))
      | `TyVrn (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "TyVrn")), (meta_loc _loc _a0))),
              (meta_astring _loc _a1))
    and meta_typedecl _loc =
      function
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
                   (meta_type_info _loc _a3))),
              (meta_list
                 (fun _loc  (_a0,_a1)  ->
                    `Tup
                      (_loc,
                        (`Com
                           (_loc, (meta_ctyp _loc _a0), (meta_ctyp _loc _a1)))))
                 _loc _a4))
      | `TyAbstr (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "TyAbstr")),
                             (meta_loc _loc _a0))), (meta_alident _loc _a1))),
                   (meta_list meta_ctyp _loc _a2))),
              (meta_list
                 (fun _loc  (_a0,_a1)  ->
                    `Tup
                      (_loc,
                        (`Com
                           (_loc, (meta_ctyp _loc _a0), (meta_ctyp _loc _a1)))))
                 _loc _a3))
      | `And (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
                   (meta_typedecl _loc _a1))), (meta_typedecl _loc _a2))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result44)
    and meta_type_info _loc =
      function
      | `TyMan (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "TyMan")),
                             (meta_loc _loc _a0))), (meta_ctyp _loc _a1))),
                   (meta_private_flag _loc _a2))), (meta_type_repr _loc _a3))
      | `TyRepr (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "TyRepr")), (meta_loc _loc _a0))),
                   (meta_private_flag _loc _a1))), (meta_type_repr _loc _a2))
      | `TyEq (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "TyEq")), (meta_loc _loc _a0))),
                   (meta_private_flag _loc _a1))), (meta_ctyp _loc _a2))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result43)
    and meta_type_repr _loc =
      function
      | `Record (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Record")), (meta_loc _loc _a0))),
              (meta_name_ctyp _loc _a1))
      | `Sum (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Sum")), (meta_loc _loc _a0))),
              (meta_or_ctyp _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result42)
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
                   (meta_sid _loc _a1))), (meta_ctyp _loc _a2))
      | `TyColMut (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "TyColMut")), (meta_loc _loc _a0))),
                   (meta_sid _loc _a1))), (meta_ctyp _loc _a2))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result41)
    and meta_or_ctyp _loc =
      function
      | `Or (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Or")), (meta_loc _loc _a0))),
                   (meta_or_ctyp _loc _a1))), (meta_or_ctyp _loc _a2))
      | `TyCol (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "TyCol")), (meta_loc _loc _a0))),
                   (meta_sid _loc _a1))), (meta_ctyp _loc _a2))
      | `Of (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Of")), (meta_loc _loc _a0))),
                   (meta_sid _loc _a1))), (meta_ctyp _loc _a2))
      | #sid as _a0 -> (meta_sid _loc _a0 :>'result40)
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result40)
    and meta_of_ctyp _loc =
      function
      | `Of (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Of")), (meta_loc _loc _a0))),
                   (meta_sid _loc _a1))), (meta_ctyp _loc _a2))
      | #sid as _a0 -> (meta_sid _loc _a0 :>'result39)
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result39)
    and meta_patt _loc =
      function
      | #sid as _a0 -> (meta_sid _loc _a0 :>'result38)
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
      | #any as _a0 -> (meta_any _loc _a0 :>'result38)
      | `Record (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Record")), (meta_loc _loc _a0))),
              (meta_rec_patt _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result38)
      | #literal as _a0 -> (meta_literal _loc _a0 :>'result38)
      | `Alias (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Alias")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_alident _loc _a2))
      | `ArrayEmpty _a0 ->
          `App (_loc, (`Vrn (_loc, "ArrayEmpty")), (meta_loc _loc _a0))
      | `Array (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Array")), (meta_loc _loc _a0))),
              (meta_patt _loc _a1))
      | `LabelS (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "LabelS")), (meta_loc _loc _a0))),
              (meta_alident _loc _a1))
      | `Label (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Label")), (meta_loc _loc _a0))),
                   (meta_alident _loc _a1))), (meta_patt _loc _a2))
      | `OptLabl (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "OptLabl")), (meta_loc _loc _a0))),
                   (meta_alident _loc _a1))), (meta_patt _loc _a2))
      | `OptLablS (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "OptLablS")), (meta_loc _loc _a0))),
              (meta_alident _loc _a1))
      | `OptLablExpr (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "OptLablExpr")),
                             (meta_loc _loc _a0))), (meta_alident _loc _a1))),
                   (meta_patt _loc _a2))), (meta_expr _loc _a3))
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
      | `ModuleUnpack (_a0,_a1) ->
          `App
            (_loc,
              (`App
                 (_loc, (`Vrn (_loc, "ModuleUnpack")), (meta_loc _loc _a0))),
              (meta_auident _loc _a1))
      | `ModuleConstraint (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "ModuleConstraint")),
                        (meta_loc _loc _a0))), (meta_auident _loc _a1))),
              (meta_ctyp _loc _a2))
    and meta_rec_patt _loc =
      function
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
      | #any as _a0 -> (meta_any _loc _a0 :>'result37)
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result37)
    and meta_expr _loc =
      function
      | #sid as _a0 -> (meta_sid _loc _a0 :>'result36)
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
      | #any as _a0 -> (meta_any _loc _a0 :>'result36)
      | `Record (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Record")), (meta_loc _loc _a0))),
              (meta_rec_expr _loc _a1))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result36)
      | #literal as _a0 -> (meta_literal _loc _a0 :>'result36)
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
      | `ArrayEmpty _a0 ->
          `App (_loc, (`Vrn (_loc, "ArrayEmpty")), (meta_loc _loc _a0))
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
      | `LabelS (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "LabelS")), (meta_loc _loc _a0))),
              (meta_alident _loc _a1))
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
      | `Obj (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Obj")), (meta_loc _loc _a0))),
              (meta_class_str_item _loc _a1))
      | `ObjEnd _a0 ->
          `App (_loc, (`Vrn (_loc, "ObjEnd")), (meta_loc _loc _a0))
      | `ObjPat (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "ObjPat")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_class_str_item _loc _a2))
      | `ObjPatEnd (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "ObjPatEnd")), (meta_loc _loc _a0))),
              (meta_patt _loc _a1))
      | `OptLabl (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "OptLabl")), (meta_loc _loc _a0))),
                   (meta_alident _loc _a1))), (meta_expr _loc _a2))
      | `OptLablS (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "OptLablS")), (meta_loc _loc _a0))),
              (meta_alident _loc _a1))
      | `OvrInst (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "OvrInst")), (meta_loc _loc _a0))),
              (meta_rec_expr _loc _a1))
      | `OvrInstEmpty _a0 ->
          `App (_loc, (`Vrn (_loc, "OvrInstEmpty")), (meta_loc _loc _a0))
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
      | `Subtype (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "Subtype")), (meta_loc _loc _a0))),
                   (meta_expr _loc _a1))), (meta_ctyp _loc _a2))
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
      | #any as _a0 -> (meta_any _loc _a0 :>'result35)
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result35)
    and meta_module_type _loc =
      function
      | #sid as _a0 -> (meta_sid _loc _a0 :>'result34)
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
              (meta_module_type _loc _a3))
      | `Sig (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Sig")), (meta_loc _loc _a0))),
              (meta_sig_item _loc _a1))
      | `SigEnd _a0 ->
          `App (_loc, (`Vrn (_loc, "SigEnd")), (meta_loc _loc _a0))
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
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result34)
    and meta_sig_item _loc =
      function
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
      | `DirectiveSimple (_a0,_a1) ->
          `App
            (_loc,
              (`App
                 (_loc, (`Vrn (_loc, "DirectiveSimple")),
                   (meta_loc _loc _a0))), (meta_alident _loc _a1))
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
              (meta_of_ctyp _loc _a1))
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
                   (meta_ctyp _loc _a2))), (meta_strings _loc _a3))
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
      | `ModuleTypeEnd (_a0,_a1) ->
          `App
            (_loc,
              (`App
                 (_loc, (`Vrn (_loc, "ModuleTypeEnd")), (meta_loc _loc _a0))),
              (meta_auident _loc _a1))
      | `Open (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Open")), (meta_loc _loc _a0))),
              (meta_ident _loc _a1))
      | `Type (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Type")), (meta_loc _loc _a0))),
              (meta_typedecl _loc _a1))
      | `Val (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Val")), (meta_loc _loc _a0))),
                   (meta_alident _loc _a1))), (meta_ctyp _loc _a2))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result33)
    and meta_with_constr _loc =
      function
      | `TypeEq (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "TypeEq")), (meta_loc _loc _a0))),
                   (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
      | `TypeEqPriv (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "TypeEqPriv")),
                        (meta_loc _loc _a0))), (meta_ctyp _loc _a1))),
              (meta_ctyp _loc _a2))
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
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result32)
    and meta_binding _loc =
      function
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
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result31)
    and meta_module_binding _loc =
      function
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
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result30)
    and meta_match_case _loc =
      function
      | `Or (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Or")), (meta_loc _loc _a0))),
                   (meta_match_case _loc _a1))), (meta_match_case _loc _a2))
      | `Case (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Case")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_expr _loc _a2))
      | `CaseWhen (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "CaseWhen")),
                             (meta_loc _loc _a0))), (meta_patt _loc _a1))),
                   (meta_expr _loc _a2))), (meta_expr _loc _a3))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result29)
    and meta_module_expr _loc =
      function
      | #sid as _a0 -> (meta_sid _loc _a0 :>'result28)
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
      | `StructEnd _a0 ->
          `App (_loc, (`Vrn (_loc, "StructEnd")), (meta_loc _loc _a0))
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
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result28)
    and meta_str_item _loc =
      function
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
      | `DirectiveSimple (_a0,_a1) ->
          `App
            (_loc,
              (`App
                 (_loc, (`Vrn (_loc, "DirectiveSimple")),
                   (meta_loc _loc _a0))), (meta_alident _loc _a1))
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
              (meta_of_ctyp _loc _a1))
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
                   (meta_ctyp _loc _a2))), (meta_strings _loc _a3))
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
              (meta_typedecl _loc _a1))
      | `Value (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Value")), (meta_loc _loc _a0))),
                   (meta_rec_flag _loc _a1))), (meta_binding _loc _a2))
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result27)
    and meta_class_type _loc =
      function
      | `ClassCon (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "ClassCon")),
                             (meta_loc _loc _a0))),
                        (meta_virtual_flag _loc _a1))),
                   (meta_ident _loc _a2))), (meta_type_parameters _loc _a3))
      | `ClassConS (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "ClassConS")), (meta_loc _loc _a0))),
                   (meta_virtual_flag _loc _a1))), (meta_ident _loc _a2))
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
      | `Obj (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Obj")), (meta_loc _loc _a0))),
              (meta_class_sig_item _loc _a1))
      | `CtSigEnd (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "CtSigEnd")), (meta_loc _loc _a0))),
              (meta_ctyp _loc _a1))
      | `ObjEnd _a0 ->
          `App (_loc, (`Vrn (_loc, "ObjEnd")), (meta_loc _loc _a0))
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
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result26)
    and meta_class_sig_item _loc =
      function
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
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result25)
    and meta_class_expr _loc =
      function
      | `CeApp (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "CeApp")), (meta_loc _loc _a0))),
                   (meta_class_expr _loc _a1))), (meta_expr _loc _a2))
      | `ClassCon (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "ClassCon")),
                             (meta_loc _loc _a0))),
                        (meta_virtual_flag _loc _a1))),
                   (meta_ident _loc _a2))), (meta_type_parameters _loc _a3))
      | `ClassConS (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "ClassConS")), (meta_loc _loc _a0))),
                   (meta_virtual_flag _loc _a1))), (meta_ident _loc _a2))
      | `CeFun (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "CeFun")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_class_expr _loc _a2))
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
                   (meta_binding _loc _a2))), (meta_class_expr _loc _a3))
      | `Obj (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Obj")), (meta_loc _loc _a0))),
              (meta_class_str_item _loc _a1))
      | `ObjEnd _a0 ->
          `App (_loc, (`Vrn (_loc, "ObjEnd")), (meta_loc _loc _a0))
      | `ObjPat (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "ObjPat")), (meta_loc _loc _a0))),
                   (meta_patt _loc _a1))), (meta_class_str_item _loc _a2))
      | `ObjPatEnd (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "ObjPatEnd")), (meta_loc _loc _a0))),
              (meta_patt _loc _a1))
      | `Constraint (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "Constraint")),
                        (meta_loc _loc _a0))), (meta_class_expr _loc _a1))),
              (meta_class_type _loc _a2))
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
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result24)
    and meta_class_str_item _loc =
      function
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
      | `Inherit (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "Inherit")), (meta_loc _loc _a0))),
                   (meta_override_flag _loc _a1))),
              (meta_class_expr _loc _a2))
      | `InheritAs (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "InheritAs")),
                             (meta_loc _loc _a0))),
                        (meta_override_flag _loc _a1))),
                   (meta_class_expr _loc _a2))), (meta_alident _loc _a3))
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
      | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc,
                             (`App
                                (_loc, (`Vrn (_loc, "CrMthS")),
                                  (meta_loc _loc _a0))),
                             (meta_alident _loc _a1))),
                        (meta_override_flag _loc _a2))),
                   (meta_private_flag _loc _a3))), (meta_expr _loc _a4))
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
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result23)
    let rec meta_ep _loc =
      function
      | #sid as _a0 -> (meta_sid _loc _a0 :>'result50)
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
      | #any as _a0 -> (meta_any _loc _a0 :>'result50)
      | `ArrayEmpty _a0 ->
          `App (_loc, (`Vrn (_loc, "ArrayEmpty")), (meta_loc _loc _a0))
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
      | #literal as _a0 -> (meta_literal _loc _a0 :>'result50)
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result50)
    and meta_rec_bind _loc =
      function
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
      | #any as _a0 -> (meta_any _loc _a0 :>'result49)
      | #ant as _a0 -> (meta_ant _loc _a0 :>'result49)
  end
include AstLoc
let match_pre =
  object (self)
    inherit  Objs.map
    method! match_case =
      function
      | `Case (_loc,p,e) ->
          `Case
            (_loc, p,
              (`Fun
                 (_loc, (`Case (_loc, (`Id (_loc, (`Uid (_loc, "()")))), e)))))
      | `CaseWhen (_loc,p,e,e1) ->
          `CaseWhen
            (_loc, p, e,
              (`Fun
                 (_loc,
                   (`Case (_loc, (`Id (_loc, (`Uid (_loc, "()")))), e1)))))
      | `Or (_loc,a1,a2) ->
          `Or (_loc, (self#match_case a1), (self#match_case a2))
      | `Ant (_loc,x) -> `Ant (_loc, (FanUtil.add_context x "lettry"))
  end