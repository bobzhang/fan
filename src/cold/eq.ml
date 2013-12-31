module Locf =
  struct include Locf
         let eq_t (_x : Locf.t) (_y : Locf.t) = true end
module Tokenf =
  struct
    include Tokenf
    let eq_ant (_x : Tokenf.ant) (_y : Tokenf.ant) = true
  end
open StdFan
open Astf
let eq_loc eta__002_ eta__003_ = Locf.eq_t eta__002_ eta__003_
let eq_ant curry__005_ curry__006_ =
  match (curry__005_, curry__006_) with
  | (`Ant (_a0,_a1),`Ant (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (Tokenf.eq_ant _a1 _b1)
let eq_literal curry__008_ curry__009_ =
  match (curry__008_, curry__009_) with
  | (`Chr (_a0,_a1),`Chr (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Int (_a0,_a1),`Int (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Int32 (_a0,_a1),`Int32 (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Int64 (_a0,_a1),`Int64 (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Flo (_a0,_a1),`Flo (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Nativeint (_a0,_a1),`Nativeint (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Str (_a0,_a1),`Str (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Bool (_a0,_a1),`Bool (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_bool _a1 _b1)
  | (`Unit _a0,`Unit _b0) -> eq_loc _a0 _b0
  | (_,_) -> false
let eq_flag curry__011_ curry__012_ =
  match (curry__011_, curry__012_) with
  | (`Positive _a0,`Positive _b0) -> eq_loc _a0 _b0
  | (`Negative _a0,`Negative _b0) -> eq_loc _a0 _b0
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__010_)
  | (_,_) -> false
let eq_position_flag curry__014_ curry__015_ =
  match (curry__014_, curry__015_) with
  | (`Positive _a0,`Positive _b0) -> eq_loc _a0 _b0
  | (`Negative _a0,`Negative _b0) -> eq_loc _a0 _b0
  | (`Normal _a0,`Normal _b0) -> eq_loc _a0 _b0
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__013_)
  | (_,_) -> false
let rec eq_strings curry__017_ curry__018_ =
  match (curry__017_, curry__018_) with
  | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_strings _a1 _b1)) && (eq_strings _a2 _b2)
  | (`Str (_a0,_a1),`Str (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__016_)
  | (_,_) -> false
let eq_lident curry__020_ curry__021_ =
  match (curry__020_, curry__021_) with
  | (`Lid (_a0,_a1),`Lid (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
let eq_alident curry__023_ curry__024_ =
  match (curry__023_, curry__024_) with
  | (`Lid (_a0,_a1),`Lid (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__022_)
  | (_,_) -> false
let eq_auident curry__026_ curry__027_ =
  match (curry__026_, curry__027_) with
  | (`Uid (_a0,_a1),`Uid (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__025_)
  | (_,_) -> false
let eq_aident curry__029_ curry__030_ =
  match (curry__029_, curry__030_) with
  | ((#alident as _a0),(#alident as _b0)) ->
      (eq_alident _a0 _b0 :>'result__028_)
  | ((#auident as _a0),(#auident as _b0)) ->
      (eq_auident _a0 _b0 :>'result__028_)
  | (_,_) -> false
let eq_astring curry__032_ curry__033_ =
  match (curry__032_, curry__033_) with
  | (`C (_a0,_a1),`C (_b0,_b1)) -> (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__031_)
  | (_,_) -> false
let rec eq_uident curry__035_ curry__036_ =
  match (curry__035_, curry__036_) with
  | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_uident _a1 _b1)) && (eq_uident _a2 _b2)
  | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_uident _a1 _b1)) && (eq_uident _a2 _b2)
  | ((#auident as _a0),(#auident as _b0)) ->
      (eq_auident _a0 _b0 :>'result__034_)
  | (_,_) -> false
let rec eq_ident curry__038_ curry__039_ =
  match (curry__038_, curry__039_) with
  | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ident _a1 _b1)) && (eq_ident _a2 _b2)
  | (`Apply (_a0,_a1,_a2),`Apply (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ident _a1 _b1)) && (eq_ident _a2 _b2)
  | ((#alident as _a0),(#alident as _b0)) ->
      (eq_alident _a0 _b0 :>'result__037_)
  | ((#auident as _a0),(#auident as _b0)) ->
      (eq_auident _a0 _b0 :>'result__037_)
  | (_,_) -> false
let eq_ident' curry__041_ curry__042_ =
  match (curry__041_, curry__042_) with
  | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ident _a1 _b1)) && (eq_ident _a2 _b2)
  | (`Apply (_a0,_a1,_a2),`Apply (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ident _a1 _b1)) && (eq_ident _a2 _b2)
  | (`Lid (_a0,_a1),`Lid (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Uid (_a0,_a1),`Uid (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (_,_) -> false
let rec eq_vid curry__044_ curry__045_ =
  match (curry__044_, curry__045_) with
  | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_vid _a1 _b1)) && (eq_vid _a2 _b2)
  | (`Lid (_a0,_a1),`Lid (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Uid (_a0,_a1),`Uid (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__043_)
  | (_,_) -> false
let eq_vid' curry__047_ curry__048_ =
  match (curry__047_, curry__048_) with
  | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_vid _a1 _b1)) && (eq_vid _a2 _b2)
  | (`Lid (_a0,_a1),`Lid (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Uid (_a0,_a1),`Uid (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (_,_) -> false
let rec eq_dupath curry__050_ curry__051_ =
  match (curry__050_, curry__051_) with
  | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_dupath _a1 _b1)) && (eq_dupath _a2 _b2)
  | ((#auident as _a0),(#auident as _b0)) ->
      (eq_auident _a0 _b0 :>'result__049_)
  | (_,_) -> false
let eq_dlpath curry__053_ curry__054_ =
  match (curry__053_, curry__054_) with
  | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_dupath _a1 _b1)) && (eq_alident _a2 _b2)
  | ((#alident as _a0),(#alident as _b0)) ->
      (eq_alident _a0 _b0 :>'result__052_)
  | (_,_) -> false
let eq_any curry__056_ curry__057_ =
  match (curry__056_, curry__057_) with
  | (`Any _a0,`Any _b0) -> eq_loc _a0 _b0
let rec eq_ctyp curry__155_ curry__156_ =
  match (curry__155_, curry__156_) with
  | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_alident _a2 _b2)
  | ((#any as _a0),(#any as _b0)) -> (eq_any _a0 _b0 :>'result__154_)
  | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`Arrow (_a0,_a1,_a2),`Arrow (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`ClassPath (_a0,_a1),`ClassPath (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_ident _a1 _b1)
  | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_ctyp _a2 _b2)
  | ((#ident' as _a0),(#ident' as _b0)) ->
      (eq_ident' _a0 _b0 :>'result__154_)
  | (`TyObj (_a0,_a1,_a2),`TyObj (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_name_ctyp _a1 _b1)) && (eq_flag _a2 _b2)
  | (`TyObjEnd (_a0,_a1),`TyObjEnd (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_flag _a1 _b1)
  | (`TyPol (_a0,_a1,_a2),`TyPol (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`TyPolEnd (_a0,_a1),`TyPolEnd (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)
  | (`TyTypePol (_a0,_a1,_a2),`TyTypePol (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`Quote (_a0,_a1,_a2),`Quote (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_position_flag _a1 _b1)) &&
        (eq_alident _a2 _b2)
  | (`QuoteAny (_a0,_a1),`QuoteAny (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_position_flag _a1 _b1)
  | (`Par (_a0,_a1),`Par (_b0,_b1)) -> (eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)
  | (`Sta (_a0,_a1,_a2),`Sta (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`PolyEq (_a0,_a1),`PolyEq (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_row_field _a1 _b1)
  | (`PolySup (_a0,_a1),`PolySup (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_row_field _a1 _b1)
  | (`PolyInf (_a0,_a1),`PolyInf (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_row_field _a1 _b1)
  | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`PolyInfSup (_a0,_a1,_a2),`PolyInfSup (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_row_field _a1 _b1)) && (eq_tag_names _a2 _b2)
  | (`Package (_a0,_a1),`Package (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_mtyp _a1 _b1)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__154_)
  | (_,_) -> false
and eq_type_parameters curry__152_ curry__153_ =
  match (curry__152_, curry__153_) with
  | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_type_parameters _a1 _b1)) &&
        (eq_type_parameters _a2 _b2)
  | (`Ctyp (_a0,_a1),`Ctyp (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__151_)
  | (_,_) -> false
and eq_row_field curry__149_ curry__150_ =
  match (curry__149_, curry__150_) with
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__148_)
  | (`Bar (_a0,_a1,_a2),`Bar (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_row_field _a1 _b1)) && (eq_row_field _a2 _b2)
  | (`TyVrn (_a0,_a1),`TyVrn (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_astring _a1 _b1)
  | (`TyVrnOf (_a0,_a1,_a2),`TyVrnOf (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_astring _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`Ctyp (_a0,_a1),`Ctyp (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)
  | (_,_) -> false
and eq_tag_names curry__146_ curry__147_ =
  match (curry__146_, curry__147_) with
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__145_)
  | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_tag_names _a1 _b1)) && (eq_tag_names _a2 _b2)
  | (`TyVrn (_a0,_a1),`TyVrn (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_astring _a1 _b1)
  | (_,_) -> false
and eq_decl curry__143_ curry__144_ =
  match (curry__143_, curry__144_) with
  | (`TyDcl (_a0,_a1,_a2,_a3,_a4),`TyDcl (_b0,_b1,_b2,_b3,_b4)) ->
      ((((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) &&
          (eq_opt_decl_params _a2 _b2))
         && (eq_type_info _a3 _b3))
        && (eq_opt_type_constr _a4 _b4)
  | (`TyAbstr (_a0,_a1,_a2,_a3),`TyAbstr (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) &&
         (eq_opt_decl_params _a2 _b2))
        && (eq_opt_type_constr _a3 _b3)
  | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_decl _a1 _b1)) && (eq_decl _a2 _b2)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__142_)
  | (_,_) -> false
and eq_type_constr curry__140_ curry__141_ =
  match (curry__140_, curry__141_) with
  | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_type_constr _a1 _b1)) &&
        (eq_type_constr _a2 _b2)
  | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__139_)
  | (_,_) -> false
and eq_opt_type_constr curry__137_ curry__138_ =
  match (curry__137_, curry__138_) with
  | (`Some (_a0,_a1),`Some (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_type_constr _a1 _b1)
  | (`None _a0,`None _b0) -> eq_loc _a0 _b0
  | (_,_) -> false
and eq_decl_param curry__134_ curry__135_ =
  match (curry__134_, curry__135_) with
  | (`Quote (_a0,_a1,_a2),`Quote (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_position_flag _a1 _b1)) &&
        (eq_alident _a2 _b2)
  | (`QuoteAny (_a0,_a1),`QuoteAny (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_position_flag _a1 _b1)
  | (`Any _a0,`Any _b0) -> eq_loc _a0 _b0
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__133_)
  | (_,_) -> false
and eq_decl_params curry__131_ curry__132_ =
  match (curry__131_, curry__132_) with
  | (`Quote (_a0,_a1,_a2),`Quote (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_position_flag _a1 _b1)) &&
        (eq_alident _a2 _b2)
  | (`QuoteAny (_a0,_a1),`QuoteAny (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_position_flag _a1 _b1)
  | (`Any _a0,`Any _b0) -> eq_loc _a0 _b0
  | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_decl_params _a1 _b1)) &&
        (eq_decl_params _a2 _b2)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__130_)
  | (_,_) -> false
and eq_opt_decl_params curry__128_ curry__129_ =
  match (curry__128_, curry__129_) with
  | (`Some (_a0,_a1),`Some (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_decl_params _a1 _b1)
  | (`None _a0,`None _b0) -> eq_loc _a0 _b0
  | (_,_) -> false
and eq_type_info curry__125_ curry__126_ =
  match (curry__125_, curry__126_) with
  | (`TyMan (_a0,_a1,_a2,_a3),`TyMan (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_flag _a2 _b2)) &&
        (eq_type_repr _a3 _b3)
  | (`TyRepr (_a0,_a1,_a2),`TyRepr (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_type_repr _a2 _b2)
  | (`TyEq (_a0,_a1,_a2),`TyEq (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_ctyp _a2 _b2)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__124_)
  | (_,_) -> false
and eq_type_repr curry__122_ curry__123_ =
  match (curry__122_, curry__123_) with
  | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_name_ctyp _a1 _b1)
  | (`Sum (_a0,_a1),`Sum (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_or_ctyp _a1 _b1)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__121_)
  | (_,_) -> false
and eq_name_ctyp curry__119_ curry__120_ =
  match (curry__119_, curry__120_) with
  | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_name_ctyp _a1 _b1)) && (eq_name_ctyp _a2 _b2)
  | (`RecCol (_a0,_a1,_a2,_a3),`RecCol (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_ctyp _a2 _b2)) &&
        (eq_flag _a3 _b3)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__118_)
  | (_,_) -> false
and eq_or_ctyp curry__116_ curry__117_ =
  match (curry__116_, curry__117_) with
  | (`Bar (_a0,_a1,_a2),`Bar (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_or_ctyp _a1 _b1)) && (eq_or_ctyp _a2 _b2)
  | (`TyCol (_a0,_a1,_a2),`TyCol (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_auident _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_auident _a1 _b1)) && (eq_ctyp _a2 _b2)
  | ((#auident as _a0),(#auident as _b0)) ->
      (eq_auident _a0 _b0 :>'result__115_)
  | (_,_) -> false
and eq_of_ctyp curry__113_ curry__114_ =
  match (curry__113_, curry__114_) with
  | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_vid _a1 _b1)) && (eq_ctyp _a2 _b2)
  | ((#vid' as _a0),(#vid' as _b0)) -> (eq_vid' _a0 _b0 :>'result__112_)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__112_)
  | (_,_) -> false
and eq_pat curry__110_ curry__111_ =
  match (curry__110_, curry__111_) with
  | ((#vid as _a0),(#vid as _b0)) -> (eq_vid _a0 _b0 :>'result__109_)
  | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_pat _a2 _b2)
  | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_pat _a2 _b2)
  | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_pat _a2 _b2)
  | (`Par (_a0,_a1),`Par (_b0,_b1)) -> (eq_loc _a0 _b0) && (eq_pat _a1 _b1)
  | ((#any as _a0),(#any as _b0)) -> (eq_any _a0 _b0 :>'result__109_)
  | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_rec_pat _a1 _b1)
  | ((#literal as _a0),(#literal as _b0)) ->
      (eq_literal _a0 _b0 :>'result__109_)
  | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_alident _a2 _b2)
  | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> eq_loc _a0 _b0
  | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_pat _a1 _b1)
  | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_alident _a1 _b1)
  | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_pat _a2 _b2)
  | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_pat _a2 _b2)
  | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_alident _a1 _b1)
  | (`OptLablExpr (_a0,_a1,_a2,_a3),`OptLablExpr (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_pat _a2 _b2)) &&
        (eq_exp _a3 _b3)
  | (`Bar (_a0,_a1,_a2),`Bar (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_pat _a2 _b2)
  | (`PaRng (_a0,_a1,_a2),`PaRng (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_pat _a2 _b2)
  | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`ClassPath (_a0,_a1),`ClassPath (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_ident _a1 _b1)
  | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) -> (eq_loc _a0 _b0) && (eq_pat _a1 _b1)
  | (`ModuleUnpack (_a0,_a1),`ModuleUnpack (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_auident _a1 _b1)
  | (`ModuleConstraint (_a0,_a1,_a2),`ModuleConstraint (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_auident _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (_,_) -> false
and eq_rec_pat curry__107_ curry__108_ =
  match (curry__107_, curry__108_) with
  | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_vid _a1 _b1)) && (eq_pat _a2 _b2)
  | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_rec_pat _a1 _b1)) && (eq_rec_pat _a2 _b2)
  | ((#any as _a0),(#any as _b0)) -> (eq_any _a0 _b0 :>'result__106_)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__106_)
  | (_,_) -> false
and eq_exp curry__104_ curry__105_ =
  match (curry__104_, curry__105_) with
  | ((#vid as _a0),(#vid as _b0)) -> (eq_vid _a0 _b0 :>'result__103_)
  | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_exp _a2 _b2)
  | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_exp _a2 _b2)
  | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_exp _a2 _b2)
  | (`Par (_a0,_a1),`Par (_b0,_b1)) -> (eq_loc _a0 _b0) && (eq_exp _a1 _b1)
  | ((#any as _a0),(#any as _b0)) -> (eq_any _a0 _b0 :>'result__103_)
  | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_rec_exp _a1 _b1)
  | ((#literal as _a0),(#literal as _b0)) ->
      (eq_literal _a0 _b0 :>'result__103_)
  | (`RecordWith (_a0,_a1,_a2),`RecordWith (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_rec_exp _a1 _b1)) && (eq_exp _a2 _b2)
  | (`Field (_a0,_a1,_a2),`Field (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_vid _a2 _b2)
  | (`ArrayDot (_a0,_a1,_a2),`ArrayDot (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_exp _a2 _b2)
  | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> eq_loc _a0 _b0
  | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_exp _a1 _b1)
  | (`Assert (_a0,_a1),`Assert (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_exp _a1 _b1)
  | (`Assign (_a0,_a1,_a2),`Assign (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_exp _a2 _b2)
  | (`For (_a0,_a1,_a2,_a3,_a4,_a5),`For (_b0,_b1,_b2,_b3,_b4,_b5)) ->
      (((((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_exp _a2 _b2)) &&
          (eq_exp _a3 _b3))
         && (eq_flag _a4 _b4))
        && (eq_exp _a5 _b5)
  | (`Fun (_a0,_a1),`Fun (_b0,_b1)) -> (eq_loc _a0 _b0) && (eq_case _a1 _b1)
  | (`IfThenElse (_a0,_a1,_a2,_a3),`IfThenElse (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_exp _a2 _b2)) &&
        (eq_exp _a3 _b3)
  | (`IfThen (_a0,_a1,_a2),`IfThen (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_exp _a2 _b2)
  | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_alident _a1 _b1)
  | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_exp _a2 _b2)
  | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) -> (eq_loc _a0 _b0) && (eq_exp _a1 _b1)
  | (`LetIn (_a0,_a1,_a2,_a3),`LetIn (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_bind _a2 _b2)) &&
        (eq_exp _a3 _b3)
  | (`LetTryInWith (_a0,_a1,_a2,_a3,_a4),`LetTryInWith (_b0,_b1,_b2,_b3,_b4))
      ->
      ((((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_bind _a2 _b2)) &&
         (eq_exp _a3 _b3))
        && (eq_case _a4 _b4)
  | (`LetModule (_a0,_a1,_a2,_a3),`LetModule (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_auident _a1 _b1)) && (eq_mexp _a2 _b2)) &&
        (eq_exp _a3 _b3)
  | (`Match (_a0,_a1,_a2),`Match (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_case _a2 _b2)
  | (`New (_a0,_a1),`New (_b0,_b1)) -> (eq_loc _a0 _b0) && (eq_ident _a1 _b1)
  | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_clfield _a1 _b1)
  | (`ObjEnd _a0,`ObjEnd _b0) -> eq_loc _a0 _b0
  | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_clfield _a2 _b2)
  | (`ObjPatEnd (_a0,_a1),`ObjPatEnd (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_pat _a1 _b1)
  | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_exp _a2 _b2)
  | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_alident _a1 _b1)
  | (`OvrInst (_a0,_a1),`OvrInst (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_rec_exp _a1 _b1)
  | (`OvrInstEmpty _a0,`OvrInstEmpty _b0) -> eq_loc _a0 _b0
  | (`Seq (_a0,_a1),`Seq (_b0,_b1)) -> (eq_loc _a0 _b0) && (eq_exp _a1 _b1)
  | (`Send (_a0,_a1,_a2),`Send (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_alident _a2 _b2)
  | (`StringDot (_a0,_a1,_a2),`StringDot (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_exp _a2 _b2)
  | (`Try (_a0,_a1,_a2),`Try (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_case _a2 _b2)
  | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`Coercion (_a0,_a1,_a2,_a3),`Coercion (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_ctyp _a2 _b2)) &&
        (eq_ctyp _a3 _b3)
  | (`Subtype (_a0,_a1,_a2),`Subtype (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`While (_a0,_a1,_a2),`While (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_exp _a1 _b1)) && (eq_exp _a2 _b2)
  | (`LetOpen (_a0,_a1,_a2,_a3),`LetOpen (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_ident _a2 _b2)) &&
        (eq_exp _a3 _b3)
  | (`LocalTypeFun (_a0,_a1,_a2),`LocalTypeFun (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_exp _a2 _b2)
  | (`Package_exp (_a0,_a1),`Package_exp (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_mexp _a1 _b1)
  | (_,_) -> false
and eq_rec_exp curry__101_ curry__102_ =
  match (curry__101_, curry__102_) with
  | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_rec_exp _a1 _b1)) && (eq_rec_exp _a2 _b2)
  | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_vid _a1 _b1)) && (eq_exp _a2 _b2)
  | ((#any as _a0),(#any as _b0)) -> (eq_any _a0 _b0 :>'result__100_)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__100_)
  | (_,_) -> false
and eq_mtyp curry__098_ curry__099_ =
  match (curry__098_, curry__099_) with
  | ((#ident' as _a0),(#ident' as _b0)) ->
      (eq_ident' _a0 _b0 :>'result__097_)
  | (`Sig (_a0,_a1),`Sig (_b0,_b1)) -> (eq_loc _a0 _b0) && (eq_sigi _a1 _b1)
  | (`SigEnd _a0,`SigEnd _b0) -> eq_loc _a0 _b0
  | (`Functor (_a0,_a1,_a2,_a3),`Functor (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_auident _a1 _b1)) && (eq_mtyp _a2 _b2)) &&
        (eq_mtyp _a3 _b3)
  | (`With (_a0,_a1,_a2),`With (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_mtyp _a1 _b1)) && (eq_constr _a2 _b2)
  | (`ModuleTypeOf (_a0,_a1),`ModuleTypeOf (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_mexp _a1 _b1)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__097_)
  | (_,_) -> false
and eq_sigi curry__095_ curry__096_ =
  match (curry__095_, curry__096_) with
  | (`Val (_a0,_a1,_a2),`Val (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`External (_a0,_a1,_a2,_a3),`External (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_ctyp _a2 _b2)) &&
        (eq_strings _a3 _b3)
  | (`Type (_a0,_a1),`Type (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_decl _a1 _b1)
  | (`Exception (_a0,_a1),`Exception (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_of_ctyp _a1 _b1)
  | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_cltdecl _a1 _b1)
  | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_cltdecl _a1 _b1)
  | (`Module (_a0,_a1,_a2),`Module (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_auident _a1 _b1)) && (eq_mtyp _a2 _b2)
  | (`ModuleTypeEnd (_a0,_a1),`ModuleTypeEnd (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_auident _a1 _b1)
  | (`ModuleType (_a0,_a1,_a2),`ModuleType (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_auident _a1 _b1)) && (eq_mtyp _a2 _b2)
  | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_sigi _a1 _b1)) && (eq_sigi _a2 _b2)
  | (`DirectiveSimple (_a0,_a1),`DirectiveSimple (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_alident _a1 _b1)
  | (`Directive (_a0,_a1,_a2),`Directive (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_exp _a2 _b2)
  | (`Open (_a0,_a1,_a2),`Open (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_ident _a2 _b2)
  | (`Include (_a0,_a1),`Include (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_mtyp _a1 _b1)
  | (`RecModule (_a0,_a1),`RecModule (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_mbind _a1 _b1)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__094_)
  | (_,_) -> false
and eq_mbind curry__092_ curry__093_ =
  match (curry__092_, curry__093_) with
  | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_mbind _a1 _b1)) && (eq_mbind _a2 _b2)
  | (`ModuleBind (_a0,_a1,_a2,_a3),`ModuleBind (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_auident _a1 _b1)) && (eq_mtyp _a2 _b2)) &&
        (eq_mexp _a3 _b3)
  | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_auident _a1 _b1)) && (eq_mtyp _a2 _b2)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__091_)
  | (_,_) -> false
and eq_constr curry__089_ curry__090_ =
  match (curry__089_, curry__090_) with
  | (`TypeEq (_a0,_a1,_a2),`TypeEq (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`ModuleEq (_a0,_a1,_a2),`ModuleEq (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ident _a1 _b1)) && (eq_ident _a2 _b2)
  | (`TypeEqPriv (_a0,_a1,_a2),`TypeEqPriv (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`TypeSubst (_a0,_a1,_a2),`TypeSubst (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`ModuleSubst (_a0,_a1,_a2),`ModuleSubst (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ident _a1 _b1)) && (eq_ident _a2 _b2)
  | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_constr _a1 _b1)) && (eq_constr _a2 _b2)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__088_)
  | (_,_) -> false
and eq_bind curry__086_ curry__087_ =
  match (curry__086_, curry__087_) with
  | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_bind _a1 _b1)) && (eq_bind _a2 _b2)
  | (`Bind (_a0,_a1,_a2),`Bind (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_exp _a2 _b2)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__085_)
  | (_,_) -> false
and eq_case curry__083_ curry__084_ =
  match (curry__083_, curry__084_) with
  | (`Bar (_a0,_a1,_a2),`Bar (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_case _a1 _b1)) && (eq_case _a2 _b2)
  | (`Case (_a0,_a1,_a2),`Case (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_exp _a2 _b2)
  | (`CaseWhen (_a0,_a1,_a2,_a3),`CaseWhen (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_exp _a2 _b2)) &&
        (eq_exp _a3 _b3)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__082_)
  | (_,_) -> false
and eq_mexp curry__080_ curry__081_ =
  match (curry__080_, curry__081_) with
  | ((#vid' as _a0),(#vid' as _b0)) -> (eq_vid' _a0 _b0 :>'result__079_)
  | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_mexp _a1 _b1)) && (eq_mexp _a2 _b2)
  | (`Functor (_a0,_a1,_a2,_a3),`Functor (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_auident _a1 _b1)) && (eq_mtyp _a2 _b2)) &&
        (eq_mexp _a3 _b3)
  | (`Struct (_a0,_a1),`Struct (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_stru _a1 _b1)
  | (`StructEnd _a0,`StructEnd _b0) -> eq_loc _a0 _b0
  | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_mexp _a1 _b1)) && (eq_mtyp _a2 _b2)
  | (`PackageModule (_a0,_a1),`PackageModule (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_exp _a1 _b1)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__079_)
  | (_,_) -> false
and eq_stru curry__077_ curry__078_ =
  match (curry__077_, curry__078_) with
  | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_cldecl _a1 _b1)
  | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_cltdecl _a1 _b1)
  | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_stru _a1 _b1)) && (eq_stru _a2 _b2)
  | (`DirectiveSimple (_a0,_a1),`DirectiveSimple (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_alident _a1 _b1)
  | (`Directive (_a0,_a1,_a2),`Directive (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_exp _a2 _b2)
  | (`Exception (_a0,_a1),`Exception (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_of_ctyp _a1 _b1)
  | (`StExp (_a0,_a1),`StExp (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_exp _a1 _b1)
  | (`External (_a0,_a1,_a2,_a3),`External (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_ctyp _a2 _b2)) &&
        (eq_strings _a3 _b3)
  | (`Include (_a0,_a1),`Include (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_mexp _a1 _b1)
  | (`Module (_a0,_a1,_a2),`Module (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_auident _a1 _b1)) && (eq_mexp _a2 _b2)
  | (`RecModule (_a0,_a1),`RecModule (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_mbind _a1 _b1)
  | (`ModuleType (_a0,_a1,_a2),`ModuleType (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_auident _a1 _b1)) && (eq_mtyp _a2 _b2)
  | (`Open (_a0,_a1,_a2),`Open (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_ident _a2 _b2)
  | (`Type (_a0,_a1),`Type (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_decl _a1 _b1)
  | (`TypeWith (_a0,_a1,_a2),`TypeWith (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_decl _a1 _b1)) && (eq_strings _a2 _b2)
  | (`Value (_a0,_a1,_a2),`Value (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_bind _a2 _b2)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__076_)
  | (_,_) -> false
and eq_cltdecl curry__074_ curry__075_ =
  match (curry__074_, curry__075_) with
  | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_cltdecl _a1 _b1)) && (eq_cltdecl _a2 _b2)
  | (`CtDecl (_a0,_a1,_a2,_a3,_a4),`CtDecl (_b0,_b1,_b2,_b3,_b4)) ->
      ((((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_ident _a2 _b2)) &&
         (eq_type_parameters _a3 _b3))
        && (eq_cltyp _a4 _b4)
  | (`CtDeclS (_a0,_a1,_a2,_a3),`CtDeclS (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_ident _a2 _b2)) &&
        (eq_cltyp _a3 _b3)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__073_)
  | (_,_) -> false
and eq_cltyp curry__071_ curry__072_ =
  match (curry__071_, curry__072_) with
  | ((#vid' as _a0),(#vid' as _b0)) -> (eq_vid' _a0 _b0 :>'result__070_)
  | (`ClApply (_a0,_a1,_a2),`ClApply (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_vid _a1 _b1)) && (eq_type_parameters _a2 _b2)
  | (`CtFun (_a0,_a1,_a2),`CtFun (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_cltyp _a2 _b2)
  | (`ObjTy (_a0,_a1,_a2),`ObjTy (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_clsigi _a2 _b2)
  | (`ObjTyEnd (_a0,_a1),`ObjTyEnd (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)
  | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_clsigi _a1 _b1)
  | (`ObjEnd _a0,`ObjEnd _b0) -> eq_loc _a0 _b0
  | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_cltyp _a1 _b1)) && (eq_cltyp _a2 _b2)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__070_)
  | (_,_) -> false
and eq_clsigi curry__068_ curry__069_ =
  match (curry__068_, curry__069_) with
  | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_clsigi _a1 _b1)) && (eq_clsigi _a2 _b2)
  | (`SigInherit (_a0,_a1),`SigInherit (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_cltyp _a1 _b1)
  | (`CgVal (_a0,_a1,_a2,_a3,_a4),`CgVal (_b0,_b1,_b2,_b3,_b4)) ->
      ((((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_flag _a2 _b2)) &&
         (eq_flag _a3 _b3))
        && (eq_ctyp _a4 _b4)
  | (`Method (_a0,_a1,_a2,_a3),`Method (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_flag _a2 _b2)) &&
        (eq_ctyp _a3 _b3)
  | (`VirMeth (_a0,_a1,_a2,_a3),`VirMeth (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_flag _a2 _b2)) &&
        (eq_ctyp _a3 _b3)
  | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__067_)
  | (_,_) -> false
and eq_cldecl curry__065_ curry__066_ =
  match (curry__065_, curry__066_) with
  | (`ClDecl (_a0,_a1,_a2,_a3,_a4),`ClDecl (_b0,_b1,_b2,_b3,_b4)) ->
      ((((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_ident _a2 _b2)) &&
         (eq_type_parameters _a3 _b3))
        && (eq_clexp _a4 _b4)
  | (`ClDeclS (_a0,_a1,_a2,_a3),`ClDeclS (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_ident _a2 _b2)) &&
        (eq_clexp _a3 _b3)
  | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_cldecl _a1 _b1)) && (eq_cldecl _a2 _b2)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__064_)
  | (_,_) -> false
and eq_clexp curry__062_ curry__063_ =
  match (curry__062_, curry__063_) with
  | (`CeApp (_a0,_a1,_a2),`CeApp (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_clexp _a1 _b1)) && (eq_exp _a2 _b2)
  | ((#vid' as _a0),(#vid' as _b0)) -> (eq_vid' _a0 _b0 :>'result__061_)
  | (`ClApply (_a0,_a1,_a2),`ClApply (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_vid _a1 _b1)) && (eq_type_parameters _a2 _b2)
  | (`CeFun (_a0,_a1,_a2),`CeFun (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_clexp _a2 _b2)
  | (`LetIn (_a0,_a1,_a2,_a3),`LetIn (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_bind _a2 _b2)) &&
        (eq_clexp _a3 _b3)
  | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_clfield _a1 _b1)
  | (`ObjEnd _a0,`ObjEnd _b0) -> eq_loc _a0 _b0
  | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_pat _a1 _b1)) && (eq_clfield _a2 _b2)
  | (`ObjPatEnd (_a0,_a1),`ObjPatEnd (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_pat _a1 _b1)
  | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_clexp _a1 _b1)) && (eq_cltyp _a2 _b2)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__061_)
  | (_,_) -> false
and eq_clfield curry__059_ curry__060_ =
  match (curry__059_, curry__060_) with
  | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_clfield _a1 _b1)) && (eq_clfield _a2 _b2)
  | (`Inherit (_a0,_a1,_a2),`Inherit (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_clexp _a2 _b2)
  | (`InheritAs (_a0,_a1,_a2,_a3),`InheritAs (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_flag _a1 _b1)) && (eq_clexp _a2 _b2)) &&
        (eq_alident _a3 _b3)
  | (`CrVal (_a0,_a1,_a2,_a3,_a4),`CrVal (_b0,_b1,_b2,_b3,_b4)) ->
      ((((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_flag _a2 _b2)) &&
         (eq_flag _a3 _b3))
        && (eq_exp _a4 _b4)
  | (`VirVal (_a0,_a1,_a2,_a3),`VirVal (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_flag _a2 _b2)) &&
        (eq_ctyp _a3 _b3)
  | (`CrMth (_a0,_a1,_a2,_a3,_a4,_a5),`CrMth (_b0,_b1,_b2,_b3,_b4,_b5)) ->
      (((((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_flag _a2 _b2)) &&
          (eq_flag _a3 _b3))
         && (eq_exp _a4 _b4))
        && (eq_ctyp _a5 _b5)
  | (`CrMthS (_a0,_a1,_a2,_a3,_a4),`CrMthS (_b0,_b1,_b2,_b3,_b4)) ->
      ((((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_flag _a2 _b2)) &&
         (eq_flag _a3 _b3))
        && (eq_exp _a4 _b4)
  | (`VirMeth (_a0,_a1,_a2,_a3),`VirMeth (_b0,_b1,_b2,_b3)) ->
      (((eq_loc _a0 _b0) && (eq_alident _a1 _b1)) && (eq_flag _a2 _b2)) &&
        (eq_ctyp _a3 _b3)
  | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ctyp _a1 _b1)) && (eq_ctyp _a2 _b2)
  | (`Initializer (_a0,_a1),`Initializer (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_exp _a1 _b1)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__058_)
  | (_,_) -> false
let rec eq_ep curry__161_ curry__162_ =
  match (curry__161_, curry__162_) with
  | ((#vid as _a0),(#vid as _b0)) -> (eq_vid _a0 _b0 :>'result__160_)
  | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ep _a1 _b1)) && (eq_ep _a2 _b2)
  | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_string _a1 _b1)
  | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ep _a1 _b1)) && (eq_ep _a2 _b2)
  | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ep _a1 _b1)) && (eq_ep _a2 _b2)
  | (`Par (_a0,_a1),`Par (_b0,_b1)) -> (eq_loc _a0 _b0) && (eq_ep _a1 _b1)
  | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_ep _a1 _b1)) && (eq_ctyp _a2 _b2)
  | ((#any as _a0),(#any as _b0)) -> (eq_any _a0 _b0 :>'result__160_)
  | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> eq_loc _a0 _b0
  | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_ep _a1 _b1)
  | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
      (eq_loc _a0 _b0) && (eq_rec_bind _a1 _b1)
  | ((#literal as _a0),(#literal as _b0)) ->
      (eq_literal _a0 _b0 :>'result__160_)
  | (_,_) -> false
and eq_rec_bind curry__158_ curry__159_ =
  match (curry__158_, curry__159_) with
  | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_vid _a1 _b1)) && (eq_ep _a2 _b2)
  | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
      ((eq_loc _a0 _b0) && (eq_rec_bind _a1 _b1)) && (eq_rec_bind _a2 _b2)
  | ((#any as _a0),(#any as _b0)) -> (eq_any _a0 _b0 :>'result__157_)
  | ((#ant as _a0),(#ant as _b0)) -> (eq_ant _a0 _b0 :>'result__157_)
  | (_,_) -> false
