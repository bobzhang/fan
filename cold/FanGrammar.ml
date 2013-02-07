open FanAst
let _ = ()
type name =  {
  expr: expr;
  tvar: string;
  loc: loc} 
type styp =
  [ `Id of (loc* ident) | `App of (loc* styp* styp)
  | `Quote of (loc* position_flag* alident meta_option)
  | `Self of (loc* string) | `Tok of loc | `Type of ctyp] 
type attr = string 
type entry =  {
  name: name;
  pos: expr option;
  levels: levels} 
and levels = [ `Group of level list | `Single of level] 
and level =  {
  label: string option;
  assoc: expr option;
  rules: rule list} 
and rule =  {
  prod: symbol list;
  action: expr option} 
and symbol =  {
  text: text;
  styp: styp;
  pattern: patt option} 
and text =
  [ `Smeta of (loc* string list* text list* expr* styp)
  | `Slist of (loc* bool* symbol* symbol option)
  | `Snterm of (loc* name* string option) | `Sopt of (loc* text)
  | `Stry of (loc* text) | `Speek of (loc* text)
  | `Srules of (loc* (text list* expr) list) | `Sself of loc | `Snext of loc
  | `Skeyword of (loc* string) | `Stok of (loc* expr* attr* string)] 
module Expr =
  struct
    open FanAst.MExpr
    open Filters.ME
    let meta_name _loc { expr = _a0; tvar = _a1; loc = _a2 } =
      `Record
        (_loc,
          (`Sem
             (_loc,
               (`RecBind (_loc, (`Lid (_loc, "expr")), (meta_expr _loc _a0))),
               (`Sem
                  (_loc,
                    (`RecBind
                       (_loc, (`Lid (_loc, "tvar")), (meta_string _loc _a1))),
                    (`RecBind
                       (_loc, (`Lid (_loc, "loc")), (meta_loc _loc _a2))))))))
    let rec meta_styp _loc =
      function
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
                   (meta_styp _loc _a1))), (meta_styp _loc _a2))
      | `Quote (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Quote")), (meta_loc _loc _a0))),
                   (meta_position_flag _loc _a1))),
              (meta_meta_option meta_alident _loc _a2))
      | `Self (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Self")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `Tok _a0 -> `App (_loc, (`Vrn (_loc, "Tok")), (meta_loc _loc _a0))
      | `Type _a0 -> `App (_loc, (`Vrn (_loc, "Type")), (meta_ctyp _loc _a0))
    let meta_attr _loc _a0 = meta_string _loc _a0
    let rec meta_entry _loc { name = _a0; pos = _a1; levels = _a2 } =
      `Record
        (_loc,
          (`Sem
             (_loc,
               (`RecBind (_loc, (`Lid (_loc, "name")), (meta_name _loc _a0))),
               (`Sem
                  (_loc,
                    (`RecBind
                       (_loc, (`Lid (_loc, "pos")),
                         (meta_option meta_expr _loc _a1))),
                    (`RecBind
                       (_loc, (`Lid (_loc, "levels")),
                         (meta_levels _loc _a2))))))))
    and meta_levels _loc =
      function
      | `Group _a0 ->
          `App
            (_loc, (`Vrn (_loc, "Group")), (meta_list meta_level _loc _a0))
      | `Single _a0 ->
          `App (_loc, (`Vrn (_loc, "Single")), (meta_level _loc _a0))
    and meta_level _loc { label = _a0; assoc = _a1; rules = _a2 } =
      `Record
        (_loc,
          (`Sem
             (_loc,
               (`RecBind
                  (_loc, (`Lid (_loc, "label")),
                    (meta_option meta_string _loc _a0))),
               (`Sem
                  (_loc,
                    (`RecBind
                       (_loc, (`Lid (_loc, "assoc")),
                         (meta_option meta_expr _loc _a1))),
                    (`RecBind
                       (_loc, (`Lid (_loc, "rules")),
                         (meta_list meta_rule _loc _a2))))))))
    and meta_rule _loc { prod = _a0; action = _a1 } =
      `Record
        (_loc,
          (`Sem
             (_loc,
               (`RecBind
                  (_loc, (`Lid (_loc, "prod")),
                    (meta_list meta_symbol _loc _a0))),
               (`RecBind
                  (_loc, (`Lid (_loc, "action")),
                    (meta_option meta_expr _loc _a1))))))
    and meta_symbol _loc { text = _a0; styp = _a1; pattern = _a2 } =
      `Record
        (_loc,
          (`Sem
             (_loc,
               (`RecBind (_loc, (`Lid (_loc, "text")), (meta_text _loc _a0))),
               (`Sem
                  (_loc,
                    (`RecBind
                       (_loc, (`Lid (_loc, "styp")), (meta_styp _loc _a1))),
                    (`RecBind
                       (_loc, (`Lid (_loc, "pattern")),
                         (meta_option meta_patt _loc _a2))))))))
    and meta_text _loc =
      function
      | `Smeta (_a0,_a1,_a2,_a3,_a4) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc,
                             (`App
                                (_loc, (`Vrn (_loc, "Smeta")),
                                  (meta_loc _loc _a0))),
                             (meta_list meta_string _loc _a1))),
                        (meta_list meta_text _loc _a2))),
                   (meta_expr _loc _a3))), (meta_styp _loc _a4))
      | `Slist (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "Slist")),
                             (meta_loc _loc _a0))), (meta_bool _loc _a1))),
                   (meta_symbol _loc _a2))),
              (meta_option meta_symbol _loc _a3))
      | `Snterm (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Snterm")), (meta_loc _loc _a0))),
                   (meta_name _loc _a1))),
              (meta_option meta_string _loc _a2))
      | `Sopt (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Sopt")), (meta_loc _loc _a0))),
              (meta_text _loc _a1))
      | `Stry (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Stry")), (meta_loc _loc _a0))),
              (meta_text _loc _a1))
      | `Speek (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Speek")), (meta_loc _loc _a0))),
              (meta_text _loc _a1))
      | `Srules (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Srules")), (meta_loc _loc _a0))),
              (meta_list
                 (fun _loc  (_a0,_a1)  ->
                    `Tup
                      (_loc,
                        (`Com
                           (_loc, (meta_list meta_text _loc _a0),
                             (meta_expr _loc _a1))))) _loc _a1))
      | `Sself _a0 ->
          `App (_loc, (`Vrn (_loc, "Sself")), (meta_loc _loc _a0))
      | `Snext _a0 ->
          `App (_loc, (`Vrn (_loc, "Snext")), (meta_loc _loc _a0))
      | `Skeyword (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Skeyword")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `Stok (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "Stok")), (meta_loc _loc _a0))),
                        (meta_expr _loc _a1))), (meta_attr _loc _a2))),
              (meta_string _loc _a3))
  end
module Patt =
  struct
    open FanAst.MPatt
    open Filters.MP
    let meta_name _loc { expr = _a0; tvar = _a1; loc = _a2 } =
      `PaRec
        (_loc,
          (`Sem
             (_loc,
               (`PaEq (_loc, (`Lid (_loc, "expr")), (meta_expr _loc _a0))),
               (`Sem
                  (_loc,
                    (`PaEq
                       (_loc, (`Lid (_loc, "tvar")), (meta_string _loc _a1))),
                    (`PaEq (_loc, (`Lid (_loc, "loc")), (meta_loc _loc _a2))))))))
    let rec meta_styp _loc =
      function
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
                   (meta_styp _loc _a1))), (meta_styp _loc _a2))
      | `Quote (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Quote")), (meta_loc _loc _a0))),
                   (meta_position_flag _loc _a1))),
              (meta_meta_option meta_alident _loc _a2))
      | `Self (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Self")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `Tok _a0 -> `App (_loc, (`Vrn (_loc, "Tok")), (meta_loc _loc _a0))
      | `Type _a0 -> `App (_loc, (`Vrn (_loc, "Type")), (meta_ctyp _loc _a0))
    let meta_attr _loc _a0 = meta_string _loc _a0
    let rec meta_entry _loc { name = _a0; pos = _a1; levels = _a2 } =
      `PaRec
        (_loc,
          (`Sem
             (_loc,
               (`PaEq (_loc, (`Lid (_loc, "name")), (meta_name _loc _a0))),
               (`Sem
                  (_loc,
                    (`PaEq
                       (_loc, (`Lid (_loc, "pos")),
                         (meta_option meta_expr _loc _a1))),
                    (`PaEq
                       (_loc, (`Lid (_loc, "levels")),
                         (meta_levels _loc _a2))))))))
    and meta_levels _loc =
      function
      | `Group _a0 ->
          `App
            (_loc, (`Vrn (_loc, "Group")), (meta_list meta_level _loc _a0))
      | `Single _a0 ->
          `App (_loc, (`Vrn (_loc, "Single")), (meta_level _loc _a0))
    and meta_level _loc { label = _a0; assoc = _a1; rules = _a2 } =
      `PaRec
        (_loc,
          (`Sem
             (_loc,
               (`PaEq
                  (_loc, (`Lid (_loc, "label")),
                    (meta_option meta_string _loc _a0))),
               (`Sem
                  (_loc,
                    (`PaEq
                       (_loc, (`Lid (_loc, "assoc")),
                         (meta_option meta_expr _loc _a1))),
                    (`PaEq
                       (_loc, (`Lid (_loc, "rules")),
                         (meta_list meta_rule _loc _a2))))))))
    and meta_rule _loc { prod = _a0; action = _a1 } =
      `PaRec
        (_loc,
          (`Sem
             (_loc,
               (`PaEq
                  (_loc, (`Lid (_loc, "prod")),
                    (meta_list meta_symbol _loc _a0))),
               (`PaEq
                  (_loc, (`Lid (_loc, "action")),
                    (meta_option meta_expr _loc _a1))))))
    and meta_symbol _loc { text = _a0; styp = _a1; pattern = _a2 } =
      `PaRec
        (_loc,
          (`Sem
             (_loc,
               (`PaEq (_loc, (`Lid (_loc, "text")), (meta_text _loc _a0))),
               (`Sem
                  (_loc,
                    (`PaEq
                       (_loc, (`Lid (_loc, "styp")), (meta_styp _loc _a1))),
                    (`PaEq
                       (_loc, (`Lid (_loc, "pattern")),
                         (meta_option meta_patt _loc _a2))))))))
    and meta_text _loc =
      function
      | `Smeta (_a0,_a1,_a2,_a3,_a4) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc,
                             (`App
                                (_loc, (`Vrn (_loc, "Smeta")),
                                  (meta_loc _loc _a0))),
                             (meta_list meta_string _loc _a1))),
                        (meta_list meta_text _loc _a2))),
                   (meta_expr _loc _a3))), (meta_styp _loc _a4))
      | `Slist (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "Slist")),
                             (meta_loc _loc _a0))), (meta_bool _loc _a1))),
                   (meta_symbol _loc _a2))),
              (meta_option meta_symbol _loc _a3))
      | `Snterm (_a0,_a1,_a2) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "Snterm")), (meta_loc _loc _a0))),
                   (meta_name _loc _a1))),
              (meta_option meta_string _loc _a2))
      | `Sopt (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Sopt")), (meta_loc _loc _a0))),
              (meta_text _loc _a1))
      | `Stry (_a0,_a1) ->
          `App
            (_loc, (`App (_loc, (`Vrn (_loc, "Stry")), (meta_loc _loc _a0))),
              (meta_text _loc _a1))
      | `Speek (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Speek")), (meta_loc _loc _a0))),
              (meta_text _loc _a1))
      | `Srules (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Srules")), (meta_loc _loc _a0))),
              (meta_list
                 (fun _loc  (_a0,_a1)  ->
                    `Tup
                      (_loc,
                        (`Com
                           (_loc, (meta_list meta_text _loc _a0),
                             (meta_expr _loc _a1))))) _loc _a1))
      | `Sself _a0 ->
          `App (_loc, (`Vrn (_loc, "Sself")), (meta_loc _loc _a0))
      | `Snext _a0 ->
          `App (_loc, (`Vrn (_loc, "Snext")), (meta_loc _loc _a0))
      | `Skeyword (_a0,_a1) ->
          `App
            (_loc,
              (`App (_loc, (`Vrn (_loc, "Skeyword")), (meta_loc _loc _a0))),
              (meta_string _loc _a1))
      | `Stok (_a0,_a1,_a2,_a3) ->
          `App
            (_loc,
              (`App
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc, (`Vrn (_loc, "Stok")), (meta_loc _loc _a0))),
                        (meta_expr _loc _a1))), (meta_attr _loc _a2))),
              (meta_string _loc _a3))
  end
type used =  
  | Unused
  | UsedScanned
  | UsedNotScanned 
type simple_patt =
  [ `Vrn of (loc* string) | `App of (loc* simple_patt* simple_patt)
  | `Id of (loc* ident) | `Com of (loc* simple_patt* simple_patt)
  | `Alias of (loc* simple_patt* alident)
  | `Or of (loc* simple_patt* simple_patt) | `Str of (loc* string)
  | `Any of loc | `Nil of loc | ant] 
type action_pattern =
  [ `Id of (loc* ident) | `Com of (loc* action_pattern* action_pattern)
  | `Tup of (loc* action_pattern) | `Any of loc | `Nil of loc] 
