open PreCast.Syntax
open Lib
open LibUtil
let debug_mode =
  try
    let str = Sys.getenv "STATIC_CAMLP4_DEBUG" in
    let rec loop acc i =
      try
        let pos = String.index_from str i ':' in
        loop (SSet.add (String.sub str i (pos - i)) acc) (pos + 1)
      with
      | Not_found  ->
          SSet.add (String.sub str i ((String.length str) - i)) acc in
    let sections = loop SSet.empty 0 in
    if SSet.mem "*" sections
    then fun _  -> true
    else (fun x  -> SSet.mem x sections)
  with | Not_found  -> (fun _  -> false)
let mk_debug_mode _loc =
  function
  | None  ->
      `ExId
        (_loc,
          (`IdAcc (_loc, (`Uid (_loc, "Debug")), (`Lid (_loc, "mode")))))
  | Some m ->
      `ExId
        (_loc,
          (`IdAcc
             (_loc, (`Uid (_loc, m)),
               (`IdAcc (_loc, (`Uid (_loc, "Debug")), (`Lid (_loc, "mode")))))))
let mk_debug _loc m fmt section args =
  let call =
    Expr.apply
      (`ExApp
         (_loc,
           (`ExApp
              (_loc,
                (`ExId
                   (_loc,
                     (`IdAcc
                        (_loc, (`Uid (_loc, "Debug")),
                          (`Lid (_loc, "printf")))))),
                (`ExStr (_loc, section)))), (`ExStr (_loc, fmt)))) args in
  `ExIfe
    (_loc, (`ExApp (_loc, (mk_debug_mode _loc m), (`ExStr (_loc, section)))),
      call, (`ExId (_loc, (`Uid (_loc, "()")))))
let apply () =
  let grammar_entry_create = Gram.mk in
  let start_debug: 'start_debug Gram.t = grammar_entry_create "start_debug"
  and end_or_in: 'end_or_in Gram.t = grammar_entry_create "end_or_in" in
  Gram.extend (expr : 'expr Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (start_debug : 'start_debug Gram.t ));
           `Stoken
             (((function | `LID _ -> true | _ -> false)),
               (`Normal, "`LID _"));
           `Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"));
           `Slist0 (`Snterml ((Gram.obj (expr : 'expr Gram.t )), "."));
           `Snterm (Gram.obj (end_or_in : 'end_or_in Gram.t ))],
            (Gram.mk_action
               (fun (x : 'end_or_in)  (args : 'expr list) 
                  (__fan_2 : [> FanToken.t])  (__fan_1 : [> FanToken.t]) 
                  (m : 'start_debug)  (_loc : FanLoc.t)  ->
                  match (__fan_2, __fan_1) with
                  | (`STR (_,fmt),`LID section) ->
                      ((match (x, (debug_mode section)) with
                        | (None ,false ) -> `ExId (_loc, (`Uid (_loc, "()")))
                        | (Some e,false ) -> e
                        | (None ,_) -> mk_debug _loc m fmt section args
                        | (Some e,_) ->
                            `ExLet
                              (_loc, `ReNil,
                                (`BiEq
                                   (_loc,
                                     (`PaId (_loc, (`Uid (_loc, "()")))),
                                     (mk_debug _loc m fmt section args))), e)) : 
                      'expr )
                  | _ -> assert false)))])]);
  Gram.extend (end_or_in : 'end_or_in Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "end"],
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (None : 'end_or_in ))));
         ([`Skeyword "in"; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
           (Gram.mk_action
              (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                 (Some e : 'end_or_in ))))])]);
  Gram.extend (start_debug : 'start_debug Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `LID "debug" -> true | _ -> false)),
                (`Normal, "`LID \"debug\""))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `LID "debug" -> (None : 'start_debug )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `LID "camlp4_debug" -> true | _ -> false)),
               (`Normal, "`LID \"camlp4_debug\""))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `LID "camlp4_debug" -> (Some "Camlp4" : 'start_debug )
                 | _ -> assert false)))])])
let _ = AstParsers.register_parser ("debug", apply)