open Ast
open Format
open Lib
open LibUtil
module MetaAst = FanAst.Make(Lib.Meta.MetaGhostLoc)
open FanGrammar
let print_warning = eprintf "%a:\n%s@." FanLoc.print
let prefix = "__fan_"
let meta_action = ref false
let grammar_module_name = let _loc = FanLoc.ghost in ref (`Uid (_loc, ""))
let gm () = grammar_module_name.contents
let mk_entry ~name  ~pos  ~levels  = { name; pos; levels }
let mk_level ~label  ~assoc  ~rules  = { label; assoc; rules }
let mk_rule ~prod  ~action  = { prod; action }
let mk_symbol ?(pattern= None)  ~text  ~styp  = { text; styp; pattern }
let string_of_patt patt =
  let buf = Buffer.create 42 in
  let () =
    Format.bprintf buf "%a@?"
      (fun fmt  p  -> AstPrint.pattern fmt (Ast2pt.patt p)) patt in
  let str = Buffer.contents buf in if str = "" then assert false else str
let check_not_tok s =
  match s with
  | { text = `TXtok (_loc,_,_,_);_} ->
      FanLoc.raise _loc
        (XStream.Error
           ("Deprecated syntax, use a sub rule. " ^
              "L0 STRING becomes L0 [ x = STRING -> x ]"))
  | _ -> ()
let new_type_var =
  let i = ref 0 in fun ()  -> incr i; "e__" ^ (string_of_int i.contents)
let gensym = let i = ref 0 in fun ()  -> incr i; i
let gen_lid () = prefix ^ (string_of_int (gensym ()).contents)
let retype_rule_list_without_patterns _loc rl =
  try
    List.map
      (function
       | { prod = ({ pattern = None ; styp = `Tok _;_} as s)::[];
           action = None  } ->
           {
             prod =
               [{ s with pattern = (Some (`Id (_loc, (`Lid (_loc, "x"))))) }];
             action =
               (Some
                  (`ExApp
                     (_loc,
                       (`Id
                          (_loc,
                            (`IdAcc
                               (_loc, (gm ()),
                                 (`Lid (_loc, "string_of_token")))))),
                       (`Id (_loc, (`Lid (_loc, "x")))))))
           }
       | { prod = ({ pattern = None ;_} as s)::[]; action = None  } ->
           {
             prod =
               [{ s with pattern = (Some (`Id (_loc, (`Lid (_loc, "x"))))) }];
             action = (Some (`Id (_loc, (`Lid (_loc, "x")))))
           }
       | { prod = []; action = Some _ } as r -> r
       | _ -> raise Exit) rl
  with | Exit  -> rl
exception NotneededTyping
let make_ctyp (styp : styp) tvar =
  (let rec aux =
     function
     | `Id _|`Quote _ as x -> x
     | `TyApp (_loc,t1,t2) -> `TyApp (_loc, (aux t1), (aux t2))
     | `Self (_loc,x) ->
         if tvar = ""
         then
           FanLoc.raise _loc
             (XStream.Error
                ("'" ^ (x ^ "' illegal in anonymous entry level")))
         else `Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, tvar))))
     | `Tok _loc ->
         `TyVrnSup
           (_loc,
             (`Id
                (_loc,
                  (`IdAcc
                     (_loc, (`Uid (_loc, "FanToken")), (`Lid (_loc, "t")))))))
     | `Type t -> t in
   try Some (aux styp) with | NotneededTyping  -> None : ctyp option )
let make_ctyp_patt styp tvar patt =
  match make_ctyp styp tvar with
  | None  -> patt
  | Some t -> let _loc = FanAst.loc_of_patt patt in `PaTyc (_loc, patt, t)
let make_ctyp_expr styp tvar expr =
  match make_ctyp styp tvar with
  | None  -> expr
  | Some t ->
      let _loc = FanAst.loc_of_expr expr in `Constraint_exp (_loc, expr, t)
let rec make_expr entry tvar =
  function
  | `TXmeta (_loc,n,tl,e,t) ->
      let el =
        Expr.mklist _loc (List.map (fun t  -> make_expr entry "" t) tl) in
      let ns = Expr.mklist _loc (List.map (fun n  -> `Str (_loc, n)) n) in
      `ExApp
        (_loc, (`ExVrn (_loc, "Smeta")),
          (`ExTup
             (_loc,
               (`ExCom
                  (_loc, ns,
                    (`ExCom
                       (_loc, el,
                         (`ExApp
                            (_loc,
                              (`Id
                                 (_loc,
                                   (`IdAcc
                                      (_loc, (gm ()),
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, "Action")),
                                             (`Lid (_loc, "mk")))))))),
                              (make_ctyp_expr t tvar e))))))))))
  | `TXlist (_loc,min,t,ts) ->
      let txt = make_expr entry "" t.text in
      (match (min, ts) with
       | (false ,None ) -> `ExApp (_loc, (`ExVrn (_loc, "Slist0")), txt)
       | (true ,None ) -> `ExApp (_loc, (`ExVrn (_loc, "Slist1")), txt)
       | (false ,Some s) ->
           let x = make_expr entry tvar s.text in
           `ExApp
             (_loc, (`ExVrn (_loc, "Slist0sep")),
               (`ExTup (_loc, (`ExCom (_loc, txt, x)))))
       | (true ,Some s) ->
           let x = make_expr entry tvar s.text in
           `ExApp
             (_loc, (`ExVrn (_loc, "Slist1sep")),
               (`ExTup (_loc, (`ExCom (_loc, txt, x))))))
  | `TXnext _loc -> `ExVrn (_loc, "Snext")
  | `TXself _loc -> `ExVrn (_loc, "Sself")
  | `TXkwd (_loc,kwd) ->
      `ExApp (_loc, (`ExVrn (_loc, "Skeyword")), (`Str (_loc, kwd)))
  | `TXnterm (_loc,n,lev) ->
      (match lev with
       | Some lab ->
           `ExApp
             (_loc, (`ExVrn (_loc, "Snterml")),
               (`ExTup
                  (_loc,
                    (`ExCom
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`Id
                                 (_loc,
                                   (`IdAcc
                                      (_loc, (gm ()), (`Lid (_loc, "obj")))))),
                              (`Constraint_exp
                                 (_loc, (n.expr),
                                   (`TyApp
                                      (_loc,
                                        (`Id
                                           (_loc,
                                             (`IdAcc
                                                (_loc, (gm ()),
                                                  (`Lid (_loc, "t")))))),
                                        (`Quote
                                           (_loc, (`Normal _loc),
                                             (`Some (`Lid (_loc, (n.tvar)))))))))))),
                         (`Str (_loc, lab)))))))
       | None  ->
           if n.tvar = tvar
           then `ExVrn (_loc, "Sself")
           else
             `ExApp
               (_loc, (`ExVrn (_loc, "Snterm")),
                 (`ExApp
                    (_loc,
                      (`Id
                         (_loc,
                           (`IdAcc (_loc, (gm ()), (`Lid (_loc, "obj")))))),
                      (`Constraint_exp
                         (_loc, (n.expr),
                           (`TyApp
                              (_loc,
                                (`Id
                                   (_loc,
                                     (`IdAcc
                                        (_loc, (gm ()), (`Lid (_loc, "t")))))),
                                (`Quote
                                   (_loc, (`Normal _loc),
                                     (`Some (`Lid (_loc, (n.tvar))))))))))))))
  | `TXopt (_loc,t) ->
      `ExApp (_loc, (`ExVrn (_loc, "Sopt")), (make_expr entry "" t))
  | `TXtry (_loc,t) ->
      `ExApp (_loc, (`ExVrn (_loc, "Stry")), (make_expr entry "" t))
  | `TXpeek (_loc,t) ->
      `ExApp (_loc, (`ExVrn (_loc, "Speek")), (make_expr entry "" t))
  | `TXrules (_loc,rl) ->
      `ExApp
        (_loc,
          (`ExApp
             (_loc,
               (`Id (_loc, (`IdAcc (_loc, (gm ()), (`Lid (_loc, "srules")))))),
               (entry.expr))), (make_expr_rules _loc entry rl ""))
  | `TXtok (_loc,match_fun,attr,descr) ->
      `ExApp
        (_loc, (`ExVrn (_loc, "Stoken")),
          (`ExTup
             (_loc,
               (`ExCom
                  (_loc, match_fun,
                    (`ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (`ExVrn (_loc, attr)),
                              (`Str
                                 (_loc, (FanAst.safe_string_escaped descr))))))))))))
and make_expr_rules _loc n rl tvar =
  Expr.mklist _loc
    (List.map
       (fun (sl,action)  ->
          let sl =
            Expr.mklist _loc (List.map (fun t  -> make_expr n tvar t) sl) in
          `ExTup (_loc, (`ExCom (_loc, sl, action)))) rl)
let text_of_action _loc psl rtvar act tvar =
  let locid = `Id (_loc, (`Lid (_loc, (FanLoc.name.contents)))) in
  let act =
    match act with
    | Some act -> act
    | None  -> `Id (_loc, (`Uid (_loc, "()"))) in
  let (_,tok_match_pl) =
    List.fold_lefti
      (fun i  tok_match_pl  x  ->
         match x with
         | { pattern = Some p; text = `TXtok _;_} ->
             let id = prefix ^ (string_of_int i) in
             Some
               ((match tok_match_pl with
                 | None  -> ((`Id (_loc, (`Lid (_loc, id)))), p)
                 | Some (oe,op) ->
                     ((`ExCom (_loc, (`Id (_loc, (`Lid (_loc, id)))), oe)),
                       (`Com (_loc, p, op)))))
         | _ -> tok_match_pl) None psl in
  let e =
    let e1 =
      `Constraint_exp
        (_loc, act,
          (`Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, rtvar)))))) in
    let e2 =
      match tok_match_pl with
      | None  -> e1
      | Some (`ExCom (_loc,t1,t2),`Com (_,p1,p2)) ->
          `Match
            (_loc, (`ExTup (_loc, (`ExCom (_loc, t1, t2)))),
              (`Or
                 (_loc,
                   (`Case
                      (_loc, (`Tup (_loc, (`Com (_loc, p1, p2)))),
                        (`Nil _loc), e1)),
                   (`Case (_loc, (`Any _loc), (`Nil _loc), (`ExAsf _loc))))))
      | Some (tok,match_) ->
          `Match
            (_loc, tok,
              (`Or
                 (_loc, (`Case (_loc, match_, (`Nil _loc), e1)),
                   (`Case (_loc, (`Any _loc), (`Nil _loc), (`ExAsf _loc)))))) in
    `Fun
      (_loc,
        (`Case
           (_loc,
             (`PaTyc
                (_loc, locid,
                  (`Id
                     (_loc,
                       (`IdAcc
                          (_loc, (`Uid (_loc, "FanLoc")), (`Lid (_loc, "t")))))))),
             (`Nil _loc), e2))) in
  let (_,txt) =
    List.fold_lefti
      (fun i  txt  s  ->
         match s.pattern with
         | None |Some (`Any _) ->
             `Fun (_loc, (`Case (_loc, (`Any _loc), (`Nil _loc), txt)))
         | Some (`Alias (_loc,`PaApp (_,_,`Tup (_,`Any _)),p)) ->
             let p = make_ctyp_patt s.styp tvar (`Id (_loc, (p :>ident))) in
             `Fun (_loc, (`Case (_loc, p, (`Nil _loc), txt)))
         | Some p when FanAst.is_irrefut_patt p ->
             let p = make_ctyp_patt s.styp tvar p in
             `Fun (_loc, (`Case (_loc, p, (`Nil _loc), txt)))
         | Some _ ->
             let p =
               make_ctyp_patt s.styp tvar
                 (`Id (_loc, (`Lid (_loc, (prefix ^ (string_of_int i)))))) in
             `Fun (_loc, (`Case (_loc, p, (`Nil _loc), txt)))) e psl in
  let txt =
    if meta_action.contents
    then
      `ExApp
        (_loc,
          (`Id
             (_loc,
               (`IdAcc (_loc, (`Uid (_loc, "Obj")), (`Lid (_loc, "magic")))))),
          (MetaAst.Expr.meta_expr _loc txt))
    else txt in
  `ExApp
    (_loc,
      (`Id (_loc, (`IdAcc (_loc, (gm ()), (`Lid (_loc, "mk_action")))))),
      txt)
let mk_srules loc t rl tvar =
  List.map
    (fun r  ->
       let sl = List.map (fun s  -> s.text) r.prod in
       let ac = text_of_action loc r.prod t r.action tvar in (sl, ac)) rl
let expr_of_delete_rule _loc n sl =
  let sl =
    List.fold_right
      (fun s  e  ->
         `ExApp
           (_loc,
             (`ExApp
                (_loc, (`Id (_loc, (`Uid (_loc, "::")))),
                  (make_expr n "" s.text))), e)) sl
      (`Id (_loc, (`Uid (_loc, "[]")))) in
  ((n.expr), sl)
let mk_name _loc i =
  { expr = (`Id (_loc, i)); tvar = (Ident.tvar_of_ident i); loc = _loc }
let mk_slist loc min sep symb = `TXlist (loc, min, symb, sep)
let text_of_entry _loc e =
  let ent =
    let x = e.name in
    let _loc = (e.name).loc in
    `Constraint_exp
      (_loc, (x.expr),
        (`TyApp
           (_loc, (`Id (_loc, (`IdAcc (_loc, (gm ()), (`Lid (_loc, "t")))))),
             (`Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, (x.tvar))))))))) in
  let pos =
    match e.pos with
    | Some pos -> `ExApp (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), pos)
    | None  -> `Id (_loc, (`Uid (_loc, "None"))) in
  let txt =
    List.fold_right
      (fun level  txt  ->
         let lab =
           match level.label with
           | Some lab ->
               `ExApp
                 (_loc, (`Id (_loc, (`Uid (_loc, "Some")))),
                   (`Str (_loc, lab)))
           | None  -> `Id (_loc, (`Uid (_loc, "None"))) in
         let ass =
           match level.assoc with
           | Some ass ->
               `ExApp (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), ass)
           | None  -> `Id (_loc, (`Uid (_loc, "None"))) in
         let txt =
           let rl = mk_srules _loc (e.name).tvar level.rules (e.name).tvar in
           let e = make_expr_rules _loc e.name rl (e.name).tvar in
           `ExApp
             (_loc,
               (`ExApp
                  (_loc, (`Id (_loc, (`Uid (_loc, "::")))),
                    (`ExTup
                       (_loc, (`ExCom (_loc, lab, (`ExCom (_loc, ass, e)))))))),
               txt) in
         txt) e.levels (`Id (_loc, (`Uid (_loc, "[]")))) in
  (ent, pos, txt)
let let_in_of_extend _loc gram gl default =
  let entry_mk =
    match gram with
    | Some g ->
        `ExApp
          (_loc, (`Id (_loc, (`IdAcc (_loc, (gm ()), (`Lid (_loc, "mk")))))),
            (`Id (_loc, g)))
    | None  -> `Id (_loc, (`IdAcc (_loc, (gm ()), (`Lid (_loc, "mk"))))) in
  let local_binding_of_name =
    function
    | { expr = `Id (_,`Lid (_,i)); tvar = x; loc = _loc } ->
        `Bind
          (_loc, (`Id (_loc, (`Lid (_loc, i)))),
            (`Constraint_exp
               (_loc,
                 (`ExApp
                    (_loc,
                      (`Id (_loc, (`Lid (_loc, "grammar_entry_create")))),
                      (`Str (_loc, i)))),
                 (`TyApp
                    (_loc,
                      (`Id
                         (_loc, (`IdAcc (_loc, (gm ()), (`Lid (_loc, "t")))))),
                      (`Quote
                         (_loc, (`Normal _loc), (`Some (`Lid (_loc, x))))))))))
    | _ -> failwith "internal error in the Grammar extension" in
  match gl with
  | None  -> default
  | Some ll ->
      (match ll with
       | [] -> default
       | x::xs ->
           let locals =
             List.fold_right
               (fun name  acc  ->
                  `And (_loc, acc, (local_binding_of_name name))) xs
               (local_binding_of_name x) in
           `LetIn
             (_loc, (`ReNil _loc),
               (`Bind
                  (_loc, (`Id (_loc, (`Lid (_loc, "grammar_entry_create")))),
                    entry_mk)),
               (`LetIn (_loc, (`ReNil _loc), locals, default))))
let text_of_functorial_extend _loc gram locals el =
  let args =
    let el =
      List.map
        (fun e  ->
           let (ent,pos,txt) = text_of_entry (e.name).loc e in
           `ExApp
             (_loc,
               (`ExApp
                  (_loc,
                    (`Id
                       (_loc,
                         (`IdAcc (_loc, (gm ()), (`Lid (_loc, "extend")))))),
                    ent)), (`ExTup (_loc, (`ExCom (_loc, pos, txt)))))) el in
    match el with
    | [] -> `Id (_loc, (`Uid (_loc, "()")))
    | e::[] -> e
    | e::el ->
        `Seq
          (_loc, (List.fold_left (fun acc  x  -> `Sem (_loc, acc, x)) e el)) in
  let_in_of_extend _loc gram locals args
let mk_tok _loc ?restrict  ~pattern  styp =
  match restrict with
  | None  ->
      let no_variable = FanAst.wildcarder#patt pattern in
      let match_fun =
        if FanAst.is_irrefut_patt no_variable
        then
          `Fun
            (_loc,
              (`Case
                 (_loc, no_variable, (`Nil _loc),
                   (`Id (_loc, (`Lid (_loc, "true")))))))
        else
          `Fun
            (_loc,
              (`Or
                 (_loc,
                   (`Case
                      (_loc, no_variable, (`Nil _loc),
                        (`Id (_loc, (`Lid (_loc, "true")))))),
                   (`Case
                      (_loc, (`Any _loc), (`Nil _loc),
                        (`Id (_loc, (`Lid (_loc, "false"))))))))) in
      let descr = string_of_patt no_variable in
      let text = `TXtok (_loc, match_fun, "Normal", descr) in
      { text; styp; pattern = (Some pattern) }
  | Some restrict ->
      let p' = FanAst.wildcarder#patt pattern in
      let match_fun =
        `Fun
          (_loc,
            (`Or
               (_loc,
                 (`Case
                    (_loc, pattern, restrict,
                      (`Id (_loc, (`Lid (_loc, "true")))))),
                 (`Case
                    (_loc, (`Any _loc), (`Nil _loc),
                      (`Id (_loc, (`Lid (_loc, "false"))))))))) in
      let descr = string_of_patt pattern in
      let text = `TXtok (_loc, match_fun, "Antiquot", descr) in
      { text; styp; pattern = (Some p') }
let sfold ?sep  _loc (ns : string list) f e s =
  let fs = [("FOLD0", "sfold0"); ("FOLD1", "sfold1")] in
  let suffix = match sep with | None  -> "" | Some _ -> "sep" in
  let n = List.hd ns in
  let foldfun =
    try (List.assoc n fs) ^ suffix with | Not_found  -> invalid_arg "sfold" in
  let styp =
    `Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, (new_type_var ()))))) in
  let e =
    `ExApp
      (_loc,
        (`ExApp
           (_loc,
             (`Id (_loc, (`IdAcc (_loc, (gm ()), (`Lid (_loc, foldfun)))))),
             f)), e) in
  let (t :styp)=
    `TyApp
      (_loc,
        (`TyApp
           (_loc,
             (`Type
                (`TyApp
                   (_loc,
                     (`Id
                        (_loc,
                          (`IdAcc
                             (_loc, (gm ()),
                               (`Lid (_loc, ("fold" ^ suffix))))))),
                     (`Any _loc)))), (s.styp))), styp) in
  let text =
    `TXmeta
      (_loc, ns,
        (match sep with | None  -> [s.text] | Some sep -> [s.text; sep.text]),
        e, t) in
  { text; styp; pattern = None }