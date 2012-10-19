open Format

open Lib

module MetaAst = (Camlp4Ast.Meta.Make)(Lib.Meta.MetaGhostLoc)

module Ast = Camlp4Ast

open FanGrammar

let print_warning = (eprintf "%a:\n%s@." FanLoc.print)

let split_ext = (ref false )

let prefix = "__camlp4_"

let meta_action = (ref false )

let grammar_module_name =
 let _loc = FanLoc.ghost in (ref ( (Ast.IdUid (_loc, "")) ))

let gm = fun ()  -> grammar_module_name.contents

let mk_entry =
 fun ~name ->
  fun ~pos -> fun ~levels -> {name = name; pos = pos; levels = levels}

let mk_level =
 fun ~label ->
  fun ~assoc -> fun ~rules -> {label = label; assoc = assoc; rules = rules}

let mk_rule = fun ~prod -> fun ~action -> {prod = prod; action = action}

let mk_symbol =
 fun ~used ->
  fun ~text ->
   fun ~styp ->
    fun ~pattern ->
     {used = used; text = text; styp = styp; pattern = pattern}

let string_of_patt =
 fun patt ->
  let buf = (Buffer.create 42) in
  let () =
   (Format.bprintf buf "%a@?" (
     fun fmt -> fun p -> (Pprintast.pattern fmt ( (Ast2pt.patt p) )) ) patt) in
  let str = (Buffer.contents buf) in
  if (str = "") then ( assert false ) else str

let check_not_tok =
 fun s ->
  (match s with
   | {text = TXtok (_loc, _, _, _); _ } ->
      (FanLoc.raise _loc (
        (Stream.Error
          ("Deprecated syntax, use a sub rule. " ^
            "LIST0 STRING becomes LIST0 [ x = STRING -> x ]")) ))
   | _ -> ())

let mark_used =
 fun modif ->
  fun tbl ->
   fun n ->
    (try
      let rll = (Hashtbl.find_all tbl n) in
      (List.iter (
        function
        | (({contents = Unused} as r), _) ->
           ( (r := UsedNotScanned ) ); (modif := true )
        | _ -> () ) rll)
     with
     Not_found -> ())

let mark_symbol =
 fun modif ->
  fun tbl ->
   fun symb -> (List.iter ( fun e -> (mark_used modif tbl e) ) ( symb.used ))

let check_use =
 fun nl ->
  fun el ->
   let tbl = (Hashtbl.create 301) in
   let modif = (ref false ) in
   let () =
    (List.iter (
      fun e ->
       let u =
        (match (e.name).expr with
         | Ast.ExId (_, Ast.IdLid (_, _)) -> (Unused)
         | _ -> (UsedNotScanned)) in
       (Hashtbl.add tbl ( (e.name).tvar ) (( (ref u) ), e)) ) el) in
   let () =
    (List.iter (
      fun n ->
       (try
         let rll = (Hashtbl.find_all tbl ( n.tvar )) in
         (List.iter ( fun (r, _) -> (r := UsedNotScanned ) ) rll)
        with
        _ -> ()) ) nl) in
   let () = (modif := true ) in
   let () =
    while modif.contents do
     (
    (modif := false )
    );
     (Hashtbl.iter (
       fun _ ->
        fun (r, e) ->
         if (( r.contents ) = UsedNotScanned )
         then
          begin
          (
          (r := UsedScanned )
          );
          (List.iter (
            fun level ->
             let rules = level.rules in
             (List.iter (
               fun rule ->
                (List.iter ( fun s -> (mark_symbol modif tbl s) ) ( rule.prod
                  )) ) rules) ) ( e.levels ))
         end else () ) tbl)
    done in
   (Hashtbl.iter (
     fun s ->
      fun (r, e) ->
       if (( r.contents ) = Unused ) then
        (
        (print_warning ( (e.name).loc ) (
          ("Unused local entry \"" ^ ( (s ^ "\"") )) ))
        )
       else () ) tbl)

let new_type_var =
 let i = (ref 0) in
 fun ()  -> ( (incr i) ); ("e__" ^ ( (string_of_int ( i.contents )) ))

let used_of_rule_list =
 fun rl ->
  (List.fold_left (
    fun nl ->
     fun r ->
      (List.fold_left ( fun nl -> fun s -> (( s.used ) @ nl) ) nl ( r.prod ))
    ) []  rl)

let retype_rule_list_without_patterns =
 fun _loc ->
  fun rl ->
   (try
     (List.map (
       function
       | {prod = (({pattern = None; styp = STtok (_); _ } as s) :: []);
          action = None} ->
          {prod = (
            [{s with
              pattern = (
               (Some ((Ast.PaId (_loc, ( (Ast.IdLid (_loc, "x")) ))))) )}] );
           action = (
            (Some
              ((Ast.ExApp
                 (_loc, (
                  (Ast.ExId
                    (_loc, (
                     (Ast.IdAcc
                       (_loc, ( (gm () ) ), (
                        (Ast.IdLid (_loc, "string_of_token")) ))) ))) ), (
                  (Ast.ExId (_loc, ( (Ast.IdLid (_loc, "x")) ))) ))))) )}
       | {prod = (({pattern = None; _ } as s) :: []);
          action = None} ->
          {prod = (
            [{s with
              pattern = (
               (Some ((Ast.PaId (_loc, ( (Ast.IdLid (_loc, "x")) ))))) )}] );
           action = ( (Some ((Ast.ExId (_loc, ( (Ast.IdLid (_loc, "x")) )))))
            )}
       | ({prod = []; action = Some (_)} as r) -> r
       | _ -> (raise Exit ) ) rl)
    with
    Exit -> rl)

exception NotneededTyping

let make_ctyp =
 fun styp ->
  fun tvar ->
   let rec aux =
    function
    | STlid (_loc, s) -> (Ast.TyId (_loc, ( (Ast.IdLid (_loc, s)) )))
    | STapp (_loc, t1, t2) -> (Ast.TyApp (_loc, ( (aux t1) ), ( (aux t2) )))
    | STquo (_loc, s) -> (Ast.TyQuo (_loc, s))
    | STself (_loc, x) ->
       if (tvar = "") then
        (
        (FanLoc.raise _loc (
          (Stream.Error
            ("'" ^ ( (x ^ "' illegal in anonymous entry level") ))) ))
        )
       else (Ast.TyQuo (_loc, tvar))
    | STtok (_loc) -> (raise NotneededTyping )
    | STtyp (t) -> t in
   (try (Some (aux styp)) with
    NotneededTyping -> (None))

let make_ctyp_patt =
 fun styp ->
  fun tvar ->
   fun patt ->
    (match (make_ctyp styp tvar) with
     | None -> patt
     | Some (t) ->
        let _loc = (Camlp4Ast.loc_of_patt patt) in
        (Ast.PaTyc (_loc, patt, t)))

let make_ctyp_expr =
 fun styp ->
  fun tvar ->
   fun expr ->
    (match (make_ctyp styp tvar) with
     | None -> expr
     | Some (t) ->
        let _loc = (Camlp4Ast.loc_of_expr expr) in
        (Ast.ExTyc (_loc, expr, t)))

let text_of_action =
 fun _loc ->
  fun psl ->
   fun (rtvar :
     string) ->
    fun (act :
      Ast.expr option) ->
     fun (tvar :
       string) ->
      let locid =
       (Ast.PaId (_loc, ( (Ast.IdLid (_loc, ( FanLoc.name.contents ))) ))) in
      let act =
       (match act with
        | Some (act) -> act
        | None -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "()")) )))) in
      let (tok_match_pl, act, _) =
       (List.fold_left (
         fun ((tok_match_pl, act, i) as accu) ->
          function
          | {pattern = None; _ } -> accu
          | {pattern = Some (p); _ } when (Camlp4Ast.is_irrefut_patt p) ->
             accu
          | {pattern =
              Some
               (Ast.PaAli
                 (_, Ast.PaApp (_, _, Ast.PaTup (_, Ast.PaAny (_))),
                  Ast.PaId (_, Ast.IdLid (_, s)))); _ } ->
             (tok_match_pl, (
              (Ast.ExLet
                (_loc, Ast.ReNil , (
                 (Ast.BiEq
                   (_loc, ( (Ast.PaId (_loc, ( (Ast.IdLid (_loc, s)) ))) ), (
                    (Ast.ExApp
                      (_loc, (
                       (Ast.ExId
                         (_loc, (
                          (Ast.IdAcc
                            (_loc, ( (gm () ) ), (
                             (Ast.IdLid (_loc, "string_of_token")) ))) ))) ),
                       ( (Ast.ExId (_loc, ( (Ast.IdLid (_loc, s)) ))) ))) )))
                 ), act)) ), i)
          | {pattern = Some (p);
             text = TXtok (_, _, _, _); _ } ->
             let id = (prefix ^ ( (string_of_int i) )) in
             ((
              (Some
                (match tok_match_pl with
                 | None ->
                    (( (Ast.ExId (_loc, ( (Ast.IdLid (_loc, id)) ))) ), p)
                 | Some (tok_pl, match_pl) ->
                    ((
                     (Ast.ExCom
                       (_loc, ( (Ast.ExId (_loc, ( (Ast.IdLid (_loc, id)) )))
                        ), tok_pl)) ), ( (Ast.PaCom (_loc, p, match_pl)) ))))
              ), act, ( (succ i) ))
          | _ -> accu ) (None , act, 0) psl) in
      let e =
       let e1 = (Ast.ExTyc (_loc, act, ( (Ast.TyQuo (_loc, rtvar)) ))) in
       let e2 =
        (match tok_match_pl with
         | None -> e1
         | Some (Ast.ExCom (_, t1, t2), Ast.PaCom (_, p1, p2)) ->
            (Ast.ExMat
              (_loc, ( (Ast.ExTup (_loc, ( (Ast.ExCom (_loc, t1, t2)) ))) ),
               (
               (Ast.McOr
                 (_loc, (
                  (Ast.McArr
                    (_loc, (
                     (Ast.PaTup (_loc, ( (Ast.PaCom (_loc, p1, p2)) ))) ), (
                     (Ast.ExNil (_loc)) ), e1)) ), (
                  (Ast.McArr
                    (_loc, ( (Ast.PaAny (_loc)) ), ( (Ast.ExNil (_loc)) ), (
                     (Ast.ExAsf (_loc)) ))) ))) )))
         | Some (tok, match_) ->
            (Ast.ExMat
              (_loc, tok, (
               (Ast.McOr
                 (_loc, (
                  (Ast.McArr (_loc, match_, ( (Ast.ExNil (_loc)) ), e1)) ), (
                  (Ast.McArr
                    (_loc, ( (Ast.PaAny (_loc)) ), ( (Ast.ExNil (_loc)) ), (
                     (Ast.ExAsf (_loc)) ))) ))) )))) in
       (Ast.ExFun
         (_loc, (
          (Ast.McArr
            (_loc, (
             (Ast.PaTyc
               (_loc, locid, (
                (Ast.TyId
                  (_loc, (
                   (Ast.IdAcc
                     (_loc, ( (Ast.IdUid (_loc, "FanLoc")) ), (
                      (Ast.IdLid (_loc, "t")) ))) ))) ))) ), (
             (Ast.ExNil (_loc)) ), e2)) ))) in
      let (txt, _) =
       (List.fold_left (
         fun (txt, i) ->
          fun s ->
           (match s.pattern with
            | (None | Some (Ast.PaAny (_))) ->
               ((
                (Ast.ExFun
                  (_loc, (
                   (Ast.McArr
                     (_loc, ( (Ast.PaAny (_loc)) ), ( (Ast.ExNil (_loc)) ),
                      txt)) ))) ), i)
            | Some
               (Ast.PaAli
                 (_, Ast.PaApp (_, _, Ast.PaTup (_, Ast.PaAny (_))), p)) ->
               let p = (make_ctyp_patt ( s.styp ) tvar p) in
               ((
                (Ast.ExFun
                  (_loc, ( (Ast.McArr (_loc, p, ( (Ast.ExNil (_loc)) ), txt))
                   ))) ), i)
            | Some (p) when (Camlp4Ast.is_irrefut_patt p) ->
               let p = (make_ctyp_patt ( s.styp ) tvar p) in
               ((
                (Ast.ExFun
                  (_loc, ( (Ast.McArr (_loc, p, ( (Ast.ExNil (_loc)) ), txt))
                   ))) ), i)
            | Some (_) ->
               let p =
                (make_ctyp_patt ( s.styp ) tvar (
                  (Ast.PaId
                    (_loc, (
                     (Ast.IdLid
                       (_loc, ( ("__camlp4_" ^ ( (string_of_int i) )) ))) )))
                  )) in
               ((
                (Ast.ExFun
                  (_loc, ( (Ast.McArr (_loc, p, ( (Ast.ExNil (_loc)) ), txt))
                   ))) ), ( (succ i) ))) ) (e, 0) psl) in
      let txt =
       if meta_action.contents then
        (
        (Ast.ExApp
          (_loc, (
           (Ast.ExId
             (_loc, (
              (Ast.IdAcc
                (_loc, ( (Ast.IdUid (_loc, "Obj")) ), (
                 (Ast.IdLid (_loc, "magic")) ))) ))) ), (
           (MetaAst.Expr.meta_expr _loc txt) )))
        )
       else txt in
      (Ast.ExApp
        (_loc, (
         (Ast.ExId
           (_loc, (
            (Ast.IdAcc
              (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, "mk_action")) ))) )))
         ), txt))

let srules =
 fun loc ->
  fun t ->
   fun rl ->
    fun tvar ->
     (List.map (
       fun r ->
        let sl = (List.map ( fun s -> s.text ) ( r.prod )) in
        let ac = (text_of_action loc ( r.prod ) t ( r.action ) tvar) in
        (sl, ac) ) rl)

let rec make_expr =
 fun entry ->
  fun tvar ->
   function
   | TXmeta (_loc, n, tl, e, t) ->
      let el =
       (List.fold_right (
         fun t ->
          fun el ->
           (Ast.ExApp
             (_loc, (
              (Ast.ExApp
                (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) ))) ), (
                 (make_expr entry "" t) ))) ), el)) ) tl (
         (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "[]")) ))) )) in
      (Ast.ExApp
        (_loc, (
         (Ast.ExApp
           (_loc, (
            (Ast.ExApp
              (_loc, (
               (Ast.ExId
                 (_loc, (
                  (Ast.IdAcc
                    (_loc, ( (gm () ) ), ( (Ast.IdUid (_loc, "Smeta")) ))) )))
               ), ( (Ast.ExStr (_loc, n)) ))) ), el)) ), (
         (Ast.ExApp
           (_loc, (
            (Ast.ExId
              (_loc, (
               (Ast.IdAcc
                 (_loc, ( (gm () ) ), (
                  (Ast.IdAcc
                    (_loc, ( (Ast.IdUid (_loc, "Action")) ), (
                     (Ast.IdLid (_loc, "mk")) ))) ))) ))) ), (
            (make_ctyp_expr t tvar e) ))) )))
   | TXlist (_loc, min, t, ts) ->
      let txt = (make_expr entry "" ( t.text )) in
      (match (min, ts) with
       | (false, None) ->
          (Ast.ExApp (_loc, ( (Ast.ExVrn (_loc, "Slist0")) ), txt))
       | (true, None) ->
          (Ast.ExApp (_loc, ( (Ast.ExVrn (_loc, "Slist1")) ), txt))
       | (false, Some (s)) ->
          let x = (make_expr entry tvar ( s.text )) in
          (Ast.ExApp
            (_loc, ( (Ast.ExVrn (_loc, "Slist0sep")) ), (
             (Ast.ExTup (_loc, ( (Ast.ExCom (_loc, txt, x)) ))) )))
       | (true, Some (s)) ->
          let x = (make_expr entry tvar ( s.text )) in
          (Ast.ExApp
            (_loc, ( (Ast.ExVrn (_loc, "Slist1sep")) ), (
             (Ast.ExTup (_loc, ( (Ast.ExCom (_loc, txt, x)) ))) ))))
   | TXnext (_loc) -> (Ast.ExVrn (_loc, "Snext"))
   | TXnterm (_loc, n, lev) ->
      (match lev with
       | Some (lab) ->
          (Ast.ExApp
            (_loc, ( (Ast.ExVrn (_loc, "Snterml")) ), (
             (Ast.ExTup
               (_loc, (
                (Ast.ExCom
                  (_loc, (
                   (Ast.ExApp
                     (_loc, (
                      (Ast.ExId
                        (_loc, (
                         (Ast.IdAcc
                           (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, "obj"))
                            ))) ))) ), (
                      (Ast.ExTyc
                        (_loc, ( n.expr ), (
                         (Ast.TyApp
                           (_loc, (
                            (Ast.TyId
                              (_loc, (
                               (Ast.IdAcc
                                 (_loc, ( (gm () ) ), (
                                  (Ast.IdLid (_loc, "t")) ))) ))) ), (
                            (Ast.TyQuo (_loc, ( n.tvar ))) ))) ))) ))) ), (
                   (Ast.ExStr (_loc, lab)) ))) ))) )))
       | None ->
          if (( n.tvar ) = tvar) then ( (Ast.ExVrn (_loc, "Sself")) )
          else
           (Ast.ExApp
             (_loc, ( (Ast.ExVrn (_loc, "Snterm")) ), (
              (Ast.ExApp
                (_loc, (
                 (Ast.ExId
                   (_loc, (
                    (Ast.IdAcc
                      (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, "obj")) ))) )))
                 ), (
                 (Ast.ExTyc
                   (_loc, ( n.expr ), (
                    (Ast.TyApp
                      (_loc, (
                       (Ast.TyId
                         (_loc, (
                          (Ast.IdAcc
                            (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, "t")) )))
                          ))) ), ( (Ast.TyQuo (_loc, ( n.tvar ))) ))) ))) )))
              ))))
   | TXopt (_loc, t) ->
      (Ast.ExApp
        (_loc, ( (Ast.ExVrn (_loc, "Sopt")) ), ( (make_expr entry "" t) )))
   | TXtry (_loc, t) ->
      (Ast.ExApp
        (_loc, ( (Ast.ExVrn (_loc, "Stry")) ), ( (make_expr entry "" t) )))
   | TXrules (_loc, rl) ->
      (Ast.ExApp
        (_loc, (
         (Ast.ExApp
           (_loc, (
            (Ast.ExId
              (_loc, (
               (Ast.IdAcc
                 (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, "srules")) ))) )))
            ), ( entry.expr ))) ), ( (make_expr_rules _loc entry rl "") )))
   | TXself (_loc) -> (Ast.ExVrn (_loc, "Sself"))
   | TXkwd (_loc, kwd) ->
      (Ast.ExApp
        (_loc, ( (Ast.ExVrn (_loc, "Skeyword")) ), ( (Ast.ExStr (_loc, kwd))
         )))
   | TXtok (_loc, match_fun, attr, descr) ->
      (Ast.ExApp
        (_loc, ( (Ast.ExVrn (_loc, "Stoken")) ), (
         (Ast.ExTup
           (_loc, (
            (Ast.ExCom
              (_loc, match_fun, (
               (Ast.ExTup
                 (_loc, (
                  (Ast.ExCom
                    (_loc, ( (Ast.ExVrn (_loc, attr)) ), (
                     (Ast.ExStr (_loc, ( (Ast.safe_string_escaped descr) )))
                     ))) ))) ))) ))) )))
and make_expr_rules =
 fun _loc ->
  fun n ->
   fun rl ->
    fun tvar ->
     (List.fold_left (
       fun txt ->
        fun (sl, ac) ->
         let sl =
          (List.fold_right (
            fun t ->
             fun txt ->
              let x = (make_expr n tvar t) in
              (Ast.ExApp
                (_loc, (
                 (Ast.ExApp
                   (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) )))
                    ), x)) ), txt)) ) sl (
            (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "[]")) ))) )) in
         (Ast.ExApp
           (_loc, (
            (Ast.ExApp
              (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) ))) ), (
               (Ast.ExTup (_loc, ( (Ast.ExCom (_loc, sl, ac)) ))) ))) ), txt))
       ) ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "[]")) ))) ) rl)

let expr_of_delete_rule =
 fun _loc ->
  fun n ->
   fun sl ->
    let sl =
     (List.fold_right (
       fun s ->
        fun e ->
         (Ast.ExApp
           (_loc, (
            (Ast.ExApp
              (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) ))) ), (
               (make_expr n "" ( s.text )) ))) ), e)) ) sl (
       (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "[]")) ))) )) in
    (( n.expr ), sl)

let mk_name =
 fun _loc ->
  fun i ->
   {expr = ( (Ast.ExId (_loc, i)) ); tvar = ( (Ident.tvar_of_ident i) );
    loc = _loc}

let slist =
 fun loc -> fun min -> fun sep -> fun symb -> (TXlist (loc, min, symb, sep))

let text_of_entry =
 fun _loc ->
  fun e ->
   let ent =
    let x = e.name in
    let _loc = (e.name).loc in
    (Ast.ExTyc
      (_loc, ( x.expr ), (
       (Ast.TyApp
         (_loc, (
          (Ast.TyId
            (_loc, (
             (Ast.IdAcc (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, "t")) ))) )))
          ), ( (Ast.TyQuo (_loc, ( x.tvar ))) ))) ))) in
   let pos =
    (match e.pos with
     | Some (pos) ->
        (Ast.ExApp
          (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "Some")) ))) ), pos))
     | None -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "None")) )))) in
   let txt =
    (List.fold_right (
      fun level ->
       fun txt ->
        let lab =
         (match level.label with
          | Some (lab) ->
             (Ast.ExApp
               (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "Some")) ))) ),
                ( (Ast.ExStr (_loc, lab)) )))
          | None -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "None")) )))) in
        let ass =
         (match level.assoc with
          | Some (ass) ->
             (Ast.ExApp
               (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "Some")) ))) ),
                ass))
          | None -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "None")) )))) in
        let txt =
         let rl =
          (srules _loc ( (e.name).tvar ) ( level.rules ) ( (e.name).tvar )) in
         let e = (make_expr_rules _loc ( e.name ) rl ( (e.name).tvar )) in
         (Ast.ExApp
           (_loc, (
            (Ast.ExApp
              (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) ))) ), (
               (Ast.ExTup
                 (_loc, (
                  (Ast.ExCom (_loc, lab, ( (Ast.ExCom (_loc, ass, e)) ))) )))
               ))) ), txt)) in
        txt ) ( e.levels ) ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "[]")) )))
      )) in
   (ent, pos, txt)

let let_in_of_extend =
 fun _loc ->
  fun gram ->
   fun gl ->
    fun el ->
     fun default ->
      (match gl with
       | None -> default
       | Some (nl) ->
          (
          (check_use nl el)
          );
          let ll =
           let same_tvar = fun e -> fun n -> (( (e.name).tvar ) = ( n.tvar )) in
           (List.fold_right (
             fun e ->
              fun ll ->
               (match (e.name).expr with
                | Ast.ExId (_, Ast.IdLid (_, _)) ->
                   if (List.exists ( (same_tvar e) ) nl) then ll
                   else if (List.exists ( (same_tvar e) ) ll) then ll
                   else ( ( e.name ) ) :: ll 
                | _ -> ll) ) el [] ) in
          let local_binding_of_name =
           function
           | {expr = Ast.ExId (_, Ast.IdLid (_, i));
              tvar = x;
              loc = _loc} ->
              (Ast.BiEq
                (_loc, ( (Ast.PaId (_loc, ( (Ast.IdLid (_loc, i)) ))) ), (
                 (Ast.ExTyc
                   (_loc, (
                    (Ast.ExApp
                      (_loc, (
                       (Ast.ExId
                         (_loc, ( (Ast.IdLid (_loc, "grammar_entry_create"))
                          ))) ), ( (Ast.ExStr (_loc, i)) ))) ), (
                    (Ast.TyApp
                      (_loc, (
                       (Ast.TyId
                         (_loc, (
                          (Ast.IdAcc
                            (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, "t")) )))
                          ))) ), ( (Ast.TyQuo (_loc, x)) ))) ))) )))
           | _ -> (failwith "internal error in the Grammar extension") in
          let expr_of_name =
           fun {expr = e;
            tvar = x;
            loc = _loc} ->
            (Ast.ExTyc
              (_loc, e, (
               (Ast.TyApp
                 (_loc, (
                  (Ast.TyId
                    (_loc, (
                     (Ast.IdAcc
                       (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, "t")) ))) )))
                  ), ( (Ast.TyQuo (_loc, x)) ))) ))) in
          let e =
           (match ll with
            | [] -> default
            | (x :: xs) ->
               let locals =
                (List.fold_right (
                  fun name ->
                   fun acc ->
                    (Ast.BiAnd (_loc, acc, ( (local_binding_of_name name) )))
                  ) xs ( (local_binding_of_name x) )) in
               let entry_mk =
                (match gram with
                 | Some (g) ->
                    (Ast.ExApp
                      (_loc, (
                       (Ast.ExId
                         (_loc, (
                          (Ast.IdAcc
                            (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, "mk"))
                             ))) ))) ), ( (Ast.ExId (_loc, g)) )))
                 | None ->
                    (Ast.ExId
                      (_loc, (
                       (Ast.IdAcc
                         (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, "mk")) )))
                       )))) in
               (Ast.ExLet
                 (_loc, Ast.ReNil , (
                  (Ast.BiEq
                    (_loc, (
                     (Ast.PaId
                       (_loc, ( (Ast.IdLid (_loc, "grammar_entry_create")) )))
                     ), entry_mk)) ), (
                  (Ast.ExLet (_loc, Ast.ReNil , locals, default)) )))) in
          (match nl with
           | [] -> e
           | (x :: xs) ->
              let globals =
               (List.fold_right (
                 fun name ->
                  fun acc ->
                   (Ast.BiAnd
                     (_loc, acc, (
                      (Ast.BiEq
                        (_loc, ( (Ast.PaAny (_loc)) ), ( (expr_of_name name)
                         ))) ))) ) xs (
                 (Ast.BiEq
                   (_loc, ( (Ast.PaAny (_loc)) ), ( (expr_of_name x) ))) )) in
              (Ast.ExLet (_loc, Ast.ReNil , globals, e))))

let text_of_functorial_extend =
 fun _loc ->
  fun gram ->
   fun gl ->
    fun el ->
     let args =
      let el =
       (List.map (
         fun e ->
          let (ent, pos, txt) = (text_of_entry ( (e.name).loc ) e) in
          (Ast.ExApp
            (_loc, (
             (Ast.ExApp
               (_loc, (
                (Ast.ExId
                  (_loc, (
                   (Ast.IdAcc
                     (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, "extend")) )))
                   ))) ), ent)) ), (
             (Ast.ExApp
               (_loc, (
                (Ast.ExFun
                  (_loc, (
                   (Ast.McArr
                     (_loc, ( (Ast.PaId (_loc, ( (Ast.IdUid (_loc, "()")) )))
                      ), ( (Ast.ExNil (_loc)) ), (
                      (Ast.ExTup (_loc, ( (Ast.ExCom (_loc, pos, txt)) ))) )))
                   ))) ), ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "()")) )))
                ))) ))) ) el) in
      (match el with
       | [] -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "()")) )))
       | (e :: []) -> e
       | (e :: el) ->
          (Ast.ExSeq
            (_loc, (
             (List.fold_left ( fun acc -> fun x -> (Ast.ExSem (_loc, acc, x))
               ) e el) )))) in
     (let_in_of_extend _loc gram gl el args)

let mk_tok =
 fun _loc ->
  fun ?restrict ->
   fun p ->
    fun t ->
     (match restrict with
      | None ->
         let p' = (Camlp4Ast.wildcarder#patt p) in
         let match_fun =
          if (Camlp4Ast.is_irrefut_patt p') then
           (
           (Ast.ExFun
             (_loc, (
              (Ast.McArr
                (_loc, p', ( (Ast.ExNil (_loc)) ), (
                 (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "True")) ))) ))) )))
           )
          else
           (Ast.ExFun
             (_loc, (
              (Ast.McOr
                (_loc, (
                 (Ast.McArr
                   (_loc, p', ( (Ast.ExNil (_loc)) ), (
                    (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "True")) ))) ))) ),
                 (
                 (Ast.McArr
                   (_loc, ( (Ast.PaAny (_loc)) ), ( (Ast.ExNil (_loc)) ), (
                    (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "False")) ))) ))) )))
              ))) in
         let descr = (string_of_patt p') in
         let text = (TXtok (_loc, match_fun, "Normal", descr)) in
         {used = [] ; text = text; styp = t; pattern = ( (Some (p)) )}
      | Some (restrict) ->
         let p' = (Camlp4Ast.wildcarder#patt p) in
         let match_fun =
          (Ast.ExFun
            (_loc, (
             (Ast.McOr
               (_loc, (
                (Ast.McArr
                  (_loc, p, restrict, (
                   (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "True")) ))) ))) ), (
                (Ast.McArr
                  (_loc, ( (Ast.PaAny (_loc)) ), ( (Ast.ExNil (_loc)) ), (
                   (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "False")) ))) ))) )))
             ))) in
         let descr = (string_of_patt p) in
         let text = (TXtok (_loc, match_fun, "Antiquot", descr)) in
         {used = [] ; text = text; styp = t; pattern = ( (Some (p')) )})

let sfold =
 fun _loc ->
  fun n ->
   fun foldfun ->
    fun f ->
     fun e ->
      fun s ->
       let styp = (STquo (_loc, ( (new_type_var () ) ))) in
       let e =
        (Ast.ExApp
          (_loc, (
           (Ast.ExApp
             (_loc, (
              (Ast.ExId
                (_loc, (
                 (Ast.IdAcc
                   (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, foldfun)) ))) )))
              ), f)) ), e)) in
       let t =
        (STapp
          (_loc, (
           (STapp
             (_loc, (
              (STtyp
                ((Ast.TyApp
                   (_loc, (
                    (Ast.TyId
                      (_loc, (
                       (Ast.IdAcc
                         (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, "fold")) )))
                       ))) ), ( (Ast.TyAny (_loc)) ))))) ), ( s.styp ))) ),
           styp)) in
       {used = ( s.used );
        text = ( (TXmeta (_loc, n, ( [( s.text )] ), e, t)) ); styp = styp;
        pattern = None }

let sfoldsep =
 fun _loc ->
  fun n ->
   fun foldfun ->
    fun f ->
     fun e ->
      fun s ->
       fun sep ->
        let styp = (STquo (_loc, ( (new_type_var () ) ))) in
        let e =
         (Ast.ExApp
           (_loc, (
            (Ast.ExApp
              (_loc, (
               (Ast.ExId
                 (_loc, (
                  (Ast.IdAcc
                    (_loc, ( (gm () ) ), ( (Ast.IdLid (_loc, foldfun)) ))) )))
               ), f)) ), e)) in
        let t =
         (STapp
           (_loc, (
            (STapp
              (_loc, (
               (STtyp
                 ((Ast.TyApp
                    (_loc, (
                     (Ast.TyId
                       (_loc, (
                        (Ast.IdAcc
                          (_loc, ( (gm () ) ), (
                           (Ast.IdLid (_loc, "foldsep")) ))) ))) ), (
                     (Ast.TyAny (_loc)) ))))) ), ( s.styp ))) ), styp)) in
        {used = ( (( s.used ) @ ( sep.used )) );
         text = ( (TXmeta (_loc, n, ( [( s.text ); ( sep.text )] ), e, t)) );
         styp = styp; pattern = None }