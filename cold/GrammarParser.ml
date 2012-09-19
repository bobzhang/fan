open FanSig

module Id =
              struct
               let name = "Camlp4GrammarParser"

               let version = Sys.ocaml_version

              end

module Make =
                    functor (Syntax : Camlp4.Sig.Camlp4Syntax) ->
                     struct
                      open Camlp4.Sig

                      include Syntax

                      module MetaLoc = Ast.Meta.MetaGhostLoc

                      module MetaAst = (Ast.Meta.Make)(MetaLoc)

                      module Ast2pt =
                       (Camlp4.Struct.Camlp4Ast2OCamlAst.Make)(Syntax.Ast)

                      let string_of_patt =
                       fun patt ->
                        let buf = (Buffer.create 42) in
                        let () =
                         (Format.bprintf buf "%a@?" (
                           fun fmt ->
                            fun p ->
                             (Pprintast.pattern fmt ( (Ast2pt.patt p) )) )
                           patt) in
                        let str = (Buffer.contents buf) in
                        if (str = "") then ( assert false ) else str

                      let split_ext = (ref false )

                      type loc = Loc.t

                      type 'e name = {expr:'e; tvar:string; loc:loc}

                      type styp =
                         STlid of loc * string
                       | STapp of loc * styp * styp
                       | STquo of loc * string
                       | STself of loc * string
                       | STtok of loc
                       | STstring_tok of loc
                       | STtyp of Ast.ctyp

                      type ('e, 'p) text =
                         TXmeta of loc * string * ('e, 'p) text list * 'e *
                          styp
                       | TXlist of loc * bool * ('e, 'p) symbol *
                          ('e, 'p) symbol option
                       | TXnext of loc
                       | TXnterm of loc * 'e name * string option
                       | TXopt of loc * ('e, 'p) text
                       | TXtry of loc * ('e, 'p) text
                       | TXrules of loc * (('e, 'p) text list * 'e) list
                       | TXself of loc
                       | TXkwd of loc * string
                       | TXtok of loc * 'e * string
                      and ('e, 'p) entry = {
                                             name:'e name;
                                             pos:'e option;
                                             levels:('e, 'p) level list}
                     and ('e, 'p) level = {
                                            label:string option;
                                            assoc:'e option;
                                            rules:('e, 'p) rule list}
                    and ('e, 'p) rule = {
                                          prod:('e, 'p) symbol list;
                                          action:'e option}
and ('e, 'p) symbol = {
                        used:string list;
                        text:('e, 'p) text;
                        styp:styp;
                        pattern:'p option}

 type used = Unused | UsedScanned | UsedNotScanned
 let _loc = Loc.ghost

 let gm = "Camlp4Grammar__"

 let mark_used =
  fun modif ->
   fun ht ->
    fun n ->
     (try
       let rll = (Hashtbl.find_all ht n) in
       (List.iter (
         fun (r, _) ->
          if (( !r ) == Unused )
          then
           begin
           ( (r := UsedNotScanned ) ); (modif := true )
          end else () ) rll)
      with
      Not_found -> ())

 let rec mark_symbol =
  fun modif ->
   fun ht ->
    fun symb ->
     (List.iter ( fun e -> (mark_used modif ht e) ) ( symb.used ))

 let check_use =
  fun nl ->
   fun el ->
    let ht = (Hashtbl.create 301) in
    let modif = (ref false ) in
    (
    (List.iter (
      fun e ->
       let u =
        (match (e.name).expr with
         | Ast.ExId (_, Ast.IdLid (_, _)) -> (Unused)
         | _ -> (UsedNotScanned)) in
       (Hashtbl.add ht ( (e.name).tvar ) (( (ref u) ), e)) ) el)
    );
    (
    (List.iter (
      fun n ->
       (try
         let rll = (Hashtbl.find_all ht ( n.tvar )) in
         (List.iter ( fun (r, _) -> (r := UsedNotScanned ) ) rll)
        with
        _ -> ()) ) nl)
    );
    (
    (modif := true )
    );
    while !modif do
     (
    (modif := false )
    );
     (Hashtbl.iter (
       fun _ ->
        fun (r, e) ->
         if (( !r ) = UsedNotScanned )
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
                (List.iter ( fun s -> (mark_symbol modif ht s) ) (
                  rule.prod )) ) rules) ) ( e.levels ))
         end else () ) ht)
    done;
    (Hashtbl.iter (
      fun s ->
       fun (r, e) ->
        if (( !r ) = Unused ) then
         (
         (print_warning ( (e.name).loc ) (
           ("Unused local entry \"" ^ ( (s ^ "\"") )) ))
         )
        else () ) ht)

 let new_type_var =
  let i = (ref 0) in
  fun ()  -> ( (incr i) ); ("e__" ^ ( (string_of_int ( !i )) ))

 let used_of_rule_list =
  fun rl ->
   (List.fold_left (
     fun nl ->
      fun r ->
       (List.fold_left ( fun nl -> fun s -> (( s.used ) @ nl) ) nl (
         r.prod )) ) []  rl)

 let retype_rule_list_without_patterns =
  fun _loc ->
   fun rl ->
    (try
      (List.map (
        function
        | {prod = (({pattern = None; styp = STtok (_)} as s) :: []);
           action = None} ->
           {prod = (
             [{s with
               pattern = (
                (Some ((Ast.PaId (_loc, ( (Ast.IdLid (_loc, "x")) ))))) )}]
             );
            action = (
             (Some
               ((Ast.ExApp
                  (_loc, (
                   (Ast.ExId
                     (_loc, (
                      (Ast.IdAcc
                        (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                         (Ast.IdAcc
                           (_loc, ( (Ast.IdUid (_loc, "Token")) ), (
                            (Ast.IdLid (_loc, "extract_string")) ))) )))
                      ))) ), (
                   (Ast.ExId (_loc, ( (Ast.IdLid (_loc, "x")) ))) ))))) )}
        | {prod = (({pattern = None} as s) :: []);
           action = None} ->
           {prod = (
             [{s with
               pattern = (
                (Some ((Ast.PaId (_loc, ( (Ast.IdLid (_loc, "x")) ))))) )}]
             );
            action = (
             (Some ((Ast.ExId (_loc, ( (Ast.IdLid (_loc, "x")) ))))) )}
        | ({prod = []; action = Some (_)} as r) -> r
        | _ -> (raise Exit ) ) rl)
     with
     Exit -> rl)
 let meta_action = (ref false )

 let mklistexp =
  fun _loc ->
   let rec loop =
    fun top ->
     function
     | [] -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "[]")) )))
     | (e1 :: el) ->
        let _loc =
         if top then _loc else (Loc.merge ( (Ast.loc_of_expr e1) ) _loc) in
        (Ast.ExApp
          (_loc, (
           (Ast.ExApp
             (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) ))) ),
              e1)) ), ( (loop false  el) ))) in
   (loop true )

 let mklistpat =
  fun _loc ->
   let rec loop =
    fun top ->
     function
     | [] -> (Ast.PaId (_loc, ( (Ast.IdUid (_loc, "[]")) )))
     | (p1 :: pl) ->
        let _loc =
         if top then _loc else (Loc.merge ( (Ast.loc_of_patt p1) ) _loc) in
        (Ast.PaApp
          (_loc, (
           (Ast.PaApp
             (_loc, ( (Ast.PaId (_loc, ( (Ast.IdUid (_loc, "::")) ))) ),
              p1)) ), ( (loop false  pl) ))) in
   (loop true )

 let rec expr_fa =
  fun al ->
   function
   | Ast.ExApp (_, f, a) -> (expr_fa ( ( a ) :: al  ) f)
   | f -> (f, al)

 let rec make_ctyp =
  fun styp ->
   fun tvar ->
    (match styp with
     | STlid (_loc, s) -> (Ast.TyId (_loc, ( (Ast.IdLid (_loc, s)) )))
     | STapp (_loc, t1, t2) ->
        (Ast.TyApp
          (_loc, ( (make_ctyp t1 tvar) ), ( (make_ctyp t2 tvar) )))
     | STquo (_loc, s) -> (Ast.TyQuo (_loc, s))
     | STself (_loc, x) ->
        if (tvar = "") then
         (
         (Loc.raise _loc (
           (Stream.Error
             ("'" ^ ( (x ^ "' illegal in anonymous entry level") ))) ))
         )
        else (Ast.TyQuo (_loc, tvar))
     | STtok (_loc) ->
        (Ast.TyId
          (_loc, (
           (Ast.IdAcc
             (_loc, (
              (Ast.IdAcc
                (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                 (Ast.IdUid (_loc, "Token")) ))) ), (
              (Ast.IdLid (_loc, "t")) ))) )))
     | STstring_tok (_loc) ->
        (Ast.TyId (_loc, ( (Ast.IdLid (_loc, "string")) )))
     | STtyp (t) -> t)

 let make_ctyp_patt =
  fun styp ->
   fun tvar ->
    fun patt ->
     let styp =
      (match styp with | STstring_tok (_loc) -> (STtok (_loc)) | t -> t) in
     (match (make_ctyp styp tvar) with
      | Ast.TyAny (_) -> patt
      | t ->
         let _loc = (Ast.loc_of_patt patt) in (Ast.PaTyc (_loc, patt, t)))

 let make_ctyp_expr =
  fun styp ->
   fun tvar ->
    fun expr ->
     (match (make_ctyp styp tvar) with
      | Ast.TyAny (_) -> expr
      | t ->
         let _loc = (Ast.loc_of_expr expr) in (Ast.ExTyc (_loc, expr, t)))

 let text_of_action =
  fun _loc ->
   fun psl ->
    fun rtvar ->
     fun act ->
      fun tvar ->
       let locid =
        (Ast.PaId (_loc, ( (Ast.IdLid (_loc, ( !Loc.name ))) ))) in
       let act =
        (match act with
         | Some (act) -> act
         | None -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "()")) )))) in
       let (tok_match_pl, act, _) =
        (List.fold_left (
          fun ((tok_match_pl, act, i) as accu) ->
           function
           | {pattern = None} -> accu
           | {pattern = Some (p)} when (Ast.is_irrefut_patt p) -> accu
           | {pattern =
               Some
                (Ast.PaAli
                  (_, Ast.PaApp (_, _, Ast.PaTup (_, Ast.PaAny (_))),
                   Ast.PaId (_, Ast.IdLid (_, s))))} ->
              (tok_match_pl, (
               (Ast.ExLet
                 (_loc, Ast.ReNil , (
                  (Ast.BiEq
                    (_loc, ( (Ast.PaId (_loc, ( (Ast.IdLid (_loc, s)) )))
                     ), (
                     (Ast.ExApp
                       (_loc, (
                        (Ast.ExId
                          (_loc, (
                           (Ast.IdAcc
                             (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                              (Ast.IdAcc
                                (_loc, ( (Ast.IdUid (_loc, "Token")) ), (
                                 (Ast.IdLid (_loc, "extract_string")) )))
                              ))) ))) ), (
                        (Ast.ExId (_loc, ( (Ast.IdLid (_loc, s)) ))) )))
                     ))) ), act)) ), i)
           | {pattern = Some (p);
              text = TXtok (_, _, _)} ->
              let id = ("__camlp4_" ^ ( (string_of_int i) )) in
              ((
               (Some
                 (match tok_match_pl with
                  | None ->
                     (( (Ast.ExId (_loc, ( (Ast.IdLid (_loc, id)) ))) ),
                      p)
                  | Some (tok_pl, match_pl) ->
                     ((
                      (Ast.ExCom
                        (_loc, (
                         (Ast.ExId (_loc, ( (Ast.IdLid (_loc, id)) ))) ),
                         tok_pl)) ), ( (Ast.PaCom (_loc, p, match_pl)) ))))
               ), act, ( (succ i) ))
           | _ -> accu ) (None , act, 0) psl) in
       let e =
        let e1 = (Ast.ExTyc (_loc, act, ( (Ast.TyQuo (_loc, rtvar)) ))) in
        let e2 =
         (match tok_match_pl with
          | None -> e1
          | Some (Ast.ExCom (_, t1, t2), Ast.PaCom (_, p1, p2)) ->
             (Ast.ExMat
               (_loc, (
                (Ast.ExTup (_loc, ( (Ast.ExCom (_loc, t1, t2)) ))) ), (
                (Ast.McOr
                  (_loc, (
                   (Ast.McArr
                     (_loc, (
                      (Ast.PaTup (_loc, ( (Ast.PaCom (_loc, p1, p2)) )))
                      ), ( (Ast.ExNil (_loc)) ), e1)) ), (
                   (Ast.McArr
                     (_loc, ( (Ast.PaAny (_loc)) ), ( (Ast.ExNil (_loc))
                      ), ( (Ast.ExAsf (_loc)) ))) ))) )))
          | Some (tok, match_) ->
             (Ast.ExMat
               (_loc, tok, (
                (Ast.McOr
                  (_loc, (
                   (Ast.McArr (_loc, match_, ( (Ast.ExNil (_loc)) ), e1))
                   ), (
                   (Ast.McArr
                     (_loc, ( (Ast.PaAny (_loc)) ), ( (Ast.ExNil (_loc))
                      ), ( (Ast.ExAsf (_loc)) ))) ))) )))) in
        (Ast.ExFun
          (_loc, (
           (Ast.McArr
             (_loc, (
              (Ast.PaTyc
                (_loc, locid, (
                 (Ast.TyId
                   (_loc, (
                    (Ast.IdAcc
                      (_loc, (
                       (Ast.IdAcc
                         (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                          (Ast.IdUid (_loc, "Loc")) ))) ), (
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
                      (_loc, ( (Ast.PaAny (_loc)) ), ( (Ast.ExNil (_loc))
                       ), txt)) ))) ), i)
             | Some
                (Ast.PaAli
                  (_, Ast.PaApp (_, _, Ast.PaTup (_, Ast.PaAny (_))), p)) ->
                let p = (make_ctyp_patt ( s.styp ) tvar p) in
                ((
                 (Ast.ExFun
                   (_loc, (
                    (Ast.McArr (_loc, p, ( (Ast.ExNil (_loc)) ), txt)) )))
                 ), i)
             | Some (p) when (Ast.is_irrefut_patt p) ->
                let p = (make_ctyp_patt ( s.styp ) tvar p) in
                ((
                 (Ast.ExFun
                   (_loc, (
                    (Ast.McArr (_loc, p, ( (Ast.ExNil (_loc)) ), txt)) )))
                 ), i)
             | Some (_) ->
                let p =
                 (make_ctyp_patt ( s.styp ) tvar (
                   (Ast.PaId
                     (_loc, (
                      (Ast.IdLid
                        (_loc, ( ("__camlp4_" ^ ( (string_of_int i) )) )))
                      ))) )) in
                ((
                 (Ast.ExFun
                   (_loc, (
                    (Ast.McArr (_loc, p, ( (Ast.ExNil (_loc)) ), txt)) )))
                 ), ( (succ i) ))) ) (e, 0) psl) in
       let txt =
        if !meta_action then
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
               (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                (Ast.IdAcc
                  (_loc, ( (Ast.IdUid (_loc, "Action")) ), (
                   (Ast.IdLid (_loc, "mk")) ))) ))) ))) ), txt))

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
                 (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) )))
                  ), ( (make_expr entry "" t) ))) ), el)) ) tl (
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
                     (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                      (Ast.IdUid (_loc, "Smeta")) ))) ))) ), (
                (Ast.ExStr (_loc, n)) ))) ), el)) ), (
          (Ast.ExApp
            (_loc, (
             (Ast.ExId
               (_loc, (
                (Ast.IdAcc
                  (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                   (Ast.IdAcc
                     (_loc, ( (Ast.IdUid (_loc, "Action")) ), (
                      (Ast.IdLid (_loc, "mk")) ))) ))) ))) ), (
             (make_ctyp_expr t tvar e) ))) )))
    | TXlist (_loc, min, t, ts) ->
       let txt = (make_expr entry "" ( t.text )) in
       (match (min, ts) with
        | (false, None) ->
           (Ast.ExApp
             (_loc, (
              (Ast.ExId
                (_loc, (
                 (Ast.IdAcc
                   (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                    (Ast.IdUid (_loc, "Slist0")) ))) ))) ), txt))
        | (true, None) ->
           (Ast.ExApp
             (_loc, (
              (Ast.ExId
                (_loc, (
                 (Ast.IdAcc
                   (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                    (Ast.IdUid (_loc, "Slist1")) ))) ))) ), txt))
        | (false, Some (s)) ->
           let x = (make_expr entry tvar ( s.text )) in
           (Ast.ExApp
             (_loc, (
              (Ast.ExApp
                (_loc, (
                 (Ast.ExId
                   (_loc, (
                    (Ast.IdAcc
                      (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                       (Ast.IdUid (_loc, "Slist0sep")) ))) ))) ), txt))
              ), x))
        | (true, Some (s)) ->
           let x = (make_expr entry tvar ( s.text )) in
           (Ast.ExApp
             (_loc, (
              (Ast.ExApp
                (_loc, (
                 (Ast.ExId
                   (_loc, (
                    (Ast.IdAcc
                      (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                       (Ast.IdUid (_loc, "Slist1sep")) ))) ))) ), txt))
              ), x)))
    | TXnext (_loc) ->
       (Ast.ExId
         (_loc, (
          (Ast.IdAcc
            (_loc, ( (Ast.IdUid (_loc, gm)) ), (
             (Ast.IdUid (_loc, "Snext")) ))) )))
    | TXnterm (_loc, n, lev) ->
       (match lev with
        | Some (lab) ->
           (Ast.ExApp
             (_loc, (
              (Ast.ExApp
                (_loc, (
                 (Ast.ExId
                   (_loc, (
                    (Ast.IdAcc
                      (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                       (Ast.IdUid (_loc, "Snterml")) ))) ))) ), (
                 (Ast.ExApp
                   (_loc, (
                    (Ast.ExId
                      (_loc, (
                       (Ast.IdAcc
                         (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                          (Ast.IdAcc
                            (_loc, ( (Ast.IdUid (_loc, "Entry")) ), (
                             (Ast.IdLid (_loc, "obj")) ))) ))) ))) ), (
                    (Ast.ExTyc
                      (_loc, ( n.expr ), (
                       (Ast.TyApp
                         (_loc, (
                          (Ast.TyId
                            (_loc, (
                             (Ast.IdAcc
                               (_loc, (
                                (Ast.IdAcc
                                  (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                                   (Ast.IdUid (_loc, "Entry")) ))) ), (
                                (Ast.IdLid (_loc, "t")) ))) ))) ), (
                          (Ast.TyQuo (_loc, ( n.tvar ))) ))) ))) ))) )))
              ), ( (Ast.ExStr (_loc, lab)) )))
        | None ->
           if (( n.tvar ) = tvar) then
            (
            (Ast.ExId
              (_loc, (
               (Ast.IdAcc
                 (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                  (Ast.IdUid (_loc, "Sself")) ))) )))
            )
           else
            (Ast.ExApp
              (_loc, (
               (Ast.ExId
                 (_loc, (
                  (Ast.IdAcc
                    (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                     (Ast.IdUid (_loc, "Snterm")) ))) ))) ), (
               (Ast.ExApp
                 (_loc, (
                  (Ast.ExId
                    (_loc, (
                     (Ast.IdAcc
                       (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                        (Ast.IdAcc
                          (_loc, ( (Ast.IdUid (_loc, "Entry")) ), (
                           (Ast.IdLid (_loc, "obj")) ))) ))) ))) ), (
                  (Ast.ExTyc
                    (_loc, ( n.expr ), (
                     (Ast.TyApp
                       (_loc, (
                        (Ast.TyId
                          (_loc, (
                           (Ast.IdAcc
                             (_loc, (
                              (Ast.IdAcc
                                (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                                 (Ast.IdUid (_loc, "Entry")) ))) ), (
                              (Ast.IdLid (_loc, "t")) ))) ))) ), (
                        (Ast.TyQuo (_loc, ( n.tvar ))) ))) ))) ))) ))))
    | TXopt (_loc, t) ->
       (Ast.ExApp
         (_loc, (
          (Ast.ExId
            (_loc, (
             (Ast.IdAcc
               (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                (Ast.IdUid (_loc, "Sopt")) ))) ))) ), (
          (make_expr entry "" t) )))
    | TXtry (_loc, t) ->
       (Ast.ExApp
         (_loc, (
          (Ast.ExId
            (_loc, (
             (Ast.IdAcc
               (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                (Ast.IdUid (_loc, "Stry")) ))) ))) ), (
          (make_expr entry "" t) )))
    | TXrules (_loc, rl) ->
       (Ast.ExApp
         (_loc, (
          (Ast.ExApp
            (_loc, (
             (Ast.ExId
               (_loc, (
                (Ast.IdAcc
                  (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                   (Ast.IdLid (_loc, "srules")) ))) ))) ), ( entry.expr
             ))) ), ( (make_expr_rules _loc entry rl "") )))
    | TXself (_loc) ->
       (Ast.ExId
         (_loc, (
          (Ast.IdAcc
            (_loc, ( (Ast.IdUid (_loc, gm)) ), (
             (Ast.IdUid (_loc, "Sself")) ))) )))
    | TXkwd (_loc, kwd) ->
       (Ast.ExApp
         (_loc, (
          (Ast.ExId
            (_loc, (
             (Ast.IdAcc
               (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                (Ast.IdUid (_loc, "Skeyword")) ))) ))) ), (
          (Ast.ExStr (_loc, kwd)) )))
    | TXtok (_loc, match_fun, descr) ->
       (Ast.ExApp
         (_loc, (
          (Ast.ExId
            (_loc, (
             (Ast.IdAcc
               (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                (Ast.IdUid (_loc, "Stoken")) ))) ))) ), (
          (Ast.ExTup
            (_loc, (
             (Ast.ExCom
               (_loc, match_fun, (
                (Ast.ExStr (_loc, ( (Ast.safe_string_escaped descr) )))
                ))) ))) )))
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
                    (_loc, (
                     (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) ))) ),
                     x)) ), txt)) ) sl (
             (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "[]")) ))) )) in
          (Ast.ExApp
            (_loc, (
             (Ast.ExApp
               (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) )))
                ), ( (Ast.ExTup (_loc, ( (Ast.ExCom (_loc, sl, ac)) )))
                ))) ), txt)) ) (
        (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "[]")) ))) ) rl)

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
               (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) )))
                ), ( (make_expr n "" ( s.text )) ))) ), e)) ) sl (
        (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "[]")) ))) )) in
     (( n.expr ), sl)

 let rec tvar_of_ident =
  function
  | (Ast.IdLid (_, x) | Ast.IdUid (_, x)) -> x
  | Ast.IdAcc (_, Ast.IdUid (_, x), xs) ->
     (x ^ ( ("__" ^ ( (tvar_of_ident xs) )) ))
  | _ -> (failwith "internal error in the Grammar extension")

 let mk_name =
  fun _loc ->
   fun i ->
    {expr = ( (Ast.ExId (_loc, i)) ); tvar = ( (tvar_of_ident i) );
     loc = _loc}

 let slist =
  fun loc ->
   fun min -> fun sep -> fun symb -> (TXlist (loc, min, symb, sep))

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
              (Ast.IdAcc
                (_loc, (
                 (Ast.IdAcc
                   (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                    (Ast.IdUid (_loc, "Entry")) ))) ), (
                 (Ast.IdLid (_loc, "t")) ))) ))) ), (
           (Ast.TyQuo (_loc, ( x.tvar ))) ))) ))) in
    let pos =
     (match e.pos with
      | Some (pos) ->
         (Ast.ExApp
           (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "Some")) ))) ),
            pos))
      | None -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "None")) )))) in
    let txt =
     (List.fold_right (
       fun level ->
        fun txt ->
         let lab =
          (match level.label with
           | Some (lab) ->
              (Ast.ExApp
                (_loc, (
                 (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "Some")) ))) ), (
                 (Ast.ExStr (_loc, lab)) )))
           | None -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "None")) )))) in
         let ass =
          (match level.assoc with
           | Some (ass) ->
              (Ast.ExApp
                (_loc, (
                 (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "Some")) ))) ),
                 ass))
           | None -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "None")) )))) in
         let txt =
          let rl =
           (srules _loc ( (e.name).tvar ) ( level.rules ) ( (e.name).tvar
             )) in
          let e = (make_expr_rules _loc ( e.name ) rl ( (e.name).tvar )) in
          (Ast.ExApp
            (_loc, (
             (Ast.ExApp
               (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) )))
                ), (
                (Ast.ExTup
                  (_loc, (
                   (Ast.ExCom (_loc, lab, ( (Ast.ExCom (_loc, ass, e)) )))
                   ))) ))) ), txt)) in
         txt ) ( e.levels ) (
       (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "[]")) ))) )) in
    (ent, pos, txt)

 let let_in_of_extend =
  fun _loc ->
   fun gram ->
    fun gl ->
     fun el ->
      fun args ->
       (match gl with
        | None -> args
        | Some (nl) ->
           (
           (check_use nl el)
           );
           let ll =
            let same_tvar =
             fun e -> fun n -> (( (e.name).tvar ) = ( n.tvar )) in
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
            fun {expr = e;
             tvar = x;
             loc = _loc} ->
             let i =
              (match e with
               | Ast.ExId (_, Ast.IdLid (_, i)) -> i
               | _ ->
                  (failwith "internal error in the Grammar extension")) in
             (Ast.BiEq
               (_loc, ( (Ast.PaId (_loc, ( (Ast.IdLid (_loc, i)) ))) ), (
                (Ast.ExTyc
                  (_loc, (
                   (Ast.ExApp
                     (_loc, (
                      (Ast.ExId
                        (_loc, (
                         (Ast.IdLid (_loc, "grammar_entry_create")) )))
                      ), ( (Ast.ExStr (_loc, i)) ))) ), (
                   (Ast.TyApp
                     (_loc, (
                      (Ast.TyId
                        (_loc, (
                         (Ast.IdAcc
                           (_loc, (
                            (Ast.IdAcc
                              (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                               (Ast.IdUid (_loc, "Entry")) ))) ), (
                            (Ast.IdLid (_loc, "t")) ))) ))) ), (
                      (Ast.TyQuo (_loc, x)) ))) ))) ))) in
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
                        (_loc, (
                         (Ast.IdAcc
                           (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                            (Ast.IdUid (_loc, "Entry")) ))) ), (
                         (Ast.IdLid (_loc, "t")) ))) ))) ), (
                   (Ast.TyQuo (_loc, x)) ))) ))) in
           let e =
            (match ll with
             | [] -> args
             | (x :: xs) ->
                let locals =
                 (List.fold_right (
                   fun name ->
                    fun acc ->
                     (Ast.BiAnd
                       (_loc, acc, ( (local_binding_of_name name) ))) )
                   xs ( (local_binding_of_name x) )) in
                let entry_mk =
                 (match gram with
                  | Some (g) ->
                     (Ast.ExApp
                       (_loc, (
                        (Ast.ExId
                          (_loc, (
                           (Ast.IdAcc
                             (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                              (Ast.IdAcc
                                (_loc, ( (Ast.IdUid (_loc, "Entry")) ), (
                                 (Ast.IdLid (_loc, "mk")) ))) ))) ))) ),
                        ( (Ast.ExId (_loc, g)) )))
                  | None ->
                     (Ast.ExId
                       (_loc, (
                        (Ast.IdAcc
                          (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                           (Ast.IdAcc
                             (_loc, ( (Ast.IdUid (_loc, "Entry")) ), (
                              (Ast.IdLid (_loc, "mk")) ))) ))) )))) in
                (Ast.ExLet
                  (_loc, Ast.ReNil , (
                   (Ast.BiEq
                     (_loc, (
                      (Ast.PaId
                        (_loc, (
                         (Ast.IdLid (_loc, "grammar_entry_create")) )))
                      ), entry_mk)) ), (
                   (Ast.ExLet (_loc, Ast.ReNil , locals, args)) )))) in
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
                         (_loc, ( (Ast.PaAny (_loc)) ), (
                          (expr_of_name name) ))) ))) ) xs (
                  (Ast.BiEq
                    (_loc, ( (Ast.PaAny (_loc)) ), ( (expr_of_name x) )))
                  )) in
               (Ast.ExLet (_loc, Ast.ReNil , globals, e))))

 class subst gmod =
  object
   inherit Ast.map as super
   method ident =
    function
    | Ast.IdUid (_, x) when (x = gm) -> gmod
    | x -> (super#ident x)
  end

 let subst_gmod = fun ast -> fun gmod -> ((((new subst) gmod)#expr) ast)

 let text_of_functorial_extend =
  fun _loc ->
   fun gmod ->
    fun gram ->
     fun gl ->
      fun el ->
       let args =
        let el =
         (List.map (
           fun e ->
            let (ent, pos, txt) = (text_of_entry ( (e.name).loc ) e) in
            let e =
             (Ast.ExApp
               (_loc, (
                (Ast.ExApp
                  (_loc, (
                   (Ast.ExId
                     (_loc, (
                      (Ast.IdAcc
                        (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                         (Ast.IdLid (_loc, "extend")) ))) ))) ), ent)) ),
                (
                (Ast.ExApp
                  (_loc, (
                   (Ast.ExFun
                     (_loc, (
                      (Ast.McArr
                        (_loc, (
                         (Ast.PaId (_loc, ( (Ast.IdUid (_loc, "()")) )))
                         ), ( (Ast.ExNil (_loc)) ), (
                         (Ast.ExTup
                           (_loc, ( (Ast.ExCom (_loc, pos, txt)) ))) )))
                      ))) ), (
                   (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "()")) ))) ))) ))) in
            if !split_ext then
             (
             (Ast.ExLet
               (_loc, Ast.ReNil , (
                (Ast.BiEq
                  (_loc, (
                   (Ast.PaId (_loc, ( (Ast.IdLid (_loc, "aux")) ))) ), (
                   (Ast.ExFun
                     (_loc, (
                      (Ast.McArr
                        (_loc, (
                         (Ast.PaId (_loc, ( (Ast.IdUid (_loc, "()")) )))
                         ), ( (Ast.ExNil (_loc)) ), e)) ))) ))) ), (
                (Ast.ExApp
                  (_loc, (
                   (Ast.ExId (_loc, ( (Ast.IdLid (_loc, "aux")) ))) ), (
                   (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "()")) ))) ))) )))
             )
            else e ) el) in
        (match el with
         | [] -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "()")) )))
         | (e :: []) -> e
         | (e :: el) ->
            (Ast.ExSeq
              (_loc, (
               (List.fold_left (
                 fun acc -> fun x -> (Ast.ExSem (_loc, acc, x)) ) e el)
               )))) in
       (subst_gmod ( (let_in_of_extend _loc gram gl el args) ) gmod)

 let wildcarder =
  object (self)
   inherit Ast.map as super
   method patt =
    function
    | Ast.PaId (_loc, Ast.IdLid (_, _)) -> (Ast.PaAny (_loc))
    | Ast.PaAli (_, p, _) -> (self#patt p)
    | p -> (super#patt p)
  end

 let mk_tok =
  fun _loc ->
   fun p ->
    fun t ->
     let p' = (wildcarder#patt p) in
     let match_fun =
      if (Ast.is_irrefut_patt p') then
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
     let text = (TXtok (_loc, match_fun, descr)) in
     {used = [] ; text = text; styp = t; pattern = ( (Some (p)) )}

 let symbol = (Gram.Entry.mk "symbol")

 let check_not_tok =
  fun s ->
   (match s with
    | {text = TXtok (_loc, _, _)} ->
       (Loc.raise _loc (
         (Stream.Error
           ("Deprecated syntax, use a sub rule. " ^
             "LIST0 STRING becomes LIST0 [ x = STRING -> x ]")) ))
    | _ -> ())
 let _ = (FanConfig.antiquotations := true )

 let _ = let _ = (expr : 'expr Gram.Entry.t)
         and _ = (symbol : 'symbol Gram.Entry.t) in
         let grammar_entry_create = Gram.Entry.mk in
         let extend_header =
          ((grammar_entry_create "extend_header") :
            'extend_header Gram.Entry.t)
         and semi_sep =
          ((grammar_entry_create "semi_sep") : 'semi_sep Gram.Entry.t)
         and string =
          ((grammar_entry_create "string") : 'string Gram.Entry.t)
         and name = ((grammar_entry_create "name") : 'name Gram.Entry.t)
         and comma_patt =
          ((grammar_entry_create "comma_patt") :
            'comma_patt Gram.Entry.t)
         and pattern =
          ((grammar_entry_create "pattern") : 'pattern Gram.Entry.t)
         and psymbol =
          ((grammar_entry_create "psymbol") : 'psymbol Gram.Entry.t)
         and rule = ((grammar_entry_create "rule") : 'rule Gram.Entry.t)
         and rule_list =
          ((grammar_entry_create "rule_list") : 'rule_list Gram.Entry.t)
         and assoc =
          ((grammar_entry_create "assoc") : 'assoc Gram.Entry.t)
         and level =
          ((grammar_entry_create "level") : 'level Gram.Entry.t)
         and level_list =
          ((grammar_entry_create "level_list") :
            'level_list Gram.Entry.t)
         and position =
          ((grammar_entry_create "position") : 'position Gram.Entry.t)
         and entry =
          ((grammar_entry_create "entry") : 'entry Gram.Entry.t)
         and global =
          ((grammar_entry_create "global") : 'global Gram.Entry.t)
         and t_qualid =
          ((grammar_entry_create "t_qualid") : 't_qualid Gram.Entry.t)
         and qualid =
          ((grammar_entry_create "qualid") : 'qualid Gram.Entry.t)
         and qualuid =
          ((grammar_entry_create "qualuid") : 'qualuid Gram.Entry.t)
         and delete_rule_body =
          ((grammar_entry_create "delete_rule_body") :
            'delete_rule_body Gram.Entry.t)
         and extend_body =
          ((grammar_entry_create "extend_body") :
            'extend_body Gram.Entry.t) in
         (
         (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
           ((fun ()
               ->
              (( (Some ((Camlp4.Sig.Grammar.After ("top")))) ), (
               [(None , None , (
                 [(( [( (Gram.Skeyword ("GEXTEND")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       ((Loc.raise _loc (
                          (Stream.Error
                            ("Deprecated syntax, use EXTEND MyGramModule ... END instead"))
                          )) : 'expr) )) ));
                  (( [( (Gram.Skeyword ("GDELETE_RULE")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       ((Loc.raise _loc (
                          (Stream.Error
                            ("Deprecated syntax, use DELETE_RULE MyGramModule ... END instead"))
                          )) : 'expr) )) ));
                  ((
                   [( (Gram.Skeyword ("DELETE_RULE")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (delete_rule_body :
                          'delete_rule_body Gram.Entry.t) ))) ); (
                    (Gram.Skeyword ("END")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (e :
                        'delete_rule_body) ->
                       fun _ -> fun (_loc : Gram.Loc.t) -> (e : 'expr) ))
                   ));
                  ((
                   [( (Gram.Skeyword ("EXTEND")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (extend_body : 'extend_body Gram.Entry.t) ))) );
                    ( (Gram.Skeyword ("END")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (e :
                        'extend_body) ->
                       fun _ -> fun (_loc : Gram.Loc.t) -> (e : 'expr) ))
                   ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (extend_header : 'extend_header Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Snterm
                      (Gram.Entry.obj ( (qualuid : 'qualuid Gram.Entry.t)
                        ))) )] ), (
                   (Gram.Action.mk (
                     fun (g :
                       'qualuid) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       ((None , g) : 'extend_header) )) ));
                  ((
                   [( (Gram.Skeyword ("(")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj ( (qualid : 'qualid Gram.Entry.t)
                        ))) ); ( (Gram.Skeyword (":")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (t_qualid : 't_qualid Gram.Entry.t) ))) ); (
                    (Gram.Skeyword (")")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (t :
                        't_qualid) ->
                       fun _ ->
                        fun (i :
                          'qualid) ->
                         fun _ ->
                          fun (_loc :
                            Gram.Loc.t) ->
                           ((( (Some (i)) ), t) : 'extend_header) )) ))]
                 ))] ))) () ) ))
         );
         (
         (Gram.extend ( (extend_body : 'extend_body Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (extend_header : 'extend_header Gram.Entry.t) )))
                    ); (
                    (Gram.Sopt
                      ((Gram.Snterm
                         (Gram.Entry.obj (
                           (global : 'global Gram.Entry.t) ))))) ); (
                    (Gram.Slist1
                      (Gram.srules extend_body (
                        [((
                          [(
                           (Gram.Snterm
                             (Gram.Entry.obj (
                               (entry : 'entry Gram.Entry.t) ))) ); (
                           (Gram.Snterm
                             (Gram.Entry.obj (
                               (semi_sep : 'semi_sep Gram.Entry.t) ))) )]
                          ), (
                          (Gram.Action.mk (
                            fun _ ->
                             fun (e :
                               'entry) ->
                              fun (_loc : Gram.Loc.t) -> (e : 'e__1) ))
                          ))] ))) )] ), (
                   (Gram.Action.mk (
                     fun (el :
                       'e__1 list) ->
                      fun (global_list :
                        'global option) ->
                       fun ((gram, g) :
                         'extend_header) ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         ((text_of_functorial_extend _loc g gram
                            global_list el) : 'extend_body) )) ))] ))] )))
             () ) ))
         );
         (
         (Gram.extend (
           (delete_rule_body : 'delete_rule_body Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Snterm
                      (Gram.Entry.obj ( (qualuid : 'qualuid Gram.Entry.t)
                        ))) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj ( (name : 'name Gram.Entry.t) )))
                    ); ( (Gram.Skeyword (":")) ); (
                    (Gram.Slist0sep
                      ((
                       (Gram.Snterm
                         (Gram.Entry.obj (
                           (symbol : 'symbol Gram.Entry.t) ))) ), (
                       (Gram.Snterm
                         (Gram.Entry.obj (
                           (semi_sep : 'semi_sep Gram.Entry.t) ))) ))) )]
                   ), (
                   (Gram.Action.mk (
                     fun (sl :
                       'symbol list) ->
                      fun _ ->
                       fun (n :
                         'name) ->
                        fun (g :
                          'qualuid) ->
                         fun (_loc :
                           Gram.Loc.t) ->
                          (let (e, b) = (expr_of_delete_rule _loc n sl) in
                           (subst_gmod (
                             (Ast.ExApp
                               (_loc, (
                                (Ast.ExApp
                                  (_loc, (
                                   (Ast.ExId
                                     (_loc, (
                                      (Ast.IdAcc
                                        (_loc, ( (Ast.IdUid (_loc, gm))
                                         ), (
                                         (Ast.IdLid (_loc, "delete_rule"))
                                         ))) ))) ), e)) ), b)) ) g) :
                            'delete_rule_body) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (qualuid : 'qualuid Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.srules qualuid (
                      [((
                        [(
                         (Gram.Stoken
                           ((
                            function
                            | UIDENT ("GLOBAL") -> (true)
                            | _ -> (false) ), "UIDENT (\"GLOBAL\")")) )]
                        ), (
                        (Gram.Action.mk (
                          fun (__camlp4_0 :
                            Gram.Token.t) ->
                           fun (_loc :
                             Gram.Loc.t) ->
                            (match __camlp4_0 with
                             | UIDENT ("GLOBAL") -> (() : 'e__2)
                             | _ -> assert false) )) ));
                       ((
                        [(
                         (Gram.Stoken
                           ((
                            function
                            | LIDENT (_) -> (true)
                            | _ -> (false) ), "LIDENT (_)")) )] ), (
                        (Gram.Action.mk (
                          fun (__camlp4_0 :
                            Gram.Token.t) ->
                           fun (_loc :
                             Gram.Loc.t) ->
                            (match __camlp4_0 with
                             | LIDENT (_) -> (() : 'e__2)
                             | _ -> assert false) )) ))] )) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       ((Loc.raise _loc (
                          (Stream.Error
                            ("Deprecated syntax, the grammar module is expected"))
                          )) : 'qualuid) )) ))] ));
                (None , None , (
                 [((
                   [(
                    (Gram.Stoken
                      (( function | UIDENT (_) -> (true) | _ -> (false)
                       ), "UIDENT _")) )] ), (
                   (Gram.Action.mk (
                     fun (i :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (let i = (Gram.Token.extract_string i) in
                        (Ast.IdUid (_loc, i)) : 'qualuid) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | UIDENT (_) -> (true) | _ -> (false)
                       ), "UIDENT _")) ); ( (Gram.Skeyword (".")) );
                    Gram.Sself ] ), (
                   (Gram.Action.mk (
                     fun (xs :
                       'qualuid) ->
                      fun _ ->
                       fun (x :
                         Gram.Token.t) ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         (let x = (Gram.Token.extract_string x) in
                          (Ast.IdAcc
                            (_loc, ( (Ast.IdUid (_loc, x)) ), xs)) :
                           'qualuid) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (qualid : 'qualid Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Stoken
                      (( function | LIDENT (_) -> (true) | _ -> (false)
                       ), "LIDENT _")) )] ), (
                   (Gram.Action.mk (
                     fun (i :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (let i = (Gram.Token.extract_string i) in
                        (Ast.IdLid (_loc, i)) : 'qualid) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | UIDENT (_) -> (true) | _ -> (false)
                       ), "UIDENT _")) )] ), (
                   (Gram.Action.mk (
                     fun (i :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (let i = (Gram.Token.extract_string i) in
                        (Ast.IdUid (_loc, i)) : 'qualid) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | UIDENT (_) -> (true) | _ -> (false)
                       ), "UIDENT _")) ); ( (Gram.Skeyword (".")) );
                    Gram.Sself ] ), (
                   (Gram.Action.mk (
                     fun (xs :
                       'qualid) ->
                      fun _ ->
                       fun (x :
                         Gram.Token.t) ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         (let x = (Gram.Token.extract_string x) in
                          (Ast.IdAcc
                            (_loc, ( (Ast.IdUid (_loc, x)) ), xs)) :
                           'qualid) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (t_qualid : 't_qualid Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | (LIDENT (_) | UIDENT (_)) -> (true)
                       | _ -> (false) ), "(LIDENT (_) | UIDENT (_))")) )]
                   ), (
                   (Gram.Action.mk (
                     fun (__camlp4_0 :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (match __camlp4_0 with
                        | (LIDENT (_) | UIDENT (_)) ->
                           ((Loc.raise _loc (
                              (Stream.Error
                                ("Wrong EXTEND header, the grammar type must finish by 't', "
                                  ^
                                  "like in EXTEND (g : Gram.t) ... END"))
                              )) : 't_qualid)
                        | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | UIDENT (_) -> (true) | _ -> (false)
                       ), "UIDENT _")) ); ( (Gram.Skeyword (".")) ); (
                    (Gram.Stoken
                      (( function | LIDENT ("t") -> (true) | _ -> (false)
                       ), "LIDENT (\"t\")")) )] ), (
                   (Gram.Action.mk (
                     fun (__camlp4_0 :
                       Gram.Token.t) ->
                      fun _ ->
                       fun (x :
                         Gram.Token.t) ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         (match __camlp4_0 with
                          | LIDENT ("t") ->
                             (let x = (Gram.Token.extract_string x) in
                              (Ast.IdUid (_loc, x)) : 't_qualid)
                          | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | UIDENT (_) -> (true) | _ -> (false)
                       ), "UIDENT _")) ); ( (Gram.Skeyword (".")) );
                    Gram.Sself ] ), (
                   (Gram.Action.mk (
                     fun (xs :
                       't_qualid) ->
                      fun _ ->
                       fun (x :
                         Gram.Token.t) ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         (let x = (Gram.Token.extract_string x) in
                          (Ast.IdAcc
                            (_loc, ( (Ast.IdUid (_loc, x)) ), xs)) :
                           't_qualid) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (global : 'global Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("GLOBAL") -> (true)
                       | _ -> (false) ), "UIDENT (\"GLOBAL\")")) ); (
                    (Gram.Skeyword (":")) ); (
                    (Gram.Slist1
                      ((Gram.Snterm
                         (Gram.Entry.obj ( (name : 'name Gram.Entry.t) )))))
                    ); (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (semi_sep : 'semi_sep Gram.Entry.t) ))) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (sl :
                        'name list) ->
                       fun _ ->
                        fun (__camlp4_0 :
                          Gram.Token.t) ->
                         fun (_loc :
                           Gram.Loc.t) ->
                          (match __camlp4_0 with
                           | UIDENT ("GLOBAL") -> (sl : 'global)
                           | _ -> assert false) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (entry : 'entry Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Snterm
                      (Gram.Entry.obj ( (name : 'name Gram.Entry.t) )))
                    ); ( (Gram.Skeyword (":")) ); (
                    (Gram.Sopt
                      ((Gram.Snterm
                         (Gram.Entry.obj (
                           (position : 'position Gram.Entry.t) ))))) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (level_list : 'level_list Gram.Entry.t) ))) )] ),
                   (
                   (Gram.Action.mk (
                     fun (ll :
                       'level_list) ->
                      fun (pos :
                        'position option) ->
                       fun _ ->
                        fun (n :
                          'name) ->
                         fun (_loc :
                           Gram.Loc.t) ->
                          ({name = n; pos = pos; levels = ll} : 'entry)
                     )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (position : 'position Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("LEVEL") -> (true)
                       | _ -> (false) ), "UIDENT (\"LEVEL\")")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj ( (string : 'string Gram.Entry.t)
                        ))) )] ), (
                   (Gram.Action.mk (
                     fun (n :
                       'string) ->
                      fun (__camlp4_0 :
                        Gram.Token.t) ->
                       fun (_loc :
                         Gram.Loc.t) ->
                        (match __camlp4_0 with
                         | UIDENT ("LEVEL") ->
                            ((Ast.ExApp
                               (_loc, (
                                (Ast.ExId
                                  (_loc, (
                                   (Ast.IdAcc
                                     (_loc, (
                                      (Ast.IdUid (_loc, "Camlp4")) ), (
                                      (Ast.IdAcc
                                        (_loc, (
                                         (Ast.IdUid (_loc, "Sig")) ), (
                                         (Ast.IdAcc
                                           (_loc, (
                                            (Ast.IdUid (_loc, "Grammar"))
                                            ), (
                                            (Ast.IdUid (_loc, "Level"))
                                            ))) ))) ))) ))) ), n)) :
                              'position)
                         | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("AFTER") -> (true)
                       | _ -> (false) ), "UIDENT (\"AFTER\")")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj ( (string : 'string Gram.Entry.t)
                        ))) )] ), (
                   (Gram.Action.mk (
                     fun (n :
                       'string) ->
                      fun (__camlp4_0 :
                        Gram.Token.t) ->
                       fun (_loc :
                         Gram.Loc.t) ->
                        (match __camlp4_0 with
                         | UIDENT ("AFTER") ->
                            ((Ast.ExApp
                               (_loc, (
                                (Ast.ExId
                                  (_loc, (
                                   (Ast.IdAcc
                                     (_loc, (
                                      (Ast.IdUid (_loc, "Camlp4")) ), (
                                      (Ast.IdAcc
                                        (_loc, (
                                         (Ast.IdUid (_loc, "Sig")) ), (
                                         (Ast.IdAcc
                                           (_loc, (
                                            (Ast.IdUid (_loc, "Grammar"))
                                            ), (
                                            (Ast.IdUid (_loc, "After"))
                                            ))) ))) ))) ))) ), n)) :
                              'position)
                         | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("BEFORE") -> (true)
                       | _ -> (false) ), "UIDENT (\"BEFORE\")")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj ( (string : 'string Gram.Entry.t)
                        ))) )] ), (
                   (Gram.Action.mk (
                     fun (n :
                       'string) ->
                      fun (__camlp4_0 :
                        Gram.Token.t) ->
                       fun (_loc :
                         Gram.Loc.t) ->
                        (match __camlp4_0 with
                         | UIDENT ("BEFORE") ->
                            ((Ast.ExApp
                               (_loc, (
                                (Ast.ExId
                                  (_loc, (
                                   (Ast.IdAcc
                                     (_loc, (
                                      (Ast.IdUid (_loc, "Camlp4")) ), (
                                      (Ast.IdAcc
                                        (_loc, (
                                         (Ast.IdUid (_loc, "Sig")) ), (
                                         (Ast.IdAcc
                                           (_loc, (
                                            (Ast.IdUid (_loc, "Grammar"))
                                            ), (
                                            (Ast.IdUid (_loc, "Before"))
                                            ))) ))) ))) ))) ), n)) :
                              'position)
                         | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("LAST") -> (true)
                       | _ -> (false) ), "UIDENT (\"LAST\")")) )] ), (
                   (Gram.Action.mk (
                     fun (__camlp4_0 :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (match __camlp4_0 with
                        | UIDENT ("LAST") ->
                           ((Ast.ExId
                              (_loc, (
                               (Ast.IdAcc
                                 (_loc, ( (Ast.IdUid (_loc, "Camlp4")) ),
                                  (
                                  (Ast.IdAcc
                                    (_loc, ( (Ast.IdUid (_loc, "Sig")) ),
                                     (
                                     (Ast.IdAcc
                                       (_loc, (
                                        (Ast.IdUid (_loc, "Grammar")) ),
                                        ( (Ast.IdUid (_loc, "Last")) )))
                                     ))) ))) ))) : 'position)
                        | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("FIRST") -> (true)
                       | _ -> (false) ), "UIDENT (\"FIRST\")")) )] ), (
                   (Gram.Action.mk (
                     fun (__camlp4_0 :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (match __camlp4_0 with
                        | UIDENT ("FIRST") ->
                           ((Ast.ExId
                              (_loc, (
                               (Ast.IdAcc
                                 (_loc, ( (Ast.IdUid (_loc, "Camlp4")) ),
                                  (
                                  (Ast.IdAcc
                                    (_loc, ( (Ast.IdUid (_loc, "Sig")) ),
                                     (
                                     (Ast.IdAcc
                                       (_loc, (
                                        (Ast.IdUid (_loc, "Grammar")) ),
                                        ( (Ast.IdUid (_loc, "First")) )))
                                     ))) ))) ))) : 'position)
                        | _ -> assert false) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (level_list : 'level_list Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [( (Gram.Skeyword ("[")) ); (
                    (Gram.Slist0sep
                      ((
                       (Gram.Snterm
                         (Gram.Entry.obj ( (level : 'level Gram.Entry.t)
                           ))) ), ( (Gram.Skeyword ("|")) ))) ); (
                    (Gram.Skeyword ("]")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (ll :
                        'level list) ->
                       fun _ ->
                        fun (_loc : Gram.Loc.t) -> (ll : 'level_list) ))
                   ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (level : 'level Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Sopt
                      (Gram.srules level (
                        [((
                          [(
                           (Gram.Stoken
                             ((
                              function
                              | STRING (_) -> (true)
                              | _ -> (false) ), "STRING _")) )] ), (
                          (Gram.Action.mk (
                            fun (x :
                              Gram.Token.t) ->
                             fun (_loc :
                               Gram.Loc.t) ->
                              (let x = (Gram.Token.extract_string x) in x :
                                'e__3) )) ))] ))) ); (
                    (Gram.Sopt
                      ((Gram.Snterm
                         (Gram.Entry.obj ( (assoc : 'assoc Gram.Entry.t)
                           ))))) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (rule_list : 'rule_list Gram.Entry.t) ))) )] ), (
                   (Gram.Action.mk (
                     fun (rules :
                       'rule_list) ->
                      fun (ass :
                        'assoc option) ->
                       fun (lab :
                         'e__3 option) ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         ({label = lab; assoc = ass; rules = rules} :
                           'level) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (assoc : 'assoc Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("NONA") -> (true)
                       | _ -> (false) ), "UIDENT (\"NONA\")")) )] ), (
                   (Gram.Action.mk (
                     fun (__camlp4_0 :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (match __camlp4_0 with
                        | UIDENT ("NONA") ->
                           ((Ast.ExId
                              (_loc, (
                               (Ast.IdAcc
                                 (_loc, ( (Ast.IdUid (_loc, "Camlp4")) ),
                                  (
                                  (Ast.IdAcc
                                    (_loc, ( (Ast.IdUid (_loc, "Sig")) ),
                                     (
                                     (Ast.IdAcc
                                       (_loc, (
                                        (Ast.IdUid (_loc, "Grammar")) ),
                                        ( (Ast.IdUid (_loc, "NonA")) )))
                                     ))) ))) ))) : 'assoc)
                        | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("RIGHTA") -> (true)
                       | _ -> (false) ), "UIDENT (\"RIGHTA\")")) )] ), (
                   (Gram.Action.mk (
                     fun (__camlp4_0 :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (match __camlp4_0 with
                        | UIDENT ("RIGHTA") ->
                           ((Ast.ExId
                              (_loc, (
                               (Ast.IdAcc
                                 (_loc, ( (Ast.IdUid (_loc, "Camlp4")) ),
                                  (
                                  (Ast.IdAcc
                                    (_loc, ( (Ast.IdUid (_loc, "Sig")) ),
                                     (
                                     (Ast.IdAcc
                                       (_loc, (
                                        (Ast.IdUid (_loc, "Grammar")) ),
                                        ( (Ast.IdUid (_loc, "RightA")) )))
                                     ))) ))) ))) : 'assoc)
                        | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("LEFTA") -> (true)
                       | _ -> (false) ), "UIDENT (\"LEFTA\")")) )] ), (
                   (Gram.Action.mk (
                     fun (__camlp4_0 :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (match __camlp4_0 with
                        | UIDENT ("LEFTA") ->
                           ((Ast.ExId
                              (_loc, (
                               (Ast.IdAcc
                                 (_loc, ( (Ast.IdUid (_loc, "Camlp4")) ),
                                  (
                                  (Ast.IdAcc
                                    (_loc, ( (Ast.IdUid (_loc, "Sig")) ),
                                     (
                                     (Ast.IdAcc
                                       (_loc, (
                                        (Ast.IdUid (_loc, "Grammar")) ),
                                        ( (Ast.IdUid (_loc, "LeftA")) )))
                                     ))) ))) ))) : 'assoc)
                        | _ -> assert false) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (rule_list : 'rule_list Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [( (Gram.Skeyword ("[")) ); (
                    (Gram.Slist1sep
                      ((
                       (Gram.Snterm
                         (Gram.Entry.obj ( (rule : 'rule Gram.Entry.t) )))
                       ), ( (Gram.Skeyword ("|")) ))) ); (
                    (Gram.Skeyword ("]")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (rules :
                        'rule list) ->
                       fun _ ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         ((retype_rule_list_without_patterns _loc rules) :
                           'rule_list) )) ));
                  ((
                   [( (Gram.Skeyword ("[")) ); ( (Gram.Skeyword ("]")) )]
                   ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun _ ->
                       fun (_loc : Gram.Loc.t) -> (([]) : 'rule_list) ))
                   ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (rule : 'rule Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Slist0sep
                      ((
                       (Gram.Snterm
                         (Gram.Entry.obj (
                           (psymbol : 'psymbol Gram.Entry.t) ))) ), (
                       (Gram.Snterm
                         (Gram.Entry.obj (
                           (semi_sep : 'semi_sep Gram.Entry.t) ))) ))) )]
                   ), (
                   (Gram.Action.mk (
                     fun (psl :
                       'psymbol list) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       ({prod = psl; action = None } : 'rule) )) ));
                  ((
                   [(
                    (Gram.Slist0sep
                      ((
                       (Gram.Snterm
                         (Gram.Entry.obj (
                           (psymbol : 'psymbol Gram.Entry.t) ))) ), (
                       (Gram.Snterm
                         (Gram.Entry.obj (
                           (semi_sep : 'semi_sep Gram.Entry.t) ))) ))) );
                    ( (Gram.Skeyword ("->")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj ( (expr : 'expr Gram.Entry.t) )))
                    )] ), (
                   (Gram.Action.mk (
                     fun (act :
                       'expr) ->
                      fun _ ->
                       fun (psl :
                         'psymbol list) ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         ({prod = psl; action = ( (Some (act)) )} :
                           'rule) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (psymbol : 'psymbol Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Snterm
                      (Gram.Entry.obj ( (symbol : 'symbol Gram.Entry.t)
                        ))) )] ), (
                   (Gram.Action.mk (
                     fun (s :
                       'symbol) ->
                      fun (_loc : Gram.Loc.t) -> (s : 'psymbol) )) ));
                  ((
                   [(
                    (Gram.Snterm
                      (Gram.Entry.obj ( (pattern : 'pattern Gram.Entry.t)
                        ))) ); ( (Gram.Skeyword ("=")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj ( (symbol : 'symbol Gram.Entry.t)
                        ))) )] ), (
                   (Gram.Action.mk (
                     fun (s :
                       'symbol) ->
                      fun _ ->
                       fun (p :
                         'pattern) ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         ((match s.pattern with
                           | Some
                              (Ast.PaApp
                                (_, Ast.PaId (_, Ast.IdUid (_, u)),
                                 Ast.PaTup (_, Ast.PaAny (_)))) ->
                              (mk_tok _loc (
                                (Ast.PaApp
                                  (_loc, (
                                   (Ast.PaId
                                     (_loc, ( (Ast.IdUid (_loc, u)) )))
                                   ), p)) ) ( s.styp ))
                           | _ -> {s with pattern = ( (Some (p)) )}) :
                           'psymbol) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | LIDENT (_) -> (true) | _ -> (false)
                       ), "LIDENT _")) ); (
                    (Gram.Sopt
                      (Gram.srules psymbol (
                        [((
                          [(
                           (Gram.Stoken
                             ((
                              function
                              | UIDENT ("LEVEL") -> (true)
                              | _ -> (false) ), "UIDENT (\"LEVEL\")")) );
                           (
                           (Gram.Stoken
                             ((
                              function
                              | STRING (_) -> (true)
                              | _ -> (false) ), "STRING _")) )] ), (
                          (Gram.Action.mk (
                            fun (s :
                              Gram.Token.t) ->
                             fun (__camlp4_0 :
                               Gram.Token.t) ->
                              fun (_loc :
                                Gram.Loc.t) ->
                               (match __camlp4_0 with
                                | UIDENT ("LEVEL") ->
                                   (let s = (Gram.Token.extract_string s) in
                                    s : 'e__4)
                                | _ -> assert false) )) ))] ))) )] ), (
                   (Gram.Action.mk (
                     fun (lev :
                       'e__4 option) ->
                      fun (i :
                        Gram.Token.t) ->
                       fun (_loc :
                         Gram.Loc.t) ->
                        (let i = (Gram.Token.extract_string i) in
                         let name =
                          (mk_name _loc ( (Ast.IdLid (_loc, i)) )) in
                         let text = (TXnterm (_loc, name, lev)) in
                         let styp = (STquo (_loc, i)) in
                         {used = ( [i] ); text = text; styp = styp;
                          pattern = None } : 'psymbol) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | LIDENT (_) -> (true) | _ -> (false)
                       ), "LIDENT _")) ); ( (Gram.Skeyword ("=")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj ( (symbol : 'symbol Gram.Entry.t)
                        ))) )] ), (
                   (Gram.Action.mk (
                     fun (s :
                       'symbol) ->
                      fun _ ->
                       fun (p :
                         Gram.Token.t) ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         (let p = (Gram.Token.extract_string p) in
                          (match s.pattern with
                           | Some
                              (Ast.PaApp
                                (_, Ast.PaId (_, Ast.IdUid (_, u)),
                                 Ast.PaTup (_, Ast.PaAny (_))) as p') ->
                              let match_fun =
                               (Ast.ExFun
                                 (_loc, (
                                  (Ast.McOr
                                    (_loc, (
                                     (Ast.McArr
                                       (_loc, p', ( (Ast.ExNil (_loc)) ),
                                        (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdUid (_loc, "True")) )))
                                        ))) ), (
                                     (Ast.McArr
                                       (_loc, ( (Ast.PaAny (_loc)) ), (
                                        (Ast.ExNil (_loc)) ), (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdUid (_loc, "False")) )))
                                        ))) ))) ))) in
                              let p' =
                               (Ast.PaAli
                                 (_loc, p', (
                                  (Ast.PaId
                                    (_loc, ( (Ast.IdLid (_loc, p)) ))) ))) in
                              let descr = (u ^ " _") in
                              let text = (TXtok (_loc, match_fun, descr)) in
                              {s with text = text;
                               pattern = ( (Some (p')) )}
                           | _ ->
                              {s with
                               pattern = (
                                (Some
                                  ((Ast.PaId
                                     (_loc, ( (Ast.IdLid (_loc, p)) )))))
                                )}) : 'psymbol) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (symbol : 'symbol Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(( (Some ("top")) ), ( (Some ((Camlp4.Sig.Grammar.NonA)))
                 ), (
                 [((
                   [(
                    (Gram.Stoken
                      ((
                       function | UIDENT ("TRY") -> (true) | _ -> (false)
                       ), "UIDENT (\"TRY\")")) ); Gram.Sself ] ), (
                   (Gram.Action.mk (
                     fun (s :
                       'symbol) ->
                      fun (__camlp4_0 :
                        Gram.Token.t) ->
                       fun (_loc :
                         Gram.Loc.t) ->
                        (match __camlp4_0 with
                         | UIDENT ("TRY") ->
                            (let text = (TXtry (_loc, ( s.text ))) in
                             {used = ( s.used ); text = text;
                              styp = ( s.styp ); pattern = None } :
                              'symbol)
                         | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function | UIDENT ("OPT") -> (true) | _ -> (false)
                       ), "UIDENT (\"OPT\")")) ); Gram.Sself ] ), (
                   (Gram.Action.mk (
                     fun (s :
                       'symbol) ->
                      fun (__camlp4_0 :
                        Gram.Token.t) ->
                       fun (_loc :
                         Gram.Loc.t) ->
                        (match __camlp4_0 with
                         | UIDENT ("OPT") ->
                            (let () = (check_not_tok s) in
                             let styp =
                              (STapp
                                (_loc, ( (STlid (_loc, "option")) ), (
                                 s.styp ))) in
                             let text = (TXopt (_loc, ( s.text ))) in
                             {used = ( s.used ); text = text;
                              styp = styp; pattern = None } : 'symbol)
                         | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("LIST1") -> (true)
                       | _ -> (false) ), "UIDENT (\"LIST1\")")) );
                    Gram.Sself ; (
                    (Gram.Sopt
                      (Gram.srules symbol (
                        [((
                          [(
                           (Gram.Stoken
                             ((
                              function
                              | UIDENT ("SEP") -> (true)
                              | _ -> (false) ), "UIDENT (\"SEP\")")) ); (
                           (Gram.Snterm
                             (Gram.Entry.obj (
                               (symbol : 'symbol Gram.Entry.t) ))) )] ),
                          (
                          (Gram.Action.mk (
                            fun (t :
                              'symbol) ->
                             fun (__camlp4_0 :
                               Gram.Token.t) ->
                              fun (_loc :
                                Gram.Loc.t) ->
                               (match __camlp4_0 with
                                | UIDENT ("SEP") -> (t : 'e__6)
                                | _ -> assert false) )) ))] ))) )] ), (
                   (Gram.Action.mk (
                     fun (sep :
                       'e__6 option) ->
                      fun (s :
                        'symbol) ->
                       fun (__camlp4_0 :
                         Gram.Token.t) ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         (match __camlp4_0 with
                          | UIDENT ("LIST1") ->
                             (let () = (check_not_tok s) in
                              let used =
                               (match sep with
                                | Some (symb) ->
                                   (( symb.used ) @ ( s.used ))
                                | None -> s.used) in
                              let styp =
                               (STapp
                                 (_loc, ( (STlid (_loc, "list")) ), (
                                  s.styp ))) in
                              let text = (slist _loc true  sep s) in
                              {used = used; text = text; styp = styp;
                               pattern = None } : 'symbol)
                          | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("LIST0") -> (true)
                       | _ -> (false) ), "UIDENT (\"LIST0\")")) );
                    Gram.Sself ; (
                    (Gram.Sopt
                      (Gram.srules symbol (
                        [((
                          [(
                           (Gram.Stoken
                             ((
                              function
                              | UIDENT ("SEP") -> (true)
                              | _ -> (false) ), "UIDENT (\"SEP\")")) ); (
                           (Gram.Snterm
                             (Gram.Entry.obj (
                               (symbol : 'symbol Gram.Entry.t) ))) )] ),
                          (
                          (Gram.Action.mk (
                            fun (t :
                              'symbol) ->
                             fun (__camlp4_0 :
                               Gram.Token.t) ->
                              fun (_loc :
                                Gram.Loc.t) ->
                               (match __camlp4_0 with
                                | UIDENT ("SEP") -> (t : 'e__5)
                                | _ -> assert false) )) ))] ))) )] ), (
                   (Gram.Action.mk (
                     fun (sep :
                       'e__5 option) ->
                      fun (s :
                        'symbol) ->
                       fun (__camlp4_0 :
                         Gram.Token.t) ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         (match __camlp4_0 with
                          | UIDENT ("LIST0") ->
                             (let () = (check_not_tok s) in
                              let used =
                               (match sep with
                                | Some (symb) ->
                                   (( symb.used ) @ ( s.used ))
                                | None -> s.used) in
                              let styp =
                               (STapp
                                 (_loc, ( (STlid (_loc, "list")) ), (
                                  s.styp ))) in
                              let text = (slist _loc false  sep s) in
                              {used = used; text = text; styp = styp;
                               pattern = None } : 'symbol)
                          | _ -> assert false) )) ))] ));
                (None , None , (
                 [((
                   [( (Gram.Skeyword ("(")) ); Gram.Sself ; (
                    (Gram.Skeyword (")")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (s_t :
                        'symbol) ->
                       fun _ ->
                        fun (_loc : Gram.Loc.t) -> (s_t : 'symbol) )) ));
                  ((
                   [(
                    (Gram.Snterm
                      (Gram.Entry.obj ( (name : 'name Gram.Entry.t) )))
                    ); (
                    (Gram.Sopt
                      (Gram.srules symbol (
                        [((
                          [(
                           (Gram.Stoken
                             ((
                              function
                              | UIDENT ("LEVEL") -> (true)
                              | _ -> (false) ), "UIDENT (\"LEVEL\")")) );
                           (
                           (Gram.Stoken
                             ((
                              function
                              | STRING (_) -> (true)
                              | _ -> (false) ), "STRING _")) )] ), (
                          (Gram.Action.mk (
                            fun (s :
                              Gram.Token.t) ->
                             fun (__camlp4_0 :
                               Gram.Token.t) ->
                              fun (_loc :
                                Gram.Loc.t) ->
                               (match __camlp4_0 with
                                | UIDENT ("LEVEL") ->
                                   (let s = (Gram.Token.extract_string s) in
                                    s : 'e__8)
                                | _ -> assert false) )) ))] ))) )] ), (
                   (Gram.Action.mk (
                     fun (lev :
                       'e__8 option) ->
                      fun (n :
                        'name) ->
                       fun (_loc :
                         Gram.Loc.t) ->
                        ({used = ( [( n.tvar )] );
                          text = ( (TXnterm (_loc, n, lev)) );
                          styp = ( (STquo (_loc, ( n.tvar ))) );
                          pattern = None } : 'symbol) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | UIDENT (_) -> (true) | _ -> (false)
                       ), "UIDENT _")) ); ( (Gram.Skeyword (".")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj ( (qualid : 'qualid Gram.Entry.t)
                        ))) ); (
                    (Gram.Sopt
                      (Gram.srules symbol (
                        [((
                          [(
                           (Gram.Stoken
                             ((
                              function
                              | UIDENT ("LEVEL") -> (true)
                              | _ -> (false) ), "UIDENT (\"LEVEL\")")) );
                           (
                           (Gram.Stoken
                             ((
                              function
                              | STRING (_) -> (true)
                              | _ -> (false) ), "STRING _")) )] ), (
                          (Gram.Action.mk (
                            fun (s :
                              Gram.Token.t) ->
                             fun (__camlp4_0 :
                               Gram.Token.t) ->
                              fun (_loc :
                                Gram.Loc.t) ->
                               (match __camlp4_0 with
                                | UIDENT ("LEVEL") ->
                                   (let s = (Gram.Token.extract_string s) in
                                    s : 'e__7)
                                | _ -> assert false) )) ))] ))) )] ), (
                   (Gram.Action.mk (
                     fun (lev :
                       'e__7 option) ->
                      fun (il :
                        'qualid) ->
                       fun _ ->
                        fun (i :
                          Gram.Token.t) ->
                         fun (_loc :
                           Gram.Loc.t) ->
                          (let i = (Gram.Token.extract_string i) in
                           let n =
                            (mk_name _loc (
                              (Ast.IdAcc
                                (_loc, ( (Ast.IdUid (_loc, i)) ), il)) )) in
                           {used = ( [( n.tvar )] );
                            text = ( (TXnterm (_loc, n, lev)) );
                            styp = ( (STquo (_loc, ( n.tvar ))) );
                            pattern = None } : 'symbol) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | STRING (_) -> (true) | _ -> (false)
                       ), "STRING _")) )] ), (
                   (Gram.Action.mk (
                     fun (s :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (let s = (Gram.Token.extract_string s) in
                        {used = [] ; text = ( (TXkwd (_loc, s)) );
                         styp = ( (STtok (_loc)) ); pattern = None } :
                         'symbol) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | UIDENT (_) -> (true) | _ -> (false)
                       ), "UIDENT _")) ); (
                    (Gram.Stoken
                      ((
                       function
                       | ANTIQUOT ("", _) -> (true)
                       | _ -> (false) ), "ANTIQUOT (\"\", _)")) )] ), (
                   (Gram.Action.mk (
                     fun (__camlp4_0 :
                       Gram.Token.t) ->
                      fun (x :
                        Gram.Token.t) ->
                       fun (_loc :
                         Gram.Loc.t) ->
                        (match __camlp4_0 with
                         | ANTIQUOT ("", s) ->
                            (let x = (Gram.Token.extract_string x) in
                             let e = (AntiquotSyntax.parse_expr _loc s) in
                             let match_fun =
                              (Ast.ExFun
                                (_loc, (
                                 (Ast.McOr
                                   (_loc, (
                                    (Ast.McArr
                                      (_loc, (
                                       (Ast.PaApp
                                         (_loc, (
                                          (Ast.PaId
                                            (_loc, (
                                             (Ast.IdUid (_loc, x)) ))) ),
                                          (
                                          (Ast.PaId
                                            (_loc, (
                                             (Ast.IdLid
                                               (_loc, "camlp4_x")) ))) )))
                                       ), (
                                       (Ast.ExApp
                                         (_loc, (
                                          (Ast.ExApp
                                            (_loc, (
                                             (Ast.ExId
                                               (_loc, (
                                                (Ast.IdLid (_loc, "="))
                                                ))) ), (
                                             (Ast.ExId
                                               (_loc, (
                                                (Ast.IdLid
                                                  (_loc, "camlp4_x")) )))
                                             ))) ), e)) ), (
                                       (Ast.ExId
                                         (_loc, (
                                          (Ast.IdUid (_loc, "True")) )))
                                       ))) ), (
                                    (Ast.McArr
                                      (_loc, ( (Ast.PaAny (_loc)) ), (
                                       (Ast.ExNil (_loc)) ), (
                                       (Ast.ExId
                                         (_loc, (
                                          (Ast.IdUid (_loc, "False")) )))
                                       ))) ))) ))) in
                             let descr = ("$" ^ ( (x ^ ( (" " ^ s) )) )) in
                             let text = (TXtok (_loc, match_fun, descr)) in
                             let p =
                              (Ast.PaApp
                                (_loc, (
                                 (Ast.PaId
                                   (_loc, ( (Ast.IdUid (_loc, x)) ))) ),
                                 (
                                 (Ast.PaTup
                                   (_loc, ( (Ast.PaAny (_loc)) ))) ))) in
                             {used = [] ; text = text;
                              styp = ( (STtok (_loc)) );
                              pattern = ( (Some (p)) )} : 'symbol)
                         | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | UIDENT (_) -> (true) | _ -> (false)
                       ), "UIDENT _")) ); (
                    (Gram.Stoken
                      (( function | STRING (_) -> (true) | _ -> (false)
                       ), "STRING _")) )] ), (
                   (Gram.Action.mk (
                     fun (s :
                       Gram.Token.t) ->
                      fun (x :
                        Gram.Token.t) ->
                       fun (_loc :
                         Gram.Loc.t) ->
                        (let s = (Gram.Token.extract_string s) in
                         let x = (Gram.Token.extract_string x) in
                         (mk_tok _loc (
                           (Ast.PaApp
                             (_loc, (
                              (Ast.PaId (_loc, ( (Ast.IdUid (_loc, x)) )))
                              ), ( (Ast.PaStr (_loc, s)) ))) ) (
                           (STtok (_loc)) )) : 'symbol) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | UIDENT (_) -> (true) | _ -> (false)
                       ), "UIDENT _")) )] ), (
                   (Gram.Action.mk (
                     fun (x :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (let x = (Gram.Token.extract_string x) in
                        (mk_tok _loc (
                          (Ast.PaApp
                            (_loc, (
                             (Ast.PaId (_loc, ( (Ast.IdUid (_loc, x)) )))
                             ), (
                             (Ast.PaTup (_loc, ( (Ast.PaAny (_loc)) )))
                             ))) ) ( (STstring_tok (_loc)) )) : 'symbol)
                     )) ));
                  ((
                   [( (Gram.Skeyword ("`")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj ( (patt : 'patt Gram.Entry.t) )))
                    )] ), (
                   (Gram.Action.mk (
                     fun (p :
                       'patt) ->
                      fun _ ->
                       fun (_loc :
                         Gram.Loc.t) ->
                        ((mk_tok _loc p ( (STtok (_loc)) )) : 'symbol) ))
                   ));
                  ((
                   [( (Gram.Skeyword ("[")) ); (
                    (Gram.Slist0sep
                      ((
                       (Gram.Snterm
                         (Gram.Entry.obj ( (rule : 'rule Gram.Entry.t) )))
                       ), ( (Gram.Skeyword ("|")) ))) ); (
                    (Gram.Skeyword ("]")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (rl :
                        'rule list) ->
                       fun _ ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         (let rl =
                           (retype_rule_list_without_patterns _loc rl) in
                          let t = (new_type_var () ) in
                          {used = ( (used_of_rule_list rl) );
                           text = (
                            (TXrules (_loc, ( (srules _loc t rl "") ))) );
                           styp = ( (STquo (_loc, t)) ); pattern = None } :
                           'symbol) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("NEXT") -> (true)
                       | _ -> (false) ), "UIDENT (\"NEXT\")")) )] ), (
                   (Gram.Action.mk (
                     fun (__camlp4_0 :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (match __camlp4_0 with
                        | UIDENT ("NEXT") ->
                           ({used = [] ; text = ( (TXnext (_loc)) );
                             styp = ( (STself (_loc, "NEXT")) );
                             pattern = None } : 'symbol)
                        | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("SELF") -> (true)
                       | _ -> (false) ), "UIDENT (\"SELF\")")) )] ), (
                   (Gram.Action.mk (
                     fun (__camlp4_0 :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (match __camlp4_0 with
                        | UIDENT ("SELF") ->
                           ({used = [] ; text = ( (TXself (_loc)) );
                             styp = ( (STself (_loc, "SELF")) );
                             pattern = None } : 'symbol)
                        | _ -> assert false) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (pattern : 'pattern Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [( (Gram.Skeyword ("(")) ); Gram.Sself ; (
                    (Gram.Skeyword (",")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (comma_patt : 'comma_patt Gram.Entry.t) ))) ); (
                    (Gram.Skeyword (")")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (p2 :
                        'comma_patt) ->
                       fun _ ->
                        fun (p1 :
                          'pattern) ->
                         fun _ ->
                          fun (_loc :
                            Gram.Loc.t) ->
                           ((Ast.PaTup
                              (_loc, ( (Ast.PaCom (_loc, p1, p2)) ))) :
                             'pattern) )) ));
                  ((
                   [( (Gram.Skeyword ("(")) ); Gram.Sself ; (
                    (Gram.Skeyword (")")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (p :
                        'pattern) ->
                       fun _ -> fun (_loc : Gram.Loc.t) -> (p : 'pattern)
                     )) ));
                  (( [( (Gram.Skeyword ("_")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       ((Ast.PaAny (_loc)) : 'pattern) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | LIDENT (_) -> (true) | _ -> (false)
                       ), "LIDENT _")) )] ), (
                   (Gram.Action.mk (
                     fun (i :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (let i = (Gram.Token.extract_string i) in
                        (Ast.PaId (_loc, ( (Ast.IdLid (_loc, i)) ))) :
                         'pattern) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (comma_patt : 'comma_patt Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Snterm
                      (Gram.Entry.obj ( (pattern : 'pattern Gram.Entry.t)
                        ))) )] ), (
                   (Gram.Action.mk (
                     fun (p :
                       'pattern) ->
                      fun (_loc : Gram.Loc.t) -> (p : 'comma_patt) )) ));
                  ((
                   [Gram.Sself ; ( (Gram.Skeyword (",")) ); Gram.Sself ]
                   ), (
                   (Gram.Action.mk (
                     fun (p2 :
                       'comma_patt) ->
                      fun _ ->
                       fun (p1 :
                         'comma_patt) ->
                        fun (_loc :
                          Gram.Loc.t) ->
                         ((Ast.PaCom (_loc, p1, p2)) : 'comma_patt) )) ))]
                 ))] ))) () ) ))
         );
         (
         (Gram.extend ( (name : 'name Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Snterm
                      (Gram.Entry.obj ( (qualid : 'qualid Gram.Entry.t)
                        ))) )] ), (
                   (Gram.Action.mk (
                     fun (il :
                       'qualid) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       ((mk_name _loc il) : 'name) )) ))] ))] ))) () ) ))
         );
         (
         (Gram.extend ( (string : 'string Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | ANTIQUOT ("", _) -> (true)
                       | _ -> (false) ), "ANTIQUOT (\"\", _)")) )] ), (
                   (Gram.Action.mk (
                     fun (__camlp4_0 :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (match __camlp4_0 with
                        | ANTIQUOT ("", s) ->
                           ((AntiquotSyntax.parse_expr _loc s) : 'string)
                        | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      (( function | STRING (_) -> (true) | _ -> (false)
                       ), "STRING _")) )] ), (
                   (Gram.Action.mk (
                     fun (s :
                       Gram.Token.t) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (let s = (Gram.Token.extract_string s) in
                        (Ast.ExStr (_loc, s)) : 'string) )) ))] ))] )))
             () ) ))
         );
         (Gram.extend ( (semi_sep : 'semi_sep Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [(( [( (Gram.Skeyword (";")) )] ), (
                   (Gram.Action.mk (
                     fun _ -> fun (_loc : Gram.Loc.t) -> (() : 'semi_sep)
                     )) ))] ))] ))) () ) ))

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
                    (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                     (Ast.IdLid (_loc, foldfun)) ))) ))) ), f)) ), e)) in
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
                          (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                           (Ast.IdLid (_loc, "fold")) ))) ))) ), (
                     (Ast.TyAny (_loc)) ))))) ), ( s.styp ))) ), styp)) in
        {used = ( s.used );
         text = ( (TXmeta (_loc, n, ( [( s.text )] ), e, t)) );
         styp = styp; pattern = None }

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
                     (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                      (Ast.IdLid (_loc, foldfun)) ))) ))) ), f)) ), e)) in
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
                           (_loc, ( (Ast.IdUid (_loc, gm)) ), (
                            (Ast.IdLid (_loc, "foldsep")) ))) ))) ), (
                      (Ast.TyAny (_loc)) ))))) ), ( s.styp ))) ), styp)) in
         {used = ( (( s.used ) @ ( sep.used )) );
          text = (
           (TXmeta (_loc, n, ( [( s.text ); ( sep.text )] ), e, t)) );
          styp = styp; pattern = None }

 let _ = let _ = (symbol : 'symbol Gram.Entry.t) in
         let grammar_entry_create = Gram.Entry.mk in
         let simple_expr =
          ((grammar_entry_create "simple_expr") :
            'simple_expr Gram.Entry.t) in
         (
         (Gram.extend ( (symbol : 'symbol Gram.Entry.t) ) (
           ((fun ()
               ->
              (( (Some ((Camlp4.Sig.Grammar.Level ("top")))) ), (
               [(None , None , (
                 [((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("FOLD1") -> (true)
                       | _ -> (false) ), "UIDENT (\"FOLD1\")")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (simple_expr : 'simple_expr Gram.Entry.t) ))) );
                    (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (simple_expr : 'simple_expr Gram.Entry.t) ))) );
                    Gram.Sself ; (
                    (Gram.Stoken
                      ((
                       function | UIDENT ("SEP") -> (true) | _ -> (false)
                       ), "UIDENT (\"SEP\")")) ); Gram.Sself ] ), (
                   (Gram.Action.mk (
                     fun (sep :
                       'symbol) ->
                      fun (__camlp4_1 :
                        Gram.Token.t) ->
                       fun (s :
                         'symbol) ->
                        fun (e :
                          'simple_expr) ->
                         fun (f :
                           'simple_expr) ->
                          fun (__camlp4_0 :
                            Gram.Token.t) ->
                           fun (_loc :
                             Gram.Loc.t) ->
                            (match (__camlp4_1, __camlp4_0) with
                             | (UIDENT ("SEP"), UIDENT ("FOLD1")) ->
                                ((sfoldsep _loc "FOLD1 SEP" "sfold1sep" f
                                   e s sep) : 'symbol)
                             | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("FOLD0") -> (true)
                       | _ -> (false) ), "UIDENT (\"FOLD0\")")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (simple_expr : 'simple_expr Gram.Entry.t) ))) );
                    (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (simple_expr : 'simple_expr Gram.Entry.t) ))) );
                    Gram.Sself ; (
                    (Gram.Stoken
                      ((
                       function | UIDENT ("SEP") -> (true) | _ -> (false)
                       ), "UIDENT (\"SEP\")")) ); Gram.Sself ] ), (
                   (Gram.Action.mk (
                     fun (sep :
                       'symbol) ->
                      fun (__camlp4_1 :
                        Gram.Token.t) ->
                       fun (s :
                         'symbol) ->
                        fun (e :
                          'simple_expr) ->
                         fun (f :
                           'simple_expr) ->
                          fun (__camlp4_0 :
                            Gram.Token.t) ->
                           fun (_loc :
                             Gram.Loc.t) ->
                            (match (__camlp4_1, __camlp4_0) with
                             | (UIDENT ("SEP"), UIDENT ("FOLD0")) ->
                                ((sfoldsep _loc "FOLD0 SEP" "sfold0sep" f
                                   e s sep) : 'symbol)
                             | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("FOLD1") -> (true)
                       | _ -> (false) ), "UIDENT (\"FOLD1\")")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (simple_expr : 'simple_expr Gram.Entry.t) ))) );
                    (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (simple_expr : 'simple_expr Gram.Entry.t) ))) );
                    Gram.Sself ] ), (
                   (Gram.Action.mk (
                     fun (s :
                       'symbol) ->
                      fun (e :
                        'simple_expr) ->
                       fun (f :
                         'simple_expr) ->
                        fun (__camlp4_0 :
                          Gram.Token.t) ->
                         fun (_loc :
                           Gram.Loc.t) ->
                          (match __camlp4_0 with
                           | UIDENT ("FOLD1") ->
                              ((sfold _loc "FOLD1" "sfold1" f e s) :
                                'symbol)
                           | _ -> assert false) )) ));
                  ((
                   [(
                    (Gram.Stoken
                      ((
                       function
                       | UIDENT ("FOLD0") -> (true)
                       | _ -> (false) ), "UIDENT (\"FOLD0\")")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (simple_expr : 'simple_expr Gram.Entry.t) ))) );
                    (
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (simple_expr : 'simple_expr Gram.Entry.t) ))) );
                    Gram.Sself ] ), (
                   (Gram.Action.mk (
                     fun (s :
                       'symbol) ->
                      fun (e :
                        'simple_expr) ->
                       fun (f :
                         'simple_expr) ->
                        fun (__camlp4_0 :
                          Gram.Token.t) ->
                         fun (_loc :
                           Gram.Loc.t) ->
                          (match __camlp4_0 with
                           | UIDENT ("FOLD0") ->
                              ((sfold _loc "FOLD0" "sfold0" f e s) :
                                'symbol)
                           | _ -> assert false) )) ))] ))] ))) () ) ))
         );
         (Gram.extend ( (simple_expr : 'simple_expr Gram.Entry.t) ) (
           ((fun ()
               ->
              (None , (
               [(None , None , (
                 [((
                   [( (Gram.Skeyword ("(")) ); (
                    (Gram.Snterm
                      (Gram.Entry.obj ( (expr : 'expr Gram.Entry.t) )))
                    ); ( (Gram.Skeyword (")")) )] ), (
                   (Gram.Action.mk (
                     fun _ ->
                      fun (e :
                        'expr) ->
                       fun _ ->
                        fun (_loc : Gram.Loc.t) -> (e : 'simple_expr) ))
                   ));
                  ((
                   [(
                    (Gram.Snterm
                      (Gram.Entry.obj (
                        (a_LIDENT : 'a_LIDENT Gram.Entry.t) ))) )] ), (
                   (Gram.Action.mk (
                     fun (i :
                       'a_LIDENT) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       ((Ast.ExId (_loc, ( (Ast.IdLid (_loc, i)) ))) :
                         'simple_expr) )) ))] ))] ))) () ) ))

 let _ = (Camlp4.Options.add "-split_ext" ( (Arg.Set (split_ext)) )
           "Split EXTEND by functions to turn around a PowerPC problem.")

 let _ = (Camlp4.Options.add "-split_gext" ( (Arg.Set (split_ext)) )
           "Old name for the option -split_ext.")

 let _ = (Camlp4.Options.add "-meta_action" ( (Arg.Set (meta_action)) )
           "Undocumented")

end
