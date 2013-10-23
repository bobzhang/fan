let loc_of = Ast_gen.loc_of
let seq_sem = Ast_gen.seq_sem
open FAst
let a_lident = Gramf.mk "a_lident"
let nonterminalsclear: exp Gramf.t = Gramf.mk "nonterminalsclear"
let qualuid = Gramf.mk "qualuid"
let _ =
  Gramf.extend_single (a_lident : 'a_lident Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("FanUtil.mk_anti _loc ~c:\"a_lident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (FanUtil.mk_anti _loc ~c:"a_lident" n s : 'a_lident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("lid",_) -> true | _ -> false)),
              (3257031, (`A "lid")), "`Ant s")],
          ("FanUtil.mk_anti _loc ~c:\"a_lident\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("lid" as n),s) ->
                      (FanUtil.mk_anti _loc ~c:"a_lident" n s : 'a_lident )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid s")],
          ("`Lid (_loc, s)\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Lid ({ txt = s;_} : Tokenf.txt) ->
                      (`Lid (_loc, s) : 'a_lident )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))))]));
  Gramf.extend_single (nonterminalsclear : 'nonterminalsclear Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (qualuid : 'qualuid Gramf.t ));
          `Slist1 (`Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t )))],
           ("(ls |>\n   (List.map\n      (fun (x : alident)  ->\n         let x = (x : alident  :>exp) in\n         let _loc = loc_of x in\n         (`App (_loc, (`Dot (_loc, t, (`Lid (_loc, \"clear\")))), x) : \n           FAst.exp ))))\n  |> seq_sem\n",
             (Gramf.mk_action
                (fun (ls : 'a_lident list)  (t : 'qualuid)  (_loc : Locf.t) 
                   ->
                   ((ls |>
                       (List.map
                          (fun (x : alident)  ->
                             let x = (x : alident  :>exp) in
                             let _loc = loc_of x in
                             (`App
                                (_loc,
                                  (`Dot (_loc, t, (`Lid (_loc, "clear")))),
                                  x) : FAst.exp ))))
                      |> seq_sem : 'nonterminalsclear )))))]));
  Gramf.extend_single (qualuid : 'qualuid Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid x");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
             (Gramf.mk_action
                (fun (xs : 'qualuid)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = x;_} : Tokenf.txt) ->
                       (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
              "`Uid x")],
          ("`Uid (_loc, x)\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Uid ({ txt = x;_} : Tokenf.txt) ->
                      (`Uid (_loc, x) : 'qualuid )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))))]))
let _ =
  let d = Ns.lang in
  Ast_quotation.of_exp ~name:(d, "clear") ~entry:nonterminalsclear ()