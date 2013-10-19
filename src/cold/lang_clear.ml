let loc_of = Ast_gen.loc_of
let seq_sem = Ast_gen.seq_sem
open FAst
let a_lident = Fgram.mk "a_lident"
let nonterminalsclear: exp Fgram.t = Fgram.mk "nonterminalsclear"
let qualuid = Fgram.mk "qualuid"
let _ =
  Fgram.extend_single (a_lident : 'a_lident Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (`App ((`App ((`Vrn "Ant"), (`Str ""))), `Any)),
               "`Ant (\"\",_)")],
           ("FanUtil.mk_anti _loc ~c:\"a_lident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> Ftoken.t])  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (FanUtil.mk_anti _loc ~c:"a_lident" n s : 'a_lident )
                   | _ ->
                       failwith "FanUtil.mk_anti _loc ~c:\"a_lident\" n s\n"))));
        ([`Stoken
            (((function | `Ant ("lid",_) -> true | _ -> false)),
              (`App ((`App ((`Vrn "Ant"), (`Str "lid"))), `Any)),
              "`Ant (\"lid\",_)")],
          ("FanUtil.mk_anti _loc ~c:\"a_lident\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> Ftoken.t])  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("lid" as n),s) ->
                      (FanUtil.mk_anti _loc ~c:"a_lident" n s : 'a_lident )
                  | _ ->
                      failwith "FanUtil.mk_anti _loc ~c:\"a_lident\" n s\n"))));
        ([`Stoken
            (((function | `Lid _ -> true | _ -> false)),
              (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("`Lid (_loc, s)\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> Ftoken.t])  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Lid s -> (`Lid (_loc, s) : 'a_lident )
                  | _ -> failwith "`Lid (_loc, s)\n"))))]));
  Fgram.extend_single (nonterminalsclear : 'nonterminalsclear Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (qualuid : 'qualuid Fgram.t ));
          `Slist1 (`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t )))],
           ("(ls |>\n   (List.map\n      (fun (x : alident)  ->\n         let x = (x : alident  :>exp) in\n         let _loc = loc_of x in\n         (`App (_loc, (`Dot (_loc, t, (`Lid (_loc, \"clear\")))), x) : \n           FAst.exp ))))\n  |> seq_sem\n",
             (Fgram.mk_action
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
  Fgram.extend_single (qualuid : 'qualuid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`App ((`Vrn "Uid"), `Any)), "`Uid _");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
             (Fgram.mk_action
                (fun (xs : 'qualuid)  _  (__fan_0 : [> Ftoken.t]) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid x ->
                       (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, x)), xs)\n"))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)),
              (`App ((`Vrn "Uid"), `Any)), "`Uid _")],
          ("`Uid (_loc, x)\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> Ftoken.t])  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Uid x -> (`Uid (_loc, x) : 'qualuid )
                  | _ -> failwith "`Uid (_loc, x)\n"))))]))
let _ =
  let d = Ns.lang in
  Ast_quotation.of_exp ~name:(d, "clear") ~entry:nonterminalsclear ()