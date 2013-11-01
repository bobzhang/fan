let loc_of = Ast_gen.loc_of
let seq_sem = Ast_gen.seq_sem
open FAst
let a_lident = Gramf.mk "a_lident"
let nonterminalsclear: exp Gramf.t = Gramf.mk "nonterminalsclear"
let qualuid = Gramf.mk "qualuid"
let _ =
  Gramf.extend_single (a_lident : 'a_lident Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("Tokenf.mk_ant ~c:\"a_lident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | (({ kind = "";_} as s) : Tokenf.ant) ->
                        (Tokenf.mk_ant ~c:"a_lident" s : 'a_lident )
                    | _ -> assert false))));
         ([`Token
             (((function
                | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "lid")), "`Ant s")],
           ("Tokenf.mk_ant ~c:\"a_lident\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | (({ kind = "lid";_} as s) : Tokenf.ant) ->
                       (Tokenf.mk_ant ~c:"a_lident" s : 'a_lident )
                   | _ -> assert false))));
         ([`Token
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "Lid")],
           ("`Lid (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = s;_} : Tokenf.txt) ->
                       (`Lid (_loc, s) : 'a_lident )))))]) : Gramf.olevel ));
  Gramf.extend_single (nonterminalsclear : 'nonterminalsclear Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (qualuid : 'qualuid Gramf.t ));
           `List1 (`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t )))],
            ("(ls |>\n   (List.map\n      (fun (x : alident)  ->\n         let x = (x : alident  :>exp) in\n         let _loc = loc_of x in\n         (`App (_loc, (`Dot (_loc, t, (`Lid (_loc, \"clear\")))), x) : \n           FAst.exp ))))\n  |> seq_sem\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(ls : 'a_lident list)  ~__fan_0:(t : 'qualuid)
                     (_loc : Locf.t)  ->
                    ((ls |>
                        (List.map
                           (fun (x : alident)  ->
                              let x = (x : alident  :>exp) in
                              let _loc = loc_of x in
                              (`App
                                 (_loc,
                                   (`Dot (_loc, t, (`Lid (_loc, "clear")))),
                                   x) : FAst.exp ))))
                       |> seq_sem : 'nonterminalsclear )))))]) : Gramf.olevel ));
  Gramf.extend_single (qualuid : 'qualuid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
                "Uid");
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'qualuid)  ~__fan_1:_  ~__fan_0 
                    (_loc : Locf.t)  ->
                    match __fan_0 with
                    | ({ txt = x;_} : Tokenf.txt) ->
                        (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "Uid")],
           ("`Uid (_loc, x)\n",
             (Gramf.mk_action
                (fun ~__fan_0  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = x;_} : Tokenf.txt) ->
                       (`Uid (_loc, x) : 'qualuid )))))]) : Gramf.olevel ))
let _ =
  let d = Ns.lang in
  Ast_quotation.of_exp ~name:(d, "clear") ~entry:nonterminalsclear ()
