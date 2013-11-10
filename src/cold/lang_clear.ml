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
         [{
            symbols =
              [Token
                 ({
                    descr =
                      { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                  } : Tokenf.pattern )];
            annot = "Tokenf.mk_ant ~c:\"a_lident\" s\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (Tokenf.mk_ant ~c:"a_lident" s : 'a_lident )))
          };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Ant; word = (Kind "lid"); tag_name = "Ant" }
                 } : Tokenf.pattern )];
           annot = "Tokenf.mk_ant ~c:\"a_lident\" s\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (Tokenf.mk_ant ~c:"a_lident" s : 'a_lident )))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                Tokenf.pattern )];
           annot = "`Lid (_loc, s)\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Lid (_loc, s) : 'a_lident )))
         }]) : Gramf.olevel ));
  Gramf.extend_single (nonterminalsclear : 'nonterminalsclear Gramf.t )
    (None,
      ((None, None,
         [{
            symbols =
              [Nterm (Gramf.obj (qualuid : 'qualuid Gramf.t ));
              List1 (Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t )))];
            annot =
              "(ls |>\n   (List.map\n      (fun (x : alident)  ->\n         let x = (x : alident  :>exp) in\n         let _loc = loc_of x in\n         (`App (_loc, (`Dot (_loc, t, (`Lid (_loc, \"clear\")))), x) : \n           FAst.exp ))))\n  |> seq_sem\n";
            fn =
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
                       |> seq_sem : 'nonterminalsclear )))
          }]) : Gramf.olevel ));
  Gramf.extend_single (qualuid : 'qualuid Gramf.t )
    (None,
      ((None, None,
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
                 Tokenf.pattern );
              Token
                ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
                Tokenf.pattern );
              Self];
            annot = "`Dot (_loc, (`Uid (_loc, x)), xs)\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'qualuid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
                Tokenf.pattern )];
           annot = "`Uid (_loc, x)\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let x = __fan_0.txt in (`Uid (_loc, x) : 'qualuid )))
         }]) : Gramf.olevel ))
let _ =
  let d = Ns.lang in
  Ast_quotation.of_exp ~name:(d, "clear") ~entry:nonterminalsclear ()
