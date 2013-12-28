let loc_of = Ast_gen.loc_of
let seq_sem = Ast_gen.seq_sem
open Astf
let a_lident = Gramf.mk "a_lident"
let nonterminalsclear: exp Gramf.t = Gramf.mk "nonterminalsclear"
let qualuid = Gramf.mk "qualuid"
let _ =
  Gramf.extend_single
    ({
       entry = (a_lident : 'a_lident Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
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
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (Tokenf.mk_ant ~c:"a_lident" s : 'a_lident ) : 
                      Tokenf.ant -> Locf.t -> 'a_lident ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "lid"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "Tokenf.mk_ant ~c:\"a_lident\" s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (Tokenf.mk_ant ~c:"a_lident" s : 'a_lident ) : 
                     Tokenf.ant -> Locf.t -> 'a_lident ))
              };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                      } : Tokenf.pattern )];
                annot = "`Lid (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Lid (_loc, s) : 'a_lident ) : 
                     Tokenf.txt -> Locf.t -> 'a_lident ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (nonterminalsclear : 'nonterminalsclear Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (qualuid : 'qualuid Gramf.t ));
                   List1 (Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t )))];
                 annot =
                   "(ls |>\n   (List.map\n      (fun (x : alident)  ->\n         let x = (x : alident  :>exp) in\n         let _loc = loc_of x in\n         (`App\n            (_loc, (`Dot (_loc, (t :>Astf.vid), (`Lid (_loc, \"clear\")))),\n              (x :>Astf.exp)) :>Astf.exp))))\n  |> seq_sem\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ls : 'a_lident list)  (t : 'qualuid) 
                         (_loc : Locf.t)  ->
                         ((ls |>
                             (List.map
                                (fun (x : alident)  ->
                                   let x = (x : alident  :>exp) in
                                   let _loc = loc_of x in
                                   (`App
                                      (_loc,
                                        (`Dot
                                           (_loc, (t :>Astf.vid),
                                             (`Lid (_loc, "clear")))),
                                        (x :>Astf.exp)) :>Astf.exp))))
                            |> seq_sem : 'nonterminalsclear ) : 'a_lident
                                                                  list ->
                                                                  'qualuid ->
                                                                    Locf.t ->
                                                                    'nonterminalsclear ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (qualuid : 'qualuid Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Dot (_loc, (`Uid (_loc, x)), xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'qualuid)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let x = __fan_0.txt in
                         (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid ) : 
                      'qualuid ->
                        Tokenf.txt -> Tokenf.txt -> Locf.t -> 'qualuid ))
               };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                      } : Tokenf.pattern )];
                annot = "`Uid (_loc, x)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let x = __fan_0.txt in (`Uid (_loc, x) : 'qualuid ) : 
                     Tokenf.txt -> Locf.t -> 'qualuid ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let _ =
  Ast_quotation.of_exp ~name:{ domain = Ns.lang; name = "clear" }
    ~entry:nonterminalsclear ()
