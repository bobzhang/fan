let hash_p = Gramf.mk "hash_p"
let _ =
  Gramf.extend_single
    ({
       entry = (hash_p : 'hash_p Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [List1sep
                      ((Token
                          ({
                             descr =
                               { tag = `Str; word = Any; tag_name = "Str" }
                           } : Tokenf.pattern )),
                        (Token
                           ({
                              descr =
                                {
                                  tag = `Key;
                                  word = (A "|");
                                  tag_name = "Key"
                                }
                            } : Tokenf.pattern )))];
                 annot =
                   "let p =\n  Ast_gen.bar_of_list\n    (List.map\n       (fun (x : Tokenf.txt)  ->\n          let v = x.txt in\n          let i = Hashtbl.hash v in\n          (`Case\n             (_loc, (`Int (_loc, (string_of_int i))),\n               (`App\n                  (_loc,\n                    (`App (_loc, (`Lid (_loc, \"=\")), (`Lid (_loc, \"s\")))),\n                    (`Str (_loc, v))))) :>Astf.case)) xs) in\n(`Fun\n   (_loc,\n     (`Case\n        (_loc,\n          (`Constraint (_loc, (`Lid (_loc, \"s\")), (`Lid (_loc, \"string\")))),\n          (`Fun\n             (_loc,\n               (`Bar\n                  (_loc, p,\n                    (`Case (_loc, (`Any _loc), (`Bool (_loc, false))))))))))) :>\n  Astf.exp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : Tokenf.txt list)  (_loc : Locf.t)  ->
                         (let p =
                            Ast_gen.bar_of_list
                              (List.map
                                 (fun (x : Tokenf.txt)  ->
                                    let v = x.txt in
                                    let i = Hashtbl.hash v in
                                    (`Case
                                       (_loc,
                                         (`Int (_loc, (string_of_int i))),
                                         (`App
                                            (_loc,
                                              (`App
                                                 (_loc, (`Lid (_loc, "=")),
                                                   (`Lid (_loc, "s")))),
                                              (`Str (_loc, v))))) :>Astf.case))
                                 xs) in
                          (`Fun
                             (_loc,
                               (`Case
                                  (_loc,
                                    (`Constraint
                                       (_loc, (`Lid (_loc, "s")),
                                         (`Lid (_loc, "string")))),
                                    (`Fun
                                       (_loc,
                                         (`Bar
                                            (_loc, p,
                                              (`Case
                                                 (_loc, (`Any _loc),
                                                   (`Bool (_loc, false))))))))))) :>
                            Astf.exp) : 'hash_p ) : Tokenf.txt list ->
                                                      Locf.t -> 'hash_p ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Ast_quotation.of_exp ~name:{ domain = Ns.lang; name = "hash_cmp" }
    ~entry:hash_p ()
