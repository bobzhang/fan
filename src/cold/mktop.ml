let of_exp = Ast_quotation.of_exp
let of_stru_with_filter = Ast_quotation.of_stru_with_filter
let of_stru = Ast_quotation.of_stru
let of_exp_with_filter = Ast_quotation.of_exp_with_filter
let of_clfield_with_filter = Ast_quotation.of_clfield_with_filter
let add = Ast_quotation.add
let loc_of = Ast_gen.loc_of
open! Syntaxf
include Prelude
let domain = Ns.lang
let _ =
  of_stru_with_filter ~name:{ domain; name = "ocaml" } ~entry:strus
    ~filter:(fun s  ->
               let _loc = loc_of s in
               let v = (`Struct (_loc, (s :>Astf.stru)) :>Astf.mexp) in
               let mexp = (Typehook.traversal ())#mexp v in
               let code =
                 match mexp with
                 | (`Struct (_loc,s) : Astf.mexp) -> s
                 | _ -> failwith "can not find items back " in
               if !Typehook.show_code
               then
                 (try Ast2pt.print_stru Format.std_formatter code
                  with
                  | _ ->
                      Util.prerr_endlinef
                        "There is a printer bugOur code generator may still work when Printer is brokenPlz send bug report to %s"
                        Configf.bug_main_address);
               code) ()
let p = Gramf.mk "p"
let _ =
  Gramf.extend_single
    ({
       entry = (p : 'p Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "when"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot =
                   "(`Fun\n   (_loc,\n     (`Bar\n        (_loc,\n          (`CaseWhen\n             (_loc, (p :>Astf.pat), (e :>Astf.exp), (`Bool (_loc, true)))),\n          (`Case (_loc, (`Any _loc), (`Bool (_loc, false))))))) :>Astf.exp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                         ((`Fun
                             (_loc,
                               (`Bar
                                  (_loc,
                                    (`CaseWhen
                                       (_loc, (p :>Astf.pat), (e :>Astf.exp),
                                         (`Bool (_loc, true)))),
                                    (`Case
                                       (_loc, (`Any _loc),
                                         (`Bool (_loc, false))))))) :>
                         Astf.exp) : 'p ) : 'exp ->
                                              Tokenf.txt ->
                                                'pat -> Locf.t -> 'p ))
               };
              {
                symbols = [Nterm (Gramf.obj (pat : 'pat Gramf.t ))];
                annot =
                  "`Fun\n  (_loc,\n    (`Bar\n       (_loc, (`Case (_loc, p, (`Bool (_loc, true)))),\n         (`Case (_loc, (`Any _loc), (`Bool (_loc, false)))))))\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'pat)  (_loc : Locf.t)  ->
                        (`Fun
                           (_loc,
                             (`Bar
                                (_loc,
                                  (`Case (_loc, p, (`Bool (_loc, true)))),
                                  (`Case
                                     (_loc, (`Any _loc),
                                       (`Bool (_loc, false))))))) : 'p ) : 
                     'pat -> Locf.t -> 'p ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let () = of_exp ~name:{ domain; name = "p" } ~entry:p ()
let import = Gramf.mk "import"
let _ =
  let a: 'a Gramf.t = Gramf.mk "a" and n: 'n Gramf.t = Gramf.mk "n" in
  Gramf.extend_single
    ({
       entry = (a : 'a Gramf.t );
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
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   List1 (Nterm (Gramf.obj (n : 'n Gramf.t )));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "Ast_gen.sem_of_list\n  (List.map\n     (fun ((l : Tokenf.txt),r)  ->\n        let xloc = l.loc in\n        let pr = `Lid (xloc, (l.txt)) in\n        let pl =\n          match r with\n          | None  -> pr\n          | Some (y : Tokenf.txt) -> let yloc = y.loc in `Lid (yloc, (y.txt)) in\n        (`Value\n           (_loc, (`Negative _loc),\n             (`Bind\n                (_loc, (pl :>Astf.pat),\n                  (`Dot (_loc, (`Uid (_loc, m)), (pr :>Astf.vid)))))) :>\n          Astf.stru)) ns)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (ns : 'n list)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let m = __fan_0.txt in
                         (Ast_gen.sem_of_list
                            (List.map
                               (fun ((l : Tokenf.txt),r)  ->
                                  let xloc = l.loc in
                                  let pr = `Lid (xloc, (l.txt)) in
                                  let pl =
                                    match r with
                                    | None  -> pr
                                    | Some (y : Tokenf.txt) ->
                                        let yloc = y.loc in
                                        `Lid (yloc, (y.txt)) in
                                  (`Value
                                     (_loc, (`Negative _loc),
                                       (`Bind
                                          (_loc, (pl :>Astf.pat),
                                            (`Dot
                                               (_loc, (`Uid (_loc, m)),
                                                 (pr :>Astf.vid)))))) :>
                                    Astf.stru)) ns) : 'a ) : Tokenf.txt ->
                                                               'n list ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'a ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (n : 'n Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                       } : Tokenf.pattern )];
                 annot = "(x, None)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : Tokenf.txt)  (_loc : Locf.t)  ->
                         ((x, None) : 'n ) : Tokenf.txt -> Locf.t -> 'n ))
               };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "as"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                    Tokenf.pattern )];
                annot = "(x, (Some y))\n";
                fn =
                  (Gramf.mk_action
                     (fun (y : Tokenf.txt)  _  (x : Tokenf.txt) 
                        (_loc : Locf.t)  -> ((x, (Some y)) : 'n ) : Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'n ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (import : 'import Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols = [List1 (Nterm (Gramf.obj (a : 'a Gramf.t )))];
                 annot = "Ast_gen.sem_of_list xs\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'a list)  (_loc : Locf.t)  ->
                         (Ast_gen.sem_of_list xs : 'import ) : 'a list ->
                                                                 Locf.t ->
                                                                   'import ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let domain = Ns.lang
let () =
  of_exp ~name:{ domain; name = "with_exp" } ~entry:with_exp_lang ();
  of_stru ~name:{ domain; name = "with_stru" } ~entry:with_stru_lang ();
  add { domain; name = "str" } Dyn_tag.exp
    (fun _loc  _loc_option  s  -> `Str (_loc, s));
  add { domain; name = "str" } Dyn_tag.stru
    (fun _loc  _loc_option  s  -> `StExp (_loc, (`Str (_loc, s))))
let () = of_stru ~name:{ domain; name = "import" } ~entry:import ()
let () =
  let f (loc : Locf.t) _meta _content =
    let s = Locf.to_string loc in (`Str (loc, s) :>Astf.exp) in
  let f2 (loc : Locf.t) _meta _content =
    let s = Locf.to_string loc in (`StExp (loc, (`Str (loc, s))) :>Astf.stru) in
  Ast_quotation.add { domain; name = "here" } Dyn_tag.exp f;
  Ast_quotation.add { domain; name = "here" } Dyn_tag.stru f2
let () =
  Printexc.register_printer @@
    (function
     | Out_of_memory  -> Some "Out of memory"
     | Assert_failure (file,line,char) ->
         Some
           (Format.sprintf "Assertion failed, file %S, line %d, char %d" file
              line char)
     | Match_failure (file,line,char) ->
         Some
           (Format.sprintf
              "Pattern matching failed, file %S, line %d, char %d" file line
              char)
     | Failure str -> Some (Format.sprintf "Failure: %S" str)
     | Invalid_argument str ->
         Some (Format.sprintf "Invalid argument: %S" str)
     | Sys_error str -> Some (Format.sprintf "I/O error: %S" str)
     | Streamf.NotConsumed  ->
         Some (Format.sprintf "Parse failure(NotConsumed)")
     | Streamf.Error str -> Some (Format.sprintf "Streamf.Error %s" str)
     | _ -> None)
