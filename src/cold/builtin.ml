let mtypes: Sigs_util.mtyps =
  let tys = Ast_basic.N.list_of_sem Astf.code_of_ast [] in
  let f = function | `TyDcl (`Lid s,_,_,_) as y -> (s, y) | _ -> assert false in
  List.map
    (fun (t : Astfn.stru)  ->
       match t with
       | `Type (`And _ as c) ->
           let ms = Ast_basic.N.list_of_and c [] in
           Sigs_util.Mutual (List.map f ms)
       | `Type x -> Sigs_util.Single (f x)
       | _ -> assert false) tys
module Print = Derive_stru.Make(struct let p = Gen_print.default end)
let buitin_dispatch = [("print", (Option.get (Print.stru_of_mtyps mtypes)))]
let builtin = Gramf.mk "builtin"
let _ =
  Gramf.extend_single
    ({
       entry = (builtin : 'builtin Gramf.t );
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
                               { tag = `Lid; word = Any; tag_name = "Lid" }
                           } : Tokenf.pattern )),
                        (Token
                           ({
                              descr =
                                {
                                  tag = `Key;
                                  word = (A ";");
                                  tag_name = "Key"
                                }
                            } : Tokenf.pattern )))];
                 annot =
                   "Ast_gen.sem_of_list\n  (List.map\n     (fun (x : Tokenf.txt)  ->\n        Fill.stru x.loc (List.assoc x.txt buitin_dispatch)) xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : Tokenf.txt list)  (_loc : Locf.t)  ->
                         (Ast_gen.sem_of_list
                            (List.map
                               (fun (x : Tokenf.txt)  ->
                                  Fill.stru x.loc
                                    (List.assoc x.txt buitin_dispatch)) xs) : 
                         'builtin ) : Tokenf.txt list -> Locf.t -> 'builtin ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let _ =
  Ast_quotation.of_stru ~lexer:Lex_fan.from_stream
    ~name:{ domain = Ns.lang; name = "builtin" } ~entry:builtin ()
