let g = Gramf.create_lexer ~annot:"" ~keywords:[] ()
let item = Gramf.mk_dynamic g "item"
let dot_namespace = Gramf.mk_dynamic g "dot_namespace"
let items = Gramf.mk_dynamic g "items"
let _ =
  Gramf.unsafe_extend_single (item : 'item Gramf.t )
    (None,
      ((None, None,
         [([Token
              ({
                 descr =
                   { tag = `Key; word = (A "default"); tag_name = "Key" }
               } : Tokenf.pattern );
           Token
             ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
             Tokenf.pattern )],
            ("match Ast_quotation.resolve_name ((`Sub []), s) with\n| None  -> Locf.failf _loc \"DDSL `%s' can not be resolved\" s\n| Some x -> Ast_quotation.set_default x\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let s = __fan_1.txt in
                    (match Ast_quotation.resolve_name ((`Sub []), s) with
                     | None  ->
                         Locf.failf _loc "DDSL `%s' can not be resolved" s
                     | Some x -> Ast_quotation.set_default x : 'item )))));
         ([Token
             ({ descr = { tag = `Key; word = (A "import"); tag_name = "Key" }
              } : Tokenf.pattern );
          Nterm (Gramf.obj (dot_namespace : 'dot_namespace Gramf.t ))],
           ("Ast_quotation.paths := ((`Absolute xs) :: (!Ast_quotation.paths))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(xs : 'dot_namespace)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   (Ast_quotation.paths := ((`Absolute xs) ::
                      (!Ast_quotation.paths)) : 'item )))));
         ([Token
             ({ descr = { tag = `Key; word = (A "filter"); tag_name = "Key" }
              } : Tokenf.pattern );
          Token
            ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
            Tokenf.pattern )],
           ("Ast_filters.use_implem_filter s\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (Ast_filters.use_implem_filter s : 'item )))));
         ([Token
             ({
                descr =
                  { tag = `Key; word = (A "lang_clear"); tag_name = "Key" }
              } : Tokenf.pattern )],
           ("Ast_quotation.clear_map (); Ast_quotation.clear_default ()\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   (Ast_quotation.clear_map ();
                    Ast_quotation.clear_default () : 'item )))))]) : 
      Gramf.olevel ));
  Gramf.unsafe_extend_single (dot_namespace : 'dot_namespace Gramf.t )
    (None,
      ((None, None,
         [([Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern );
           Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           Self],
            ("i :: xs\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'dot_namespace)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (i :: xs : 'dot_namespace )))));
         ([Token
             ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
             Tokenf.pattern )],
           ("[i]\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let i = __fan_0.txt in ([i] : 'dot_namespace )))))]) : 
      Gramf.olevel ));
  Gramf.unsafe_extend_single (items : 'items Gramf.t )
    (None,
      ((None, None,
         [([Nterm (Gramf.obj (item : 'item Gramf.t ));
           Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("()\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (() : 'items )))));
         ([Nterm (Gramf.obj (item : 'item Gramf.t ));
          Token
            ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
            Tokenf.pattern );
          Self],
           ("()\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (() : 'items )))));
         ([],
           ("()\n",
             (Gramf.mk_action (fun (_loc : Locf.t)  -> (() : 'items )))))]) : 
      Gramf.olevel ))
let () =
  Fdir.register
    ((Tokenf.name_of_string "control"),
      (fun loc  _  c  -> Gramf.parse_string ~loc items c))
