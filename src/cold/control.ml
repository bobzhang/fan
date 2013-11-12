let g = Gramf.create_lexer ~annot:"" ~keywords:[] ()
let item = Gramf.mk_dynamic g "item"
let dot_namespace = Gramf.mk_dynamic g "dot_namespace"
let items = Gramf.mk_dynamic g "items"
let _ =
  Gramf.unsafe_extend_single (item : 'item Gramf.t )
    (None,
      ((None, None,
         [{
            symbols =
              [Token
                 ({
                    descr =
                      { tag = `Key; word = (A "default"); tag_name = "Key" }
                  } : Tokenf.pattern );
              Token
                ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                Tokenf.pattern )];
            annot =
              "match Ast_quotation.resolve_name ((`Sub []), s) with\n| None  -> Locf.failf _loc \"DDSL `%s' can not be resolved\" s\n| Some x -> Ast_quotation.set_default x\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let s = __fan_1.txt in
                    (match Ast_quotation.resolve_name ((`Sub []), s) with
                     | None  ->
                         Locf.failf _loc "DDSL `%s' can not be resolved" s
                     | Some x -> Ast_quotation.set_default x : 'item )))
          };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "import"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Nterm (Gramf.obj (dot_namespace : 'dot_namespace Gramf.t ))];
           annot =
             "Ast_quotation.paths := ((`Absolute xs) :: (!Ast_quotation.paths))\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:(xs : 'dot_namespace)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   (Ast_quotation.paths := ((`Absolute xs) ::
                      (!Ast_quotation.paths)) : 'item )))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "filter"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot = "Ast_filters.use_implem_filter s\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (Ast_filters.use_implem_filter s : 'item )))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "lang_clear"); tag_name = "Key"
                     }
                 } : Tokenf.pattern )];
           annot =
             "Ast_quotation.clear_map (); Ast_quotation.clear_default ()\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   (Ast_quotation.clear_map ();
                    Ast_quotation.clear_default () : 'item )))
         }]) : Gramf.olevel ));
  Gramf.unsafe_extend_single (dot_namespace : 'dot_namespace Gramf.t )
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
            annot = "i :: xs\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'dot_namespace)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (i :: xs : 'dot_namespace )))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
                Tokenf.pattern )];
           annot = "[i]\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let i = __fan_0.txt in ([i] : 'dot_namespace )))
         }]) : Gramf.olevel ));
  Gramf.unsafe_extend_single (items : 'items Gramf.t )
    (None,
      ((None, None,
         [{
            symbols =
              [Nterm (Gramf.obj (item : 'item Gramf.t ));
              Token
                ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
                Tokenf.pattern )];
            annot = "()\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (() : 'items )))
          };
         {
           symbols =
             [Nterm (Gramf.obj (item : 'item Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Self];
           annot = "()\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (() : 'items )))
         };
         {
           symbols = [];
           annot = "()\n";
           fn = (Gramf.mk_action (fun (_loc : Locf.t)  -> (() : 'items )))
         }]) : Gramf.olevel ))
let () =
  Fdir.register
    ((Tokenf.name_of_string "control"),
      (fun loc  _  c  -> Gramlib.parse_string ~loc items c))
