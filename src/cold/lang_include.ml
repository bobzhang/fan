let g = Gramf.create_lexer ~annot:"" ~keywords:[] ()
let include_quot = Gramf.mk_dynamic g "include_quot"
let _ =
  Gramf.unsafe_extend_single (include_quot : 'include_quot Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                 Tokenf.pattern )];
            annot =
              "let (keep,cf) = let open State in (keep, current_filters) in\nlet fan_keep__0 = !keep and fan_cf__1 = !cf in\ntry\n  let fan_res__2 = State.reset (); Gramlib.parse_include_file Syntaxf.strus s in\n  let _ = keep := fan_keep__0; cf := fan_cf__1 in fan_res__2\nwith | fan_e__3 -> ((keep := fan_keep__0; cf := fan_cf__1); raise fan_e__3)\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in
                    (let (keep,cf) =
                       let open State in (keep, current_filters) in
                     let fan_keep__0 = !keep and fan_cf__1 = !cf in
                     try
                       let fan_res__2 =
                         State.reset ();
                         Gramlib.parse_include_file Syntaxf.strus s in
                       let _ = keep := fan_keep__0; cf := fan_cf__1 in
                       fan_res__2
                     with
                     | fan_e__3 ->
                         ((keep := fan_keep__0; cf := fan_cf__1);
                          raise fan_e__3) : 'include_quot )))
          }]
     } : Gramf.olevel )
let _ =
  Ast_quotation.of_stru ~name:(Ns.lang, "include") ~entry:include_quot ()
