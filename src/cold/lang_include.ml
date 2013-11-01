let g = Gramf.create_lexer ~annot:"" ~keywords:[] ()
let include_quot = Gramf.mk_dynamic g "include_quot"
let _ =
  Gramf.unsafe_extend_single (include_quot : 'include_quot Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
                "Str")],
            ("let (keep,cf) = let open State in (keep, current_filters) in\nlet fan_keep__0 = !keep and fan_cf__1 = !cf in\ntry\n  let fan_res__2 = State.reset (); Gramf.parse_include_file Syntaxf.strus s in\n  let _ = keep := fan_keep__0; cf := fan_cf__1 in fan_res__2\nwith | fan_e__3 -> ((keep := fan_keep__0; cf := fan_cf__1); raise fan_e__3)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Str ({ txt = s;_} : Tokenf.txt) ->
                        (let (keep,cf) =
                           let open State in (keep, current_filters) in
                         let fan_keep__0 = !keep and fan_cf__1 = !cf in
                         (try
                            let fan_res__2 =
                              State.reset ();
                              Gramf.parse_include_file Syntaxf.strus s in
                            let _ = keep := fan_keep__0; cf := fan_cf__1 in
                            fan_res__2
                          with
                          | fan_e__3 ->
                              ((keep := fan_keep__0; cf := fan_cf__1);
                               raise fan_e__3)) : 'include_quot )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ))
let _ =
  Ast_quotation.of_stru ~name:(Ns.lang, "include") ~entry:include_quot ()
