let g = Fgram.create_lexer ~annot:"" ~keywords:[] ()
let include_quot = Fgram.mk_dynamic g "include_quot"
let _ =
  Fgram.unsafe_extend_single (include_quot : 'include_quot Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Str _ -> true | _ -> false)),
               (`App ((`Vrn "Str"), `Any)), "`Str _")],
           ("let (keep,cf) = let open FState in (keep, current_filters) in\nlet fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\ntry\n  let fan_res__2 = FState.reset (); Fgram.parse_include_file Fsyntax.strus s in\n  let _ = keep := fan_keep__0; cf := fan_cf__1 in fan_res__2\nwith | fan_e__3 -> ((keep := fan_keep__0; cf := fan_cf__1); raise fan_e__3)\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> Ftoken.t])  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str s ->
                       (let (keep,cf) =
                          let open FState in (keep, current_filters) in
                        let fan_keep__0 = keep.contents
                        and fan_cf__1 = cf.contents in
                        (try
                           let fan_res__2 =
                             FState.reset ();
                             Fgram.parse_include_file Fsyntax.strus s in
                           let _ = keep := fan_keep__0; cf := fan_cf__1 in
                           fan_res__2
                         with
                         | fan_e__3 ->
                             ((keep := fan_keep__0; cf := fan_cf__1);
                              raise fan_e__3)) : 'include_quot )
                   | _ ->
                       failwith
                         "let (keep,cf) = let open FState in (keep, current_filters) in\nlet fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\ntry\n  let fan_res__2 = FState.reset (); Fgram.parse_include_file Fsyntax.strus s in\n  let _ = keep := fan_keep__0; cf := fan_cf__1 in fan_res__2\nwith | fan_e__3 -> ((keep := fan_keep__0; cf := fan_cf__1); raise fan_e__3)\n"))))]))
