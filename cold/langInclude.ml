let g = Fgram.create_lexer ~annot:"" ~keywords:[] ()

let include_quot = Fgram.mk_dynamic g "include_quot"

let _ =
  Fgram.unsafe_extend_single (include_quot : 'include_quot Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) ->\n         (let (keep,cf) = let open FState in (keep, current_filters) in\n          let fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\n          (try\n             let fan_res__2 =\n               begin\n                 FState.reset (); Fgram.parse_include_file Fsyntax.strus s\n               end in\n             let _ = begin keep := fan_keep__0; cf := fan_cf__1 end in\n             fan_res__2\n           with\n           | fan_e__3 ->\n               begin\n                 begin keep := fan_keep__0; cf := fan_cf__1 end;\n                 raise fan_e__3\n               end) : 'include_quot )\n     | _ ->\n         failwith\n           \"let (keep,cf) = let open FState in (keep, current_filters) in\nlet fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\ntry\n  let fan_res__2 =\n    begin FState.reset (); Fgram.parse_include_file Fsyntax.strus s end in\n  let _ = begin keep := fan_keep__0; cf := fan_cf__1 end in fan_res__2\nwith\n| fan_e__3 ->\n    begin begin keep := fan_keep__0; cf := fan_cf__1 end; raise fan_e__3 end\n\")\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,s) ->
                       (let (keep,cf) =
                          let open FState in (keep, current_filters) in
                        let fan_keep__0 = keep.contents
                        and fan_cf__1 = cf.contents in
                        (try
                           let fan_res__2 =
                             begin
                               FState.reset ();
                               Fgram.parse_include_file Fsyntax.strus s
                             end in
                           let _ =
                             begin keep := fan_keep__0; cf := fan_cf__1 end in
                           fan_res__2
                         with
                         | fan_e__3 ->
                             begin
                               begin keep := fan_keep__0; cf := fan_cf__1 end;
                               raise fan_e__3
                             end) : 'include_quot )
                   | _ ->
                       failwith
                         "let (keep,cf) = let open FState in (keep, current_filters) in\nlet fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\ntry\n  let fan_res__2 =\n    begin FState.reset (); Fgram.parse_include_file Fsyntax.strus s end in\n  let _ = begin keep := fan_keep__0; cf := fan_cf__1 end in fan_res__2\nwith\n| fan_e__3 ->\n    begin begin keep := fan_keep__0; cf := fan_cf__1 end; raise fan_e__3 end\n"))))]))