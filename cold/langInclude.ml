let g = Gram.create_lexer ~annot:"include" ~keywords:[] ()

let include_quot = Gram.mk_dynamic g "include_quot"

let _ =
  Gram.extend_single (include_quot : 'include_quot Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) ->\n         (let (keep,cf) = let open FanState in (keep, current_filters) in\n          let fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\n          (try\n             let fan_res__2 =\n               begin\n                 FanState.reset ();\n                 FanBasic.parse_include_file Syntax.strus s\n               end in\n             let _ = begin keep := fan_keep__0; cf := fan_cf__1 end in\n             fan_res__2\n           with\n           | fan_e__3 ->\n               begin\n                 begin keep := fan_keep__0; cf := fan_cf__1 end;\n                 raise fan_e__3\n               end) : 'include_quot )\n     | _ ->\n         failwith\n           \"let (keep,cf) = let open FanState in (keep, current_filters) in\nlet fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\ntry\n  let fan_res__2 =\n    begin FanState.reset (); FanBasic.parse_include_file Syntax.strus s end in\n  let _ = begin keep := fan_keep__0; cf := fan_cf__1 end in fan_res__2\nwith\n| fan_e__3 ->\n    begin begin keep := fan_keep__0; cf := fan_cf__1 end; raise fan_e__3 end\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,s) ->
                       (let (keep,cf) =
                          let open FanState in (keep, current_filters) in
                        let fan_keep__0 = keep.contents
                        and fan_cf__1 = cf.contents in
                        (try
                           let fan_res__2 =
                             begin
                               FanState.reset ();
                               FanBasic.parse_include_file Syntax.strus s
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
                         "let (keep,cf) = let open FanState in (keep, current_filters) in\nlet fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\ntry\n  let fan_res__2 =\n    begin FanState.reset (); FanBasic.parse_include_file Syntax.strus s end in\n  let _ = begin keep := fan_keep__0; cf := fan_cf__1 end in fan_res__2\nwith\n| fan_e__3 ->\n    begin begin keep := fan_keep__0; cf := fan_cf__1 end; raise fan_e__3 end\n"))))]))