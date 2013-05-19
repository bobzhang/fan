let g = Gram.create_lexer ~annot:"include" ~keywords:[] ()

let include_quot = Gram.mk_dynamic g "include_quot"

let _ =
  Gram.extend_single (include_quot : 'include_quot Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) ->\n         (let keep = FanState.keep in\n          let cf = FanState.current_filters in\n          let fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\n          (try\n             let fan_res__2 =\n               FanState.reset ();\n               FanBasic.parse_include_file PreCast.Syntax.strus s in\n             let _ = keep := fan_keep__0; cf := fan_cf__1 in fan_res__2\n           with\n           | fan_e__3 ->\n               ((keep := fan_keep__0; cf := fan_cf__1); raise fan_e__3)) : \n         'include_quot )\n     | _ ->\n         failwith\n           \"let keep = FanState.keep in\nlet cf = FanState.current_filters in\nlet fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\ntry\n  let fan_res__2 =\n    FanState.reset (); FanBasic.parse_include_file PreCast.Syntax.strus s in\n  let _ = keep := fan_keep__0; cf := fan_cf__1 in fan_res__2\nwith | fan_e__3 -> ((keep := fan_keep__0; cf := fan_cf__1); raise fan_e__3)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,s) ->
                       (let keep = FanState.keep in
                        let cf = FanState.current_filters in
                        let fan_keep__0 = keep.contents
                        and fan_cf__1 = cf.contents in
                        (try
                           let fan_res__2 =
                             FanState.reset ();
                             FanBasic.parse_include_file PreCast.Syntax.strus
                               s in
                           let _ = keep := fan_keep__0; cf := fan_cf__1 in
                           fan_res__2
                         with
                         | fan_e__3 ->
                             ((keep := fan_keep__0; cf := fan_cf__1);
                              raise fan_e__3)) : 'include_quot )
                   | _ ->
                       failwith
                         "let keep = FanState.keep in\nlet cf = FanState.current_filters in\nlet fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\ntry\n  let fan_res__2 =\n    FanState.reset (); FanBasic.parse_include_file PreCast.Syntax.strus s in\n  let _ = keep := fan_keep__0; cf := fan_cf__1 in fan_res__2\nwith | fan_e__3 -> ((keep := fan_keep__0; cf := fan_cf__1); raise fan_e__3)\n"))))]))