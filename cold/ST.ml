open LibUtil.Stream
let u = Gram.token_stream_of_string "A.B.C.d"
let v = dup u
let c = Gram.mk "c"
let _ =
  let grammar_entry_create = Gram.mk in
  let d: 'd Gram.t = grammar_entry_create "d" in
  Gram.extend (c : 'c Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (d : 'd Gram.t )); `Skeyword "("],
            (Gram.mk_action
               (fun _  (x : 'd)  (_loc : FanLoc.t)  ->
                  (Ast.IdUid (_loc, x) : 'c ))))])]);
  Gram.extend (d : 'd Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `UID _ -> true | _ -> false)),
                (`Normal, "`UID _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.token])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID x -> ((prerr_endline "d"; x) : 'd )
                  | _ -> assert false)))])])
let p (t,i) =
  Format.eprintf "(%a:@\n%a)@." FanToken.print t
    Grammar.Structure.pp_token_info i
let f = Grammar.Comb.tryp (Gram.parse_origin_tokens c)
let _ = f u