%create{a};;

%extend{a:  [ Lid i as u  %{(u,i)}]}

%extend{a: [?a as v;?Lid as k %{(v,c,k)} ]}


%extend{a: [?a as v;?Lid as k %{v,c,k} ]}
(* FIXME should have a parser error here ... *)  
(*
let _ =
  Gramf.extend_single (a : 'a Gramf.t )
    (None,
      ((None, None,
         [(([`Token
              (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
                "`Lid i")]:Gramf.symbol list),
            ("(u, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(u : Tokenf.t)  (_loc : Locf.t)  ->
                    ((u, i) : 'a )))))]) : Gramf.olevel ))
*)  
(*
let _ =
  Gramf.extend_single (a : 'a Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
                "`Lid i")],
            ("(u, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(u : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | u -> ((u, i) : 'a )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ))
*)
(* local variables: *)
(* compile-command: "ocamlc -I ../../common   -I ../../cold -pp ../../fan -c a.ml" *)
(* end: *)
