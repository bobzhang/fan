%create{a};;

%extend{a:  [ Lid i as u  %{(u,i)}]}

(* let _ = *)
(*   Gramf.extend_single (a : 'a Gramf.t ) *)
(*     (None, *)
(*       ((None, None, *)
(*          [(([`Token *)
(*               (((function | `Lid _ -> true | _ -> false)), (3802919, `Any), *)
(*                 "`Lid i")]:Gramf.symbol list), *)
(*             ("(u, i)\n", *)
(*               (Gramf.mk_action *)
(*                  (fun ~__fan_0:(u : Tokenf.t)  (_loc : Locf.t)  -> *)
(*                     ((u, i) : 'a )))))]) : Gramf.olevel )) *)
  

(* local variables: *)
(* compile-command: "ocamlc -I ../../common   -I ../../cold -pp ../../fan -c a.ml" *)
(* end: *)
