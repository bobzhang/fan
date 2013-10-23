let loaded_modules = ref Setf.String.empty
let add_to_loaded_modules name =
  loaded_modules := (Setf.String.add name loaded_modules.contents)
let add name =
  if not @@ (Setf.String.mem name loaded_modules.contents)
  then (add_to_loaded_modules name; Dyn_load.load (name ^ Dyn_load.libext))
let () =
  let open Control in
    Fgram.unsafe_extend_single (item : 'item Fgram.t )
      (None,
        (None, None,
          [([`Skeyword "require";
            `Stoken
              (((function | `Str (_,_) -> true | _ -> false)),
                (4153489, `Any), "`Str s")],
             ("add s\n",
               (Fgram.mk_action
                  (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                     match __fan_1 with
                     | `Str (_,s) -> (add s : 'item )
                     | _ ->
                         failwith
                           (Printf.sprintf "%s"
                              (Ftoken.token_to_string __fan_1))))))]))