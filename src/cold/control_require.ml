let loaded_modules = ref Setf.String.empty
let add_to_loaded_modules name =
  loaded_modules := (Setf.String.add name loaded_modules.contents)
let add name =
  if not @@ (Setf.String.mem name loaded_modules.contents)
  then (add_to_loaded_modules name; Dyn_load.load (name ^ Dyn_load.libext))
let () =
  let open Control in
    Gramf.unsafe_extend_single (item : 'item Gramf.t )
      (None,
        (None, None,
          [([`Skeyword "require";
            `Stoken
              (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
                "`Str s")],
             ("add s\n",
               (Gramf.mk_action
                  (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                     match __fan_1 with
                     | `Str ({ txt = s;_} : Tokenf.txt) -> (add s : 'item )
                     | _ ->
                         failwith
                           (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))))]))