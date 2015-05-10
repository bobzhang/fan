let loaded_modules = ref Setf.String.empty
let add_to_loaded_modules =
  function
  | name -> loaded_modules := (Setf.String.add name (!loaded_modules))
let add =
  function
  | name ->
      if not @@ (Setf.String.mem name (!loaded_modules))
      then
        (add_to_loaded_modules name; Dyn_load.load (name ^ Dyn_load.libext))
let () =
  let open Control in
    Gramf.extend_single
      ({
         entry = (item : 'item Gramf.t);
         olevel =
           ({
              label = None;
              lassoc = true;
              productions =
                [{
                   symbols =
                     [Token
                        ({
                           descr =
                             {
                               tag = `Key;
                               word = (A "require");
                               tag_name = "Key"
                             }
                         } : Tokenf.pattern);
                     Token
                       ({
                          descr =
                            { tag = `Str; word = Any; tag_name = "Str" }
                        } : Tokenf.pattern)];
                   annot = "add s\n";
                   fn =
                     (Gramf.mk_action
                        (function
                         | (__fan_1 : Tokenf.txt) ->
                             (function
                              | _ ->
                                  (function
                                   | (_loc : Locf.t) ->
                                       let s = __fan_1.txt in (add s : 'item))) : 
                        Tokenf.txt -> Tokenf.txt -> Locf.t -> 'item))
                 }]
            } : Gramf.olevel)
       } : _ Gramf.single_extend_statement)
