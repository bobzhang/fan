let loaded_modules = ref Setf.String.empty
let add_to_loaded_modules name =
  loaded_modules := (Setf.String.add name (!loaded_modules))
let add name =
  if not @@ (Setf.String.mem name (!loaded_modules))
  then (add_to_loaded_modules name; Dyn_load.load (name ^ Dyn_load.libext))
let () =
  let open Control in
    Gramf.unsafe_extend_single (item : 'item Gramf.t )
      ({
         label = None;
         assoc = true;
         productions =
           [{
              symbols =
                [Token
                   ({
                      descr =
                        { tag = `Key; word = (A "require"); tag_name = "Key"
                        }
                    } : Tokenf.pattern );
                Token
                  ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                  Tokenf.pattern )];
              annot = "add s\n";
              fn =
                (Gramf.mk_action
                   (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                      (_loc : Locf.t)  ->
                      let s = __fan_1.txt in (add s : 'item )))
            }]
       } : Gramf.olevel )
