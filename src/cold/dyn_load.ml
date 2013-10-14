exception Error of string*string
let _initialized = ref false
let load file =
  if not _initialized.contents
  then
    (try
       Dynlink.init ();
       Dynlink.allow_unsafe_modules true;
       _initialized := true
     with
     | Dynlink.Error e ->
         raise
           (Error
              ("Fan's dynamic loader initialization",
                (Dynlink.error_message e))));
  (match Filenamef.find_in_path ~path:("." :: Configf.fan_plugins_library ::
           (Configf.dynload_dirs.contents)) file
   with
   | None  -> raise (Error (file, "file not found in path"))
   | Some fname ->
       (try Dynlink.loadfile fname
        with
        | Dynlink.Error e -> raise (Error (fname, (Dynlink.error_message e)))))
let (objext,libext) =
  if Dynlink.is_native then (".cmxs", ".cmxs") else (".cmo", ".cma")