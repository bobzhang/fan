open LibUtil

exception Error of string*string

let _initialized = ref false

let load file =
  begin
    if not _initialized.contents
    then
      (try
         begin
           Dynlink.init (); Dynlink.allow_unsafe_modules true;
           _initialized := true
         end
       with
       | Dynlink.Error e ->
           raise
             (Error
                ("Fan's dynamic loader initialization",
                  (Dynlink.error_message e))))
    else ();
    ((try
        let fname =
          Filename.find_in_path ~path:("." :: FanConfig.fan_standard_library
            :: (FanConfig.dynload_dirs.contents)) file in
        fun ()  ->
          try Dynlink.loadfile fname
          with
          | Dynlink.Error e ->
              raise (Error (fname, (Dynlink.error_message e)))
      with
      | Not_found  ->
          (fun ()  -> raise (Error (file, "file not found in path"))))) ()
  end