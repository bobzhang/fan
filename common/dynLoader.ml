open LibUtil

exception Error of string * string


(* we should delay the initialization to make [fan] available to
   more platforms. Since in some cases, this function will not be triggered
 *)    
let _initialized = ref false 


let load file =
  begin
    if not !_initialized then
      try
        (Dynlink.init ();
         Dynlink.allow_unsafe_modules true;
         _initialized := true)
      with
        Dynlink.Error e ->
          raise
            (Error("Fan's dynamic loader initialization",
                   (Dynlink.error_message e)))
    else ();
    (try
      let fname =
        Filename.find_in_path ~path:("." :: FConfig.fan_plugins_library ::
                                     (FConfig.dynload_dirs.contents)) file
      in
      fun ()  ->
       try Dynlink.loadfile fname
       with
       | Dynlink.Error e -> raise (Error (fname, (Dynlink.error_message e)))
    with
    | Not_found  ->
        (fun ()  -> raise (Error (file, "file not found in path")))) ()
  end


