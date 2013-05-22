open LibUtil

exception Error of string * string
(* we should delay the initialization to make [fan] available to
   more platforms *)    
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
          raise (Error "Fan's dynamic loader initialization" (Dynlink.error_message e))
    else ();
    let try fname =  Filename.find_in_path
        ~path:("." :: FanConfig.fan_standard_library :: !FanConfig.dynload_dirs) file in
    try Dynlink.loadfile fname
    with Dynlink.Error e -> raise (Error fname (Dynlink.error_message e))
    with Not_found -> raise (Error file "file not found in path") 
  end


