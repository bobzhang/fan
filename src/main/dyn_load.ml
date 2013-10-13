

exception Error of string * string


(* we should delay the initialization to make [fan] available to
   more platforms. Since in some cases, this function will not be triggered.
 *)    
let _initialized = ref false 


let load file =
  begin
    if not !_initialized then
      (try
        begin
          Dynlink.init ();
          Dynlink.allow_unsafe_modules true;
          _initialized := true
        end
      with
        Dynlink.Error e ->
          raise 
            (Error ("Fan's dynamic loader initialization",
                    Dynlink.error_message e)));
    match Filenamef.find_in_path
        ~path: ("." :: FConfig.fan_plugins_library :: !FConfig.dynload_dirs) file
    with
    | None -> raise (Error (file, "file not found in path"))            
    | Some fname ->
        try Dynlink.loadfile fname
        with Dynlink.Error e -> raise (Error (fname, Dynlink.error_message e))
end

(** no repeat loading
    FIXME? it can only load [cma] and [cmxs] files? *)
let (objext,libext) =
  if Dynlink.is_native then
    (".cmxs",".cmxs")
  else (".cmo",".cma")


(* local variables: *)
(* compile-command: "cd ../main_annot && pmake dyn_load.cmo " *)
(* end: *)
