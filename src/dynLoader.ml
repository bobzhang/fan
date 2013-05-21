

  
module Make (U:sig end)= struct 

  type t = string Queue.t 

  let instance =  ref (fun () -> failwith "empty in dynloader")

  exception Error of string * string

  let include_dir x y = Queue.add y x

  let fold_load_path x f acc = Queue.fold (fun x y -> f y x) acc x

  let mk ?(ocaml_stdlib = true)  () =
    let q = Queue.create () in begin
      if ocaml_stdlib then include_dir q FanConfig.ocaml_standard_library else ();
      include_dir q ".";
      q
    end


  (* Load files in core *)
  let find_in_path x name =
    if not (Filename.is_implicit name) then
      if Sys.file_exists name then name else raise Not_found
    else
      let res =
        fold_load_path x
          (fun dir ->
            function
            | None ->
                let fullname = Filename.concat dir name in
                if Sys.file_exists fullname then Some fullname else None
            | x -> x ) None in
      match res with
      |None -> raise Not_found
      | Some x -> x 

  let load =
    let _initialized = ref false in
    fun _path file ->
      begin
        if not !_initialized then
          try
            (Dynlink.init ();
             Dynlink.allow_unsafe_modules true;
             _initialized := true)
          with
            Dynlink.Error e ->
              raise (Error "Fan's dynamic loader initialization" (Dynlink.error_message e));
      let fname =
        try find_in_path _path file with
         Not_found -> raise (Error file "file not found in path") in
      try Dynlink.loadfile fname
      with Dynlink.Error e -> raise (Error fname (Dynlink.error_message e)) 
    end


  
end
