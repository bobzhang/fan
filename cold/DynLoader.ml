module type S =
  sig type t   exception Error of string *string 
  val mk : ?ocaml_stdlib:bool  -> ?camlp4_stdlib:bool  -> unit  -> t 
  val fold_load_path : t  -> (string  -> 'a -> 'a) -> 'a -> 'a
  val load : t  -> string  -> unit  val include_dir : t  -> string  -> unit 
  val find_in_path : t  -> string  -> string  val is_native : bool 
  val instance : (unit  -> t ) ref  end
module Make(U:sig  end) : S = struct
  type t = string  Queue.t  
  let instance = (ref ( (fun (() ) -> (failwith "empty in dynloader")) ))
  exception Error of string *string 
  let include_dir (x) (y) = (Queue.add y x)
  let fold_load_path (x) (f) (acc) =
    (Queue.fold ( (fun (x) -> (fun (y) -> (f y x))) ) acc x)
  let mk ?(ocaml_stdlib=true)  ?(camlp4_stdlib=true)  (() ) =
    let q = (Queue.create () ) in
    begin
      if ocaml_stdlib then begin
        (include_dir q FanConfig.ocaml_standard_library)
      end else begin
        ()
      end;
      if camlp4_stdlib then begin
        begin
        (include_dir q FanConfig.camlp4_standard_library);
        (include_dir q (
          (Filename.concat FanConfig.camlp4_standard_library
            "Camlp4Printers") ));
        (include_dir q (
          (Filename.concat FanConfig.camlp4_standard_library "Camlp4Filters")
          ))
        end
      end else begin
        ()
      end;
      (include_dir q ".");
      q
      end
  let find_in_path (x) (name) =
    if (not ( (Filename.is_implicit name) )) then begin
      if (Sys.file_exists name) then begin
        name
      end else begin
        (raise Not_found )
      end
    end else begin
      let res =
        (fold_load_path x (
          (fun (dir) ->
            (function
            | None  ->
                let fullname = (Filename.concat dir name) in
                if (Sys.file_exists fullname) then begin
                  Some (fullname)
                end else begin
                  None
                end
            | x ->   x)) ) None ) in
      begin match res with | None  ->   (raise Not_found )
                           | Some(x) ->   x
        end
    end
  let load =
    let _initialized = (ref false ) in
    (fun (_path) ->
      (fun (file) ->
        begin
        if (not ( _initialized.contents )) then begin
          begin try
          begin
            (Dynlink.init () );
            (Dynlink.allow_unsafe_modules true );
            (_initialized := true )
            end
          with
          | Dynlink.Error(e) ->
            (raise (
              Error
                (("Camlp4's dynamic loader initialization",(
                  (Dynlink.error_message e) ))) ))
        end
        end else begin
          ()
        end;
        let fname = begin try (find_in_path _path file)
          with
          | Not_found  ->
            (raise ( Error ((file,"file not found in path")) ))
        end in begin try (Dynlink.loadfile fname)
          with
          | Dynlink.Error(e) ->
            (raise ( Error ((fname,( (Dynlink.error_message e) ))) ))
        end
        end)) let is_native = Dynlink.is_native
  end 