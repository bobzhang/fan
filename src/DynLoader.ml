module type S = sig
  type t;
  exception Error of string * string;
  (** [mk ?ocaml_stdlib ?camlp4_stdlib]
      The stdlib flag is true by default.
      To disable it use: [mk ~ocaml_stdlib:false] *)
  val mk : ?ocaml_stdlib: bool -> ?camlp4_stdlib: bool -> unit -> t;
  (** Fold over the current load path list. *)
  val fold_load_path : t -> (string -> 'a -> 'a) -> 'a -> 'a;
  (** [load f] Load the file [f]. If [f] is not an absolute path name,
      the load path list used to find the directory of [f]. *)
  val load : t -> string -> unit;
  (** [include_dir d] Add the directory [d] in the current load path
     list (like the common -I option). *)
  val include_dir : t -> string -> unit;
  (** [find_in_path f] Returns the full path of the file [f] if
     [f] is in the current load path, raises [Not_found] otherwise. *)
  val find_in_path : t -> string -> string;
  (** [is_native] [true] if we are in native code, [false] for bytecode. *)
  val is_native : bool;
  val instance: ref (unit -> t);  
end;

module Make (U:sig end) : S= struct 
  type t = Queue.t string;

  let instance =  ref (fun () -> failwith "empty in dynloader");
  exception Error of string * string;

  let include_dir x y = Queue.add y x;

  let fold_load_path x f acc = Queue.fold (fun x y -> f y x) acc x;

  let mk ?(ocaml_stdlib = true) ?(camlp4_stdlib = true) () =
  let q = Queue.create () in begin
    if ocaml_stdlib then include_dir q FanConfig.ocaml_standard_library else ();
    if camlp4_stdlib then begin
    end else ();
    include_dir q ".";
  q
end;
(* Load files in core *)
let find_in_path x name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else
    let res =
      fold_load_path x
        (fun dir ->
          fun
          [ None ->
              let fullname = Filename.concat dir name in
              if Sys.file_exists fullname then Some fullname else None
          | x -> x ]) None
    in match res with [ None -> raise Not_found | Some x -> x ];

let load =
  let _initialized = ref false in
  fun _path file ->
    begin
      if not !_initialized then
        try begin
          Dynlink.init ();
          Dynlink.allow_unsafe_modules true;
         _initialized := true
        end
        with
        [ Dynlink.Error e ->
           raise (Error "Camlp4's dynamic loader initialization" (Dynlink.error_message e)) ]
      else ();
      let fname =
        try find_in_path _path file with
        [ Not_found -> raise (Error file "file not found in path") ]
      in
      try Dynlink.loadfile fname with
      [ Dynlink.Error e -> raise (Error fname (Dynlink.error_message e)) ]
    end;


let is_native = Dynlink.is_native;
end;
