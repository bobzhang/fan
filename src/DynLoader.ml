module type S = sig
  type t;
  exception Error of string and string;
  (** [mk ?ocaml_stdlib ?camlp4_stdlib]
      The stdlib flag is true by default.
      To disable it use: [mk ~ocaml_stdlib:False] *)
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
  (** [is_native] [True] if we are in native code, [False] for bytecode. *)
  val is_native : bool;
  val instance: ref (unit -> t);  
end;

module Make (U:sig end) : S= struct 
  type t = Queue.t string;

  let instance =  ref (fun () -> failwith "empty in dynloader");
  exception Error of string and string;

  let include_dir x y = Queue.add y x;

  let fold_load_path x f acc = Queue.fold (fun x y -> f y x) acc x;

  let mk ?(ocaml_stdlib = True) ?(camlp4_stdlib = True) () =
  let q = Queue.create () in do {
    if ocaml_stdlib then include_dir q FanConfig.ocaml_standard_library else ();
    if camlp4_stdlib then do {
      include_dir q FanConfig.camlp4_standard_library;
      (* include_dir q (Filename.concat FanConfig.camlp4_standard_library "Camlp4Parsers"); *)
      include_dir q (Filename.concat FanConfig.camlp4_standard_library "Camlp4Printers");
      include_dir q (Filename.concat FanConfig.camlp4_standard_library "Camlp4Filters");
    } else ();
    include_dir q ".";
  q
};
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
  let _initialized = ref False in
  fun _path file ->
    do {
      if not !_initialized then
        try do {
          Dynlink.init ();
          Dynlink.allow_unsafe_modules True;
         _initialized := True
        }
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
    };


let is_native = Dynlink.is_native;
end;
