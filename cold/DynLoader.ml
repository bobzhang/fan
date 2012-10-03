module type S =
 sig
  type t

  exception Error of string * string

  val mk : (?ocaml_stdlib : bool -> (?camlp4_stdlib : bool -> (unit -> t)))

  val fold_load_path : (t -> ((string -> ('a -> 'a)) -> ('a -> 'a)))

  val load : (t -> (string -> unit))

  val include_dir : (t -> (string -> unit))

  val find_in_path : (t -> (string -> string))

  val is_native : bool

  val instance : (unit -> t) ref

 end

module Make =
       functor (U : sig end) ->
        (struct
          type t = string Queue.t

          let instance = (ref ( fun ()  -> (failwith "empty in dynloader") ))

          exception Error of string * string

          let include_dir = fun x -> fun y -> (Queue.add y x)

          let fold_load_path =
           fun x ->
            fun f ->
             fun acc -> (Queue.fold ( fun x -> fun y -> (f y x) ) acc x)

          let mk =
           fun ?(ocaml_stdlib = (true)) ->
            fun ?(camlp4_stdlib = (true)) ->
             fun ()
               ->
              let q = (Queue.create () ) in
              (
              if ocaml_stdlib then
               (
               (include_dir q FanConfig.ocaml_standard_library)
               )
              else ()
              );
              (
              if camlp4_stdlib
              then
               begin
               (
               (include_dir q FanConfig.camlp4_standard_library)
               );
               (
               (include_dir q (
                 (Filename.concat FanConfig.camlp4_standard_library
                   "Camlp4Parsers") ))
               );
               (
               (include_dir q (
                 (Filename.concat FanConfig.camlp4_standard_library
                   "Camlp4Printers") ))
               );
               (include_dir q (
                 (Filename.concat FanConfig.camlp4_standard_library
                   "Camlp4Filters") ))
              end else ()
              );
              (
              (include_dir q ".")
              );
              q

          let find_in_path =
           fun x ->
            fun name ->
             if (not ( (Filename.is_implicit name) )) then
              (
              if (Sys.file_exists name) then name else (raise Not_found )
              )
             else
              let res =
               (fold_load_path x (
                 fun dir ->
                  function
                  | None ->
                     let fullname = (Filename.concat dir name) in
                     if (Sys.file_exists fullname) then ( (Some (fullname)) )
                     else (None)
                  | x -> x ) None ) in
              (match res with | None -> (raise Not_found ) | Some (x) -> x)

          let load =
           let _initialized = (ref false ) in
           fun _path ->
            fun file ->
             (
             if (not ( _initialized.contents )) then
              (
              (try
                (
               (Dynlink.init () )
               );
                (
               (Dynlink.allow_unsafe_modules true )
               );
                (_initialized := true )
               with
               Dynlink.Error (e) ->
                (raise (
                  (Error
                    ("Camlp4's dynamic loader initialization", (
                     (Dynlink.error_message e) ))) )))
              )
             else ()
             );
             let fname =
              (try (find_in_path _path file) with
               Not_found ->
                (raise ( (Error (file, "file not found in path")) ))) in
             (try (Dynlink.loadfile fname) with
              Dynlink.Error (e) ->
               (raise ( (Error (fname, ( (Dynlink.error_message e) ))) )))

          let is_native = Dynlink.is_native

         end : S)
