let parse_file = Prelude.parse_file
let parse_interf = Prelude.parse_interf
let parse_implem = Prelude.parse_implem
let eprintf = Format.eprintf
let fprintf = Format.fprintf
let printf = Format.printf
open Util
let just_print_filters =
  function
  | () ->
      let pp = eprintf in
      let p_tbl =
        function
        | f ->
            (function
             | tbl ->
                 Hashtbl.iter
                   (function | k -> (function | _v -> fprintf f "%s@;" k))
                   tbl) in
      (pp "@[for interface:@[<hv2>%a@]@]@." p_tbl Ast_filters.interf_filters;
       pp "@[for phrase:@[<hv2>%a@]@]@." p_tbl Ast_filters.implem_filters;
       pp "@[for top_phrase:@[<hv2>%a@]@]@." p_tbl
         Ast_filters.topphrase_filters)
let just_print_parsers =
  function
  | () ->
      let pp = eprintf in
      let p_tbl =
        function
        | f ->
            (function
             | tbl ->
                 Hashtbl.iter
                   (function | k -> (function | _v -> fprintf f "%s@;" k))
                   tbl) in
      pp "@[Loaded Parsers:@;@[<hv2>%a@]@]@." p_tbl
        Ast_parsers.registered_parsers
let just_print_applied_parsers =
  function
  | () ->
      let pp = eprintf in
      pp "@[Applied Parsers:@;@[<hv2>%a@]@]@."
        (function
         | f ->
             (function
              | q -> Queue.iter (function | (k,_) -> fprintf f "%s@;" k) q))
        Ast_parsers.applied_parsers
type file_kind =
  | Intf of string
  | Impl of string
  | Str of string
  | ModuleImpl of string
  | IncludeDir of string
let print_loaded_modules = ref false
let output_file = ref None
let process_intf =
  function
  | name ->
      let v =
        (Option.map Ast_filters.apply_interf_filters) @@
          (parse_file name parse_interf) in
      Prelude.CurrentPrinter.print_interf ?input_file:(Some name)
        ?output_file:(!output_file) v
let process_impl =
  function
  | name ->
      let v =
        (Option.map Ast_filters.apply_implem_filters) @@
          (parse_file name parse_implem) in
      Prelude.CurrentPrinter.print_implem ~input_file:name
        ?output_file:(!output_file) v
let input_file =
  function
  | x ->
      (match x with
       | Intf file_name ->
           (if file_name <> "-"
            then
              Configf.compilation_unit :=
                (Some
                   (String.capitalize
                      (let open Filename in
                         chop_extension (basename file_name))));
            Configf.current_input_file := file_name;
            process_intf file_name)
       | Impl file_name ->
           (if file_name <> "-"
            then
              Configf.compilation_unit :=
                (Some
                   (String.capitalize
                      (let open Filename in
                         chop_extension (basename file_name))));
            Configf.current_input_file := file_name;
            process_impl file_name)
       | Str s ->
           let (f,o) = Filename.open_temp_file "from_string" ".ml" in
           (output_string o s;
            close_out o;
            Configf.current_input_file := f;
            process_impl f;
            at_exit (function | () -> Sys.remove f))
       | ModuleImpl file_name -> Control_require.add file_name
       | IncludeDir dir -> Ref.modify Configf.dynload_dirs (cons dir))
let initial_spec_list: (string* Arg.spec* string) list =
  [("-plugin", (String Control_require.add), "load plugin cma or cmxs files");
  ("-printer",
    (String
       ((function
         | s ->
             let x =
               try Hashtbl.find Prelude.backends s
               with | Not_found  -> failwithf "%s backend not found" s in
             (Prelude.sigi_printer := x.interf;
              Prelude.stru_printer := x.implem)))), " Set the backend")]
let anon_fun =
  function
  | name ->
      let check = Filename.check_suffix name in
      input_file
        (if check ".mli"
         then Intf name
         else
           if check ".ml"
           then Impl name
           else
             if check Dyn_load.objext
             then ModuleImpl name
             else
               if check Dyn_load.libext
               then ModuleImpl name
               else raise (Arg.Bad ("don't know what to do with " ^ name)))
