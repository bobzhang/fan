(** TODO: add an unit test  for import DDSL *)
(* %import{ *)
(* Prelude: *)
(*   parse_file *)
(*   parse_interf *)
(*   parse_implem *)
(*   ; *)
(* Format: *)
(*   eprintf *)
(*   fprintf *)
(*   printf *)
(*   ; *)
(* };; *)

let parse_file  = Prelude.parse_file
let eprintf = Format.eprintf 
let fprintf = Format.fprintf
let printf = Format.printf


open Util

    
let just_print_filters () =
  let pp = eprintf  in 
  let p_tbl f tbl = Hashtbl.iter (fun k _v -> fprintf f "%s@;" k) tbl in
  begin
    pp  "@[for interface:@[<hv2>%a@]@]@." p_tbl Ast_filters.interf_filters ;
    pp  "@[for phrase:@[<hv2>%a@]@]@." p_tbl Ast_filters.implem_filters ;
    pp  "@[for top_phrase:@[<hv2>%a@]@]@." p_tbl Ast_filters.topphrase_filters 
  end
let just_print_parsers () =
  let pp = eprintf in
  let p_tbl f tbl = Hashtbl.iter (fun k _v -> fprintf f "%s@;" k) tbl in begin
    pp "@[Loaded Parsers:@;@[<hv2>%a@]@]@." p_tbl Ast_parsers.registered_parsers
  end
  
let just_print_applied_parsers () =
  let pp = eprintf in
  pp "@[Applied Parsers:@;@[<hv2>%a@]@]@."
    (fun f q -> Queue.iter (fun (k,_) -> fprintf f "%s@;" k) q  ) Ast_parsers.applied_parsers
  
type file_kind =
  | Intf of string
  | Impl of string
  | Str of string
  | ModuleImpl of string
  | IncludeDir of string 
  

    
let print_loaded_modules = ref false






let output_file = ref None              


(** parse the file, apply the filter and pipe it to the backend *)  
let process_intf  name =
  let v =
    Option.map Ast_filters.apply_interf_filters
    @@ parse_file name Prelude.parse_interf in
  Prelude.CurrentPrinter.print_interf
    ?input_file:(Some name)
    ?output_file:(!output_file) v 


let process_impl name =
  let v =
    Option.map Ast_filters.apply_implem_filters
    @@ parse_file name Prelude.parse_implem in
  Prelude.CurrentPrinter.print_implem
    ~input_file:name
    ?output_file:(!output_file) v 

          
      
let input_file x =
  match x with
  | Intf file_name ->
      begin
        if file_name <> "-" then 
          Configf.compilation_unit :=
            Some (String.capitalize (Filename.(chop_extension (basename file_name))));
        Configf.current_input_file := file_name;
        process_intf  file_name
      end
  | Impl file_name ->
      begin
        if file_name <> "-" then 
          Configf.compilation_unit :=
            Some (String.capitalize (Filename.(chop_extension (basename file_name))));
        Configf.current_input_file := file_name;
        process_impl  file_name;
      end
  | Str s ->
      let (f, o) = Filename.open_temp_file "from_string" ".ml" in
      (output_string o s;
       close_out o;
       Configf.current_input_file := f;
       process_impl  f;
       at_exit (fun () -> Sys.remove f))
        
  | ModuleImpl file_name -> Control_require.add  file_name

  | IncludeDir dir ->
      Ref.modify Configf.dynload_dirs (cons dir) 











(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/fan_args.cmo" *)
(* end: *)
