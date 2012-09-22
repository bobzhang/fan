(* utilty modules for Camlp4 *)

open Format

(* copied from otypes.ml Values *)
let valid_float_lexeme s =
  let l = String.length s in
  let rec loop i =
    if i >= l
    then s ^ "."
    else (match s.[i] with | '0' .. '9' | '-' -> loop (i + 1) | _ -> s)
  in loop 0
    
let float_repres f =
  match classify_float f with
  | FP_nan -> "nan"
  | FP_infinite -> if f < 0.0 then "neg_infinity" else "infinity"
  | _ ->
      let float_val =
        let s1 = Printf.sprintf "%.12g" f
        in
        if f = (float_of_string s1)
        then s1
        else
          (let s2 = Printf.sprintf "%.15g" f
          in
          if f = (float_of_string s2)
          then s2
          else Printf.sprintf "%.18g" f)
      in valid_float_lexeme float_val

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ s))
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ s))
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ s))
    
module SSet = Set.Make (String)


(* either dump to a file or stdout *)    
let with_open_out_file x f =
  match x with
  | Some file -> let oc = open_out_bin file in (f oc; flush oc; close_out oc)
  | None -> (set_binary_mode_out stdout true; f stdout; flush stdout)
        
(* dump binary *)
let dump_ast magic ast oc =
  (output_string oc magic; output_value oc ast)
    
let dump_pt magic fname pt oc =
  (output_string oc magic;
   output_value oc (if fname = "-" then "" else fname);
   output_value oc pt)


(** Handle Obj utils *)    
module ObjTools = struct
  
  let desc obj =
    if Obj.is_block obj then
      "tag = " ^ (string_of_int (Obj.tag obj))
    else "int_val = " ^ (string_of_int (Obj.obj obj))
                          
                          
  let rec to_string r =
    if Obj.is_int r   then
      (let i : int = Obj.magic r in
      (string_of_int i) ^ (" | CstTag" ^ (string_of_int (i + 1))))
    else (* Block. *)
      (let rec get_fields acc = function
        | 0 -> acc
        | n -> let n = n - 1 in get_fields ((Obj.field r n) :: acc) n in
      let rec is_list r =
        if Obj.is_int r
        then r = (Obj.repr 0)
        else (* [] *)
          (let s = Obj.size r
          and t = Obj.tag r
          in (t = 0) && ((s = 2) && (is_list (Obj.field r 1)))) in
      (* h :: t *)
      let rec get_list r =
        if Obj.is_int r
        then []
        else
          (let h = Obj.field r 0
          and t = get_list (Obj.field r 1)
          in h :: t) in
      let opaque name =
        (* XXX In future, print the address of value 'r'.  Not possible in
           pure OCaml at the moment.     *)
        "<" ^ (name ^ ">") in
      let s = Obj.size r
      and t = Obj.tag r in
      (* From the tag, determine the type of block. *)
      match t with
      | _ when is_list r -> let fields = get_list r in
        "[" ^ ((String.concat "; " (List.map to_string fields)) ^ "]")
      | 0 -> let fields = get_fields [] s in
        "(" ^ ((String.concat ", " (List.map to_string fields)) ^ ")")
      | x when x = Obj.lazy_tag ->
          (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
           * clear if very large constructed values could have the same
           * tag. XXX *)
          opaque "lazy"
      | x when x = Obj.closure_tag -> opaque "closure"
      | x when x = Obj.object_tag ->  let fields = get_fields [] s in
        let (_class, id, slots) =
          (match fields with
          | h :: h' :: t -> (h, h', t)
          | _ -> assert false)  in
        (* No information on decoding the class (first field).  So just print
         * out the ID and the slots. *)
        "Object #" ^
        ((to_string id) ^
         (" (" ^
          ((String.concat ", " (List.map to_string slots)) ^
           ")")))
      | x when x = Obj.infix_tag -> opaque "infix"
      | x when x = Obj.forward_tag -> opaque "forward"
      | x when x < Obj.no_scan_tag ->
          let fields = get_fields [] s in
          "Tag" ^
          ((string_of_int t) ^
           (" (" ^
            ((String.concat ", " (List.map to_string fields)) ^
             ")")))
      | x when x = Obj.string_tag ->
          "\"" ^ ((String.escaped (Obj.magic r : string)) ^ "\"")
      | x when x = Obj.double_tag ->
          float_repres (Obj.magic r : float)
      | x when x = Obj.abstract_tag -> opaque "abstract"
      | x when x = Obj.custom_tag -> opaque "custom"
      | x when x = Obj.final_tag -> opaque "final"
      | _ -> failwith
            ("ObjTools.to_string: unknown tag (" ^
             ((string_of_int t) ^ ")")))

        (*
          {[
          ObjTools.print std_formatter (Obj.repr [|32;1;2|]);;
          (32 | CstTag33, 1 | CstTag2, 2 | CstTag3)- : unit = ()
          ObjTools.print_desc std_formatter (Obj.repr [|32;1;2|]);;
          tag = 0- : unit = ()
          ]}
         *)        
  let print ppf (x:Obj.t) = fprintf ppf "%s" (to_string x)
  let print_desc ppf (x:Obj.t) = fprintf ppf "%s" (desc x)
end


(********************************************************************)
module ErrorHandler = struct
  let default_handler ppf x =
    let x = Obj.repr x in
    (fprintf ppf "Camlp4: Uncaught exception: %s"
       (Obj.obj (Obj.field (Obj.field x 0) 0) : string);
     if (Obj.size x) > 1  then
       (pp_print_string ppf " (";
        for i = 1 to (Obj.size x) - 1 do
          if i > 1 then pp_print_string ppf ", " else ();
          ObjTools.print ppf (Obj.field x i)
        done;
        pp_print_char ppf ')')
     else ();
     fprintf ppf "@.")
  let handler = ref (fun ppf default_handler exn -> default_handler ppf exn)
  let register f =
    let current_handler = !handler in
    handler :=
    fun ppf default_handler exn ->
      try f ppf exn with | exn -> current_handler ppf default_handler exn
          
  module Register (Error : FanSig.Error) = struct
    let _ =
      let current_handler = !handler in
      handler :=
      fun ppf default_handler ->
        function
          | Error.E x -> Error.print ppf x
          | x -> current_handler ppf default_handler x
  end
      
  let gen_print ppf default_handler =   function
    | Out_of_memory -> fprintf ppf "Out of memory"
    | Assert_failure ((file, line, char)) ->
        fprintf ppf "Assertion failed, file %S, line %d, char %d" file line
          char
    | Match_failure ((file, line, char)) ->
        fprintf ppf "Pattern matching failed, file %S, line %d, char %d" file
          line char
    | Failure str -> fprintf ppf "Failure: %S" str
    | Invalid_argument str -> fprintf ppf "Invalid argument: %S" str
    | Sys_error str -> fprintf ppf "I/O error: %S" str
    | Stream.Failure -> fprintf ppf "Parse failure"
    | Stream.Error str -> fprintf ppf "Parse error: %s" str
    | x -> !handler ppf default_handler x
          
  let print ppf = gen_print ppf default_handler
      
  let try_print ppf = gen_print ppf (fun _ -> raise)
      
  let to_string exn =
    let buf = Buffer.create 128 in
    let () = bprintf buf "%a" print exn in Buffer.contents buf
      
  let try_to_string exn =
    let buf = Buffer.create 128 in
    let () = bprintf buf "%a" try_print exn in Buffer.contents buf
      
end

(********************************************************************************)
module DynLoader = struct
  type t = string Queue.t

  exception Error of string * string
      
  let include_dir x y = Queue.add y x
      
  let fold_load_path x f acc = Queue.fold (fun x y -> f y x) acc x
      
  let mk ?(ocaml_stdlib = true) ?(camlp4_stdlib = true) () =
    let q = Queue.create ()
    in
    (if ocaml_stdlib
    then include_dir q FanConfig.ocaml_standard_library
    else ();
     if camlp4_stdlib
     then
       (include_dir q FanConfig.camlp4_standard_library;
        include_dir q
          (Filename.concat FanConfig.camlp4_standard_library "Camlp4Parsers");
        include_dir q
          (Filename.concat FanConfig.camlp4_standard_library "Camlp4Printers");
        include_dir q
          (Filename.concat FanConfig.camlp4_standard_library "Camlp4Filters"))
     else ();
     include_dir q ".";
     q)
      
(* Load files in core *)
  let find_in_path x name =
    if not (Filename.is_implicit name)
    then if Sys.file_exists name then name else raise Not_found
    else
      (let res =
        fold_load_path x
          (fun dir ->
            function
              | None ->
                  let fullname = Filename.concat dir name
                  in if Sys.file_exists fullname then Some fullname else None
              | x -> x)
          None
      in match res with | None -> raise Not_found | Some x -> x)
        
  let load =
    let _initialized = ref false
    in
    fun _path file ->
      (if not !_initialized
      then
        (try
          (Dynlink.init ();
           Dynlink.allow_unsafe_modules true;
           _initialized := true)
        with
        | Dynlink.Error e ->
            raise
              (Error ("Camlp4's dynamic loader initialization",
                      (Dynlink.error_message e))))
      else ();
       let fname =
         try find_in_path _path file
         with | Not_found -> raise (Error (file, "file not found in path"))
       in
       try Dynlink.loadfile fname
       with
       | Dynlink.Error e ->
           raise (Error (fname, (Dynlink.error_message e))))
        
  let is_native = Dynlink.is_native

end

(********************************************************************************)
module Options :sig
  type spec_list = (string * Arg.spec * string) list
        
  val init : spec_list -> unit
      
  val add : string -> Arg.spec -> string -> unit
      
(** Add an option to the command line options. *)
  val print_usage_list : spec_list -> unit
  val ext_spec_list : unit -> spec_list
  val parse : (string -> unit) -> string array -> string list
end= struct
  type spec_list = (string * Arg.spec * string) list

  open Format
    
  let rec action_arg s sl =
    function
      | Arg.Unit f -> if s = "" then (f (); Some sl) else None
      | Arg.Bool f ->
          if s = ""
          then
            (match sl with
            | s :: sl ->
                (try (f (bool_of_string s); Some sl)
                with | Invalid_argument "bool_of_string" -> None)
            | [] -> None)
          else
            (try (f (bool_of_string s); Some sl)
            with | Invalid_argument "bool_of_string" -> None)
      | Arg.Set r -> if s = "" then (r := true; Some sl) else None
      | Arg.Clear r -> if s = "" then (r := false; Some sl) else None
      | Arg.Rest f -> (List.iter f (s :: sl); Some [])
      | Arg.String f ->
          if s = ""
          then (match sl with | s :: sl -> (f s; Some sl) | [] -> None)
          else (f s; Some sl)
      | Arg.Set_string r ->
          if s = ""
          then (match sl with | s :: sl -> (r := s; Some sl) | [] -> None)
          else (r := s; Some sl)
      | Arg.Int f ->
          if s = ""
          then
            (match sl with
            | s :: sl ->
                (try (f (int_of_string s); Some sl)
                with | Failure "int_of_string" -> None)
            | [] -> None)
          else
            (try (f (int_of_string s); Some sl)
            with | Failure "int_of_string" -> None)
      | Arg.Set_int r ->
          if s = ""
          then
            (match sl with
            | s :: sl ->
                (try (r := int_of_string s; Some sl)
                with | Failure "int_of_string" -> None)
            | [] -> None)
          else
            (try (r := int_of_string s; Some sl)
            with | Failure "int_of_string" -> None)
      | Arg.Float f ->
          if s = ""
          then
            (match sl with
            | s :: sl -> (f (float_of_string s); Some sl)
            | [] -> None)
          else (f (float_of_string s); Some sl)
      | Arg.Set_float r ->
          if s = ""
          then
            (match sl with
            | s :: sl -> (r := float_of_string s; Some sl)
            | [] -> None)
          else (r := float_of_string s; Some sl)
      | Arg.Tuple specs ->
          let rec action_args s sl =
            (function
              | [] -> Some sl
              | spec :: spec_list ->
                  (match action_arg s sl spec with
                  | None -> action_args "" [] spec_list
                  | Some (s :: sl) -> action_args s sl spec_list
                  | Some sl -> action_args "" sl spec_list))
          in action_args s sl specs
      | Arg.Symbol (syms, f) ->
          (match if s = "" then sl else s :: sl with
          | s :: sl when List.mem s syms -> (f s; Some sl)
          | _ -> None)
            
  let common_start s1 s2 =
    let rec loop i =
      if (i == (String.length s1)) || (i == (String.length s2))
      then i
      else if s1.[i] == s2.[i] then loop (i + 1) else i
    in loop 0
      
  let parse_arg fold s sl =
    fold
      (fun (name, action, _) acu ->
        let i = common_start s name
        in
        if i == (String.length name)
        then
          (try action_arg (String.sub s i ((String.length s) - i)) sl action
          with | Arg.Bad _ -> acu)
        else acu)
      None
      
  let rec parse_aux fold anon_fun =
    function
      | [] -> []
      | s :: sl ->
          if ((String.length s) > 1) && (s.[0] = '-')
          then
            (match parse_arg fold s sl with
            | Some sl -> parse_aux fold anon_fun sl
            | None -> s :: (parse_aux fold anon_fun sl))
          else ((anon_fun s : unit); parse_aux fold anon_fun sl)
              
  let align_doc key s =
    let s =
      let rec loop i =
        if i = (String.length s)
        then ""
        else
          if s.[i] = ' '
          then loop (i + 1)
          else String.sub s i ((String.length s) - i)
      in loop 0 in
    let (p, s) =
      if (String.length s) > 0
      then
        if s.[0] = '<'
        then
          (let rec loop i =
            if i = (String.length s)
            then ("", s)
            else
              if s.[i] <> '>'
              then loop (i + 1)
              else
                (let p = String.sub s 0 (i + 1) in
                let rec loop i =
                  if i >= (String.length s)
                  then (p, "")
                  else
                    if s.[i] = ' '
                    then loop (i + 1)
                    else (p, (String.sub s i ((String.length s) - i)))
                in loop (i + 1))
          in loop 0)
        else ("", s)
      else ("", "") in
    let tab =
      String.make (max 1 ((16 - (String.length key)) - (String.length p))) ' '
    in p ^ (tab ^ s)
             
  let make_symlist l =
    match l with
    | [] -> "<none>"
    | h :: t -> (List.fold_left (fun x y -> x ^ ("|" ^ y)) ("{" ^ h) t) ^ "}"
                                                                            
  let print_usage_list l =
    List.iter
      (fun (key, spec, doc) ->
        match spec with
        | Arg.Symbol (symbs, _) ->
            let s = make_symlist symbs in
            let synt = key ^ (" " ^ s)
            in eprintf "  %s %s\n" synt (align_doc synt doc)
        | _ -> eprintf "  %s %s\n" key (align_doc key doc))
      l
      
  let remaining_args argv =
    let rec loop l i =
      if i == (Array.length argv) then l else loop (argv.(i) :: l) (i + 1)
    in List.rev (loop [] (!Arg.current + 1))
      
  let init_spec_list = ref []
      
  let ext_spec_list = ref []
      
  let init spec_list = init_spec_list := spec_list
      
  let add name spec descr =
    ext_spec_list := (name, spec, descr) :: !ext_spec_list
                                             
  let fold f init =
    let spec_list = !init_spec_list @ !ext_spec_list in
    let specs = Sort.list (fun (k1, _, _) (k2, _, _) -> k1 >= k2) spec_list
    in List.fold_right f specs init
      
  let parse anon_fun argv =
    let remaining_args = remaining_args argv
    in parse_aux fold anon_fun remaining_args
      
  let ext_spec_list () = !ext_spec_list
      


end
(********************************************************************************)

let (|>)  x f  = f x 
let uncurry f (x,y) = f x y
let flip f x y = f y x
