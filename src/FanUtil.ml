
open Format;

(* copied from otypes.ml Values *)
let valid_float_lexeme s =
  let l = String.length s in
  let rec loop i =
    if i >= l
    then s ^ "."
    else (match s.[i] with [ '0' .. '9' | '-' -> loop (i + 1) | _ -> s])
  in loop 0;
    
let float_repres f =
  match classify_float f with
  [ FP_nan -> "nan"
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
      in valid_float_lexeme float_val];

let cvt_int_literal s =
  - int_of_string ("-" ^ s);
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ s));
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ s));
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ s));

let is_antiquot s =
  let len = String.length s in
  len > 2 && s.[0] = '\\' && s.[1] = '$';

let handle_antiquot_in_string s term parse loc decorate =
  if is_antiquot s then
    let pos = String.index s ':' in
    let name = String.sub s 2 (pos - 2)
    and code = String.sub s (pos + 1) (String.length s - pos - 1) in
    decorate name (parse loc code)
  else term;

    

(* either dump to a file or stdout *)    
let with_open_out_file x f =
  match x with
  [ Some file -> let oc = open_out_bin file in begin f oc; flush oc; close_out oc end
  | None -> (set_binary_mode_out stdout True; f stdout; flush stdout) ];
        
(* dump binary *)
let dump_ast magic ast oc =
  begin output_string oc magic; output_value oc ast end;
    
let dump_pt magic fname pt oc = begin
  output_string oc magic;
  output_value oc (if fname = "-" then "" else fname);
  output_value oc pt
end;


let char_of_char_token loc s =
  try TokenEval.char s with [ Failure _ as exn -> FanLoc.raise loc exn ] ;
    
let string_of_string_token loc s =
  try TokenEval.string s
  with [ Failure _ as exn -> FanLoc.raise loc exn ] ;


(*
  {[
  remove_underscores "_a";;
  - : string = "a"
  remove_underscores "__a";;
  - : string = "a"
  remove_underscores "__a___b";;
  - : string = "ab"
  remove_underscores "__a___b__";;
  - : string = "ab"
  ]}
 *)    
let remove_underscores s =
  let l = String.length s in
  let buf = Buffer.create l in
  let () = String.iter (fun ch ->
    if ch <> '_' then ignore (Buffer.add_char buf ch) else () ) s in
  Buffer.contents buf ;
    

(********************************************************************)
module ErrorHandler = struct
  (*
    {[
    let a : string = Obj.(obj (field (field (repr (Failure "haha")) 0) 0));;
    val a : string = "Failure"
    ]}
   *)
  let default_handler ppf x =
    let x = Obj.repr x in begin 
      fprintf ppf "Camlp4: Uncaught exception: %s" (Obj.(obj (field (field x 0) 0)) : string);
      if (Obj.size x) > 1  then begin
        pp_print_string ppf " (";
        for i = 1 to (Obj.size x) - 1 do
          if i > 1 then pp_print_string ppf ", " else ();
          pp_print_string ppf  (BatPervasives.dump  (Obj.field x i))
        done;
        pp_print_char ppf ')'
      end
     else ();
     fprintf ppf "@."
    end;
  let handler =
    ref (fun ppf default_handler exn -> default_handler ppf exn);
  let register f =
    let current_handler = !handler in
    handler :=
    fun ppf default_handler exn ->
      try f ppf exn with exn -> current_handler ppf default_handler exn;
          
  module Register (Error : FanSig.Error) = struct
    let _ =
      let current_handler = !handler in
      handler :=
      fun ppf default_handler ->
        fun
          [ Error.E x -> Error.print ppf x
          | x -> current_handler ppf default_handler x];
  end;
      
  let gen_print ppf default_handler =   fun
    [ Out_of_memory -> fprintf ppf "Out of memory"
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
    | x -> !handler ppf default_handler x];
          
  let print ppf = gen_print ppf default_handler;
      
  let try_print ppf = gen_print ppf (fun _ -> raise); (* FIXME*)
      
  let to_string exn =
    let buf = Buffer.create 128 in
    let () = bprintf buf "%a" print exn in Buffer.contents buf;
      
  let try_to_string exn =
    let buf = Buffer.create 128 in
    let () = bprintf buf "%a" try_print exn in Buffer.contents buf;
      
end;


(********************************************************************************)
module Options :sig
  type spec_list = list (string * Arg.spec *  string);
        
  val init : spec_list -> unit;
      
  val add : string -> Arg.spec -> string -> unit;
      
(** Add an option to the command line options. *)
  val print_usage_list : spec_list -> unit;
  val ext_spec_list : unit -> spec_list;
  val parse : (string -> unit) -> array string  -> list string ;
end= struct
  type spec_list = list (string  * Arg.spec * string);

  open Format;
    
  let rec action_arg s sl = fun 
      [ Arg.Unit f -> if s = "" then (f (); Some sl) else None
      | Arg.Bool f ->
          if s = ""
          then
            (match sl with
            [ [s :: sl] ->
                (try (f (bool_of_string s); Some sl)
                with [ Invalid_argument "bool_of_string" -> None])
            | [] -> None])
          else
            (try (f (bool_of_string s); Some sl)
            with [ Invalid_argument "bool_of_string" -> None])
      | Arg.Set r -> if s = "" then (r := True; Some sl) else None
      | Arg.Clear r -> if s = "" then (r := False; Some sl) else None
      | Arg.Rest f -> (List.iter f ([s :: sl]); Some [])
      | Arg.String f ->
          if s = ""
          then (match sl with [ [s :: sl] -> (f s; Some sl) | [] -> None])
          else (f s; Some sl)
      | Arg.Set_string r ->
          if s = ""
          then (match sl with [ [s :: sl] -> (r := s; Some sl) | [] -> None])
          else (r := s; Some sl)
      | Arg.Int f ->
          if s = ""
          then
            (match sl with
            [ [s :: sl] ->
                (try (f (int_of_string s); Some sl)
                with [Failure "int_of_string" -> None])
            | [] -> None])
          else
            (try (f (int_of_string s); Some sl)
            with [Failure "int_of_string" -> None])
      | Arg.Set_int r ->
          if s = ""
          then
            (match sl with
            [ [s :: sl] ->
                (try (r := int_of_string s; Some sl)
                with [ Failure "int_of_string" -> None])
            | [] -> None])
          else
            (try (r := int_of_string s; Some sl)
            with [ Failure "int_of_string" -> None])
      | Arg.Float f ->
          if s = ""
          then
            (match sl with
            [ [s :: sl] -> (f (float_of_string s); Some sl)
            | [] -> None])
          else (f (float_of_string s); Some sl)
      | Arg.Set_float r ->
          if s = ""
          then
            (match sl with
            [ [s :: sl] -> (r := float_of_string s; Some sl)
            | [] -> None])
          else (r := float_of_string s; Some sl)
      | Arg.Tuple specs ->
          let rec action_args s sl =
            (fun
              [ [] -> Some sl
              | [spec :: spec_list] ->
                  (match action_arg s sl spec with
                  [ None -> action_args "" [] spec_list
                  | Some [s :: sl] -> action_args s sl spec_list
                  | Some sl -> action_args "" sl spec_list])])
          in action_args s sl specs
      | Arg.Symbol (syms, f) ->
          (match if s = "" then sl else [s :: sl] with
          [ [s :: sl] when List.mem s syms -> (f s; Some sl)
          | _ -> None])];
            
  let common_start s1 s2 =
    let rec loop i =
      if (i == (String.length s1)) || (i == (String.length s2))
      then i
      else if s1.[i] == s2.[i] then loop (i + 1) else i (* FIXME == *)
    in loop 0;
      
  let parse_arg fold s sl =
    fold
      (fun (name, action, _) acu ->
        let i = common_start s name
        in
        if i == (String.length name)
        then
          (try action_arg (String.sub s i ((String.length s) - i)) sl action
          with [ Arg.Bad _ -> acu])
        else acu)
      None;
      
  let rec parse_aux fold anon_fun =
    fun
      [ [] -> []
      | [s :: sl] ->
          if ((String.length s) > 1) && (s.[0] = '-')
          then
            (match parse_arg fold s sl with
            [ Some sl -> parse_aux fold anon_fun sl
            | None -> [s :: (parse_aux fold anon_fun sl)]])
          else ((anon_fun s : unit); parse_aux fold anon_fun sl)];
              
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
    in p ^ (tab ^ s);
             
  let make_symlist l =
    match l with
    [ [] -> "<none>"
    | [h :: t] -> (List.fold_left (fun x y -> x ^ ("|" ^ y)) ("{" ^ h) t) ^ "}"];
                                                                            
  let print_usage_list l =
    List.iter
      (fun (key, spec, doc) ->
        match spec with
        [ Arg.Symbol (symbs, _) ->
            let s = make_symlist symbs in
            let synt = key ^ (" " ^ s)
            in eprintf "  %s %s\n" synt (align_doc synt doc)
        | _ -> eprintf "  %s %s\n" key (align_doc key doc)])
      l;
      
  let remaining_args argv =
    let rec loop l i =
      if i == (Array.length argv) then l else loop [argv.(i) :: l] (i + 1)
    in List.rev (loop [] (!Arg.current + 1));
      
  let init_spec_list = ref [];
      
  let ext_spec_list = ref [];
      
  let init spec_list = init_spec_list := spec_list;
      
  let add name spec descr =
    ext_spec_list := [(name, spec, descr) :: !ext_spec_list];
                                             
  let fold f init =
    let spec_list = !init_spec_list @ !ext_spec_list in
    let specs = Sort.list (fun (k1, _, _) (k2, _, _) -> k1 >= k2) spec_list
    in List.fold_right f specs init;
      
  let parse anon_fun argv =
    let remaining_args = remaining_args argv
    in parse_aux fold anon_fun remaining_args;
      
  let ext_spec_list () = !ext_spec_list;
      


end;
