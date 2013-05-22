open Format

open LibUtil

let normal_handler =
  function
  | Out_of_memory  -> Some "Out of memory"
  | Assert_failure (file,line,char) ->
      Some
        (sprintf "Assertion failed, file %S, line %d, char %d" file line char)
  | Match_failure (file,line,char) ->
      Some
        (sprintf "Pattern matching failed, file %S, line %d, char %d" file
           line char)
  | Failure str -> Some (sprintf "Failure: %S" str)
  | Invalid_argument str -> Some (sprintf "Invalid argument: %S" str)
  | Sys_error str -> Some (sprintf "I/O error: %S" str)
  | XStream.Failure  -> Some (sprintf "Parse failure")
  | XStream.Error str -> Some (sprintf "XStream.Error %s" str)
  | _ -> None

let _ = Printexc.register_printer normal_handler

let valid_float_lexeme s =
  let l = String.length s in
  let rec loop i =
    if i >= l
    then s ^ "."
    else (match s.[i] with | '0'|'1'..'9'|'-' -> loop (i + 1) | _ -> s) in
  loop 0

let float_repres f =
  match classify_float f with
  | FP_nan  -> "nan"
  | FP_infinite  -> if f < 0.0 then "neg_infinity" else "infinity"
  | _ ->
      let float_val =
        let s1 = Printf.sprintf "%.12g" f in
        if f = (float_of_string s1)
        then s1
        else
          (let s2 = Printf.sprintf "%.15g" f in
           if f = (float_of_string s2) then s2 else Printf.sprintf "%.18g" f) in
      valid_float_lexeme float_val

let cvt_int_literal s =
  let n = String.length s in
  match s.[n - 1] with
  | 'l' -> `INT32 ((let open Int32 in neg (of_string ("-" ^ s))), s)
  | 'L' -> `INT64 ((let open Int64 in neg (of_string ("-" ^ s))), s)
  | 'n' -> `NATIVEINT ((let open Nativeint in neg (of_string ("-" ^ s))), s)
  | _ -> `INT ((- (int_of_string ("-" ^ s))), s)

open StdLib

let _ = begin (); () end

type anti_cxt = 
  {
  cxt: string;
  sep: string option;
  mutable decorations: string;
  content: string} 

let pp_print_anti_cxt: Format.formatter -> anti_cxt -> unit =
  fun fmt  { cxt = _a0; sep = _a1; decorations = _a2; content = _a3 }  ->
    Format.fprintf fmt
      "@[<hv 1>{cxt:%a;@,sep:%a;@,decorations:%a;@,content:%a}@]"
      pp_print_string _a0 (pp_print_option pp_print_string) _a1
      pp_print_string _a2 pp_print_string _a3

let mk_anti ?(c= "")  ?sep  loc n s =
  let c = { cxt = c; decorations = n; content = s; sep } in `Ant (loc, c)

let add_context s c = { s with decorations = (s.decorations ^ c) }

let symbolchars =
  ['$';
  '!';
  '%';
  '&';
  '*';
  '+';
  '-';
  '.';
  '/';
  ':';
  '<';
  '=';
  '>';
  '?';
  '@';
  '^';
  '|';
  '~';
  '\\']

let symbolchar s i =
  let len = String.length s in
  try
    begin
      for j = i to len - 1 do
        if not (List.mem (s.[j]) symbolchars) then raise Not_found else ()
      done; true
    end
  with | Not_found  -> false

let with_open_out_file x f =
  match x with
  | Some file ->
      let oc = open_out_bin file in begin f oc; flush oc; close_out oc end
  | None  ->
      begin set_binary_mode_out stdout true; f stdout; flush stdout end

let dump_ast magic ast oc =
  begin output_string oc magic; output_value oc ast end

let dump_pt magic fname pt oc =
  begin
    output_string oc magic;
    output_value oc (if fname = "-" then "" else fname); output_value oc pt
  end

let char_of_char_token loc s =
  try TokenEval.char s with | Failure _ as exn -> FanLoc.raise loc exn

let string_of_string_token loc s =
  try TokenEval.string s with | Failure _ as exn -> FanLoc.raise loc exn

let remove_underscores s =
  let l = String.length s in
  let buf = Buffer.create l in
  let () =
    String.iter
      (fun ch  -> if ch <> '_' then ignore (Buffer.add_char buf ch) else ())
      s in
  Buffer.contents buf

let destruct_poly s =
  let n = String.length s in
  if n = 0
  then invalid_arg "destruct_poly length=0"
  else if (s.[0]) = '`' then Some (String.sub s 1 (n - 1)) else None