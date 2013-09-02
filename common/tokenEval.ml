
 
(*  {[valch '3' = 3  ]} *) 
let valch x = Char.code x - Char.code '0'

(*
  {[
  valch_hex 'a' = 10
  valch_hex 'A' = 10
  valch_hex '3' = 3
  ]}
  no error check
 *)      
let valch_hex x =
  let d = Char.code x in
  if d >= 97 then d - 87
  else if d >= 65 then d - 55
  else d - 48

let rec skip_indent (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some (' '|'\t') -> (XStream.junk __strm; skip_indent __strm)
  | _ -> ()

let skip_opt_linefeed (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some '\n' -> (XStream.junk __strm; ())
  | _ -> ()

let chr c =
  if c < 0 || c > 255 then failwith "invalid char token" else Char.chr c

(*
  {[
  backslash (XStream.of_string "321");;
  Exception: Failure "invalid char token".
  backslash (XStream.of_string "255");;
  - : char = '\255'
  backslash (XStream.of_string "xff");;
  - : char = '\255'
  '\255';;
  - : char = '\255'
  '\xff';;
  - : char = '\255'
  ]}
 *)  
(* let  backslash = parser *)
(*   |  ( '\010' | '\013' | '\\' | '\'' | ' ' | '"' as x)  -> x *)
(*   |  'n'   -> '\n' *)
(*   |  'r'   -> '\r' *)
(*   |  't'   -> '\t' *)
(*   |  'b'   -> '\b' *)
(*   |   ('0'..'9' as c1); ('0'..'9' as c2);  ('0'..'9' as c3)  -> *)
(*       chr (100 * (valch c1) + 10 * (valch c2) + (valch c3)) *)
(*   |  'x'; ('0'..'9' | 'a'..'f' | 'A'..'F' as c1) ; *)
(*       ('0'..'9' | 'a'..'f' | 'A'..'F' as c2)  -> *)
(*      chr (16 * (valch_hex c1) + (valch_hex c2))  *)

let backslash (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some ('\n'|'\r'|'\\'|'\''|' '|'"' as x) -> (XStream.junk __strm; x)
  | Some 'n' -> (XStream.junk __strm; '\n')
  | Some 'r' -> (XStream.junk __strm; '\r')
  | Some 't' -> (XStream.junk __strm; '\t')
  | Some 'b' -> (XStream.junk __strm; '\b')
  | Some ('0'..'9' as c1) ->
      (XStream.junk __strm;
       (match XStream.peek __strm with
        | Some ('0'..'9' as c2) ->
            (XStream.junk __strm;
             (match XStream.peek __strm with
              | Some ('0'..'9' as c3) ->
                  (XStream.junk __strm;
                   chr
                     (((100 * (valch c1)) + (10 * (valch c2))) + (valch c3)))
              | _ -> raise (XStream.Error "")))
        | _ -> raise (XStream.Error "")))
  | Some 'x' ->
      (XStream.junk __strm;
       (match XStream.peek __strm with
        | Some ('0'|'1'..'9'|'a'..'f'|'A'..'F' as c1) ->
            (XStream.junk __strm;
             (match XStream.peek __strm with
              | Some ('0'|'1'..'9'|'a'..'f'|'A'..'F' as c2) ->
                  (XStream.junk __strm;
                   chr ((16 * (valch_hex c1)) + (valch_hex c2)))
              | _ -> raise (XStream.Error "")))
        | _ -> raise (XStream.Error "")))
  | _ -> raise XStream.NotConsumed


(* follow the ocaml convention *)    
(* let  backslash_in_string strict store = parser *)
(*   |  '\010'; 's  -> skip_indent s *)
(*   |  '\013'; 's  -> begin  skip_opt_linefeed s; skip_indent s  end *)
(*   |  x = backslash  -> store x *)
(*   |  c when not strict  -> begin  store '\\'; store c  end *)
(*   |  -> failwith "invalid string token"  *)
let backslash_in_string strict store (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some '\n' -> (XStream.junk __strm; skip_indent __strm)
  | Some '\r' ->
      (XStream.junk __strm;
       (let s = __strm in skip_opt_linefeed s; skip_indent s))
  | _ ->
      (match try Some (backslash __strm) with  XStream.NotConsumed  -> None with
       | Some x -> store x
       | _ ->
           (match XStream.peek __strm with
            | Some c when not strict ->
                (XStream.junk __strm; store '\\'; store c)
            | _ -> failwith "invalid string token"))
(*
  Exportered here wrap [backslash]
  {[
  char "\\321";;
  Exception: Failure "invalid char token".
  char "\\255";;
  - : char = '\255'
  char "\\xff" ;;
  - : char = '\255'
  ]}  
 *)
let char s =
  if String.length s = 1 then s.[0] (* normal *)
  else if String.length s = 0 then failwith "invalid char token"
  else
    let (__strm :_ XStream.t)= XStream.of_string s in
    match XStream.peek __strm with
    | Some '\\' ->
        (XStream.junk __strm;
         (try backslash __strm
         with  XStream.NotConsumed  -> raise (XStream.Error "Invalid char token")))
    | _ -> failwith "invalid char token"
    (* match XStream.of_string s with parser *)
    (* | '\\'; x = backslash  ?? "Invalid char token"-> x *)
    (* |  -> failwith "invalid char token"  *)

(*
  {[
  string "\\nag";;
  - : string = "\nag"
  string "\\tag";;
  - : string = "\tag"
  
  ]}
 *)    
let string ?strict s =
  let buf = Buffer.create 23 in
  let store = Buffer.add_char buf in
  let rec parse = fun (__strm : _ XStream.t)  ->
    match XStream.peek __strm with
    | Some '\\' ->
        (XStream.junk __strm;
         (let _ =
            try backslash_in_string (strict <> None) store __strm
            with | XStream.NotConsumed  -> raise (XStream.Error "") in
          parse __strm))
    | Some c -> (XStream.junk __strm; (let s = __strm in store c; parse s))
    | _ -> Buffer.contents buf
    (* parser *)
    (* |  '\\'; _ = backslash_in_string (strict <> None) store; 's  -> parse s *)
    (* |  c; 's  ->  begin store c; parse s end *)
    (* |  -> Buffer.contents buf  *)
  in parse (XStream.of_string s)
        



let char_of_char_token loc s =
  try char s with
  | Failure _ as exn -> FLoc.raise loc exn
        
let string_of_string_token loc s =
  try string s with
  | Failure _ as exn -> FLoc.raise loc exn 
                 
