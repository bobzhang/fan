let valch x = (Char.code x) - (Char.code '0')

let valch_hex x =
  let d = Char.code x in
  if d >= 97 then d - 87 else if d >= 65 then d - 55 else d - 48

let rec skip_indent (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some (' '|'\t') -> begin XStream.junk __strm; skip_indent __strm end
  | _ -> ()

let skip_opt_linefeed (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some '\n' -> begin XStream.junk __strm; () end
  | _ -> ()

let chr c =
  if (c < 0) || (c > 255) then failwith "invalid char token" else Char.chr c

let backslash (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some ('\n'|'\r'|'\\'|'\''|' '|'"' as x) ->
      begin XStream.junk __strm; x end
  | Some 'n' -> begin XStream.junk __strm; '\n' end
  | Some 'r' -> begin XStream.junk __strm; '\r' end
  | Some 't' -> begin XStream.junk __strm; '\t' end
  | Some 'b' -> begin XStream.junk __strm; '\b' end
  | Some ('0'..'9' as c1) ->
      begin
        XStream.junk __strm;
        (match XStream.peek __strm with
         | Some ('0'..'9' as c2) ->
             begin
               XStream.junk __strm;
               (match XStream.peek __strm with
                | Some ('0'..'9' as c3) ->
                    begin
                      XStream.junk __strm;
                      chr
                        (((100 * (valch c1)) + (10 * (valch c2))) +
                           (valch c3))
                    end
                | _ -> raise (XStream.Error ""))
             end
         | _ -> raise (XStream.Error ""))
      end
  | Some 'x' ->
      begin
        XStream.junk __strm;
        (match XStream.peek __strm with
         | Some ('0'|'1'..'9'|'a'..'f'|'A'..'F' as c1) ->
             begin
               XStream.junk __strm;
               (match XStream.peek __strm with
                | Some ('0'|'1'..'9'|'a'..'f'|'A'..'F' as c2) ->
                    begin
                      XStream.junk __strm;
                      chr ((16 * (valch_hex c1)) + (valch_hex c2))
                    end
                | _ -> raise (XStream.Error ""))
             end
         | _ -> raise (XStream.Error ""))
      end
  | _ -> raise XStream.Failure

let backslash_in_string strict store (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some '\n' -> begin XStream.junk __strm; skip_indent __strm end
  | Some '\r' ->
      begin
        XStream.junk __strm;
        (let s = __strm in begin skip_opt_linefeed s; skip_indent s end)
      end
  | _ ->
      (match try Some (backslash __strm) with | XStream.Failure  -> None with
       | Some x -> store x
       | _ ->
           (match XStream.peek __strm with
            | Some c when not strict ->
                begin XStream.junk __strm; store '\\'; store c end
            | _ -> failwith "invalid string token"))

let char s =
  if (String.length s) = 1
  then s.[0]
  else
    if (String.length s) = 0
    then failwith "invalid char token"
    else
      (let (__strm :_ XStream.t)= XStream.of_string s in
       match XStream.peek __strm with
       | Some '\\' ->
           begin
             XStream.junk __strm;
             (try backslash __strm
              with | XStream.Failure  -> raise (XStream.Error ""))
           end
       | _ -> failwith "invalid char token")

let string ?strict  s =
  let buf = Buffer.create 23 in
  let store = Buffer.add_char buf in
  let rec parse (__strm : _ XStream.t) =
    match XStream.peek __strm with
    | Some '\\' ->
        begin
          XStream.junk __strm;
          (let _ =
             try backslash_in_string (strict <> None) store __strm
             with | XStream.Failure  -> raise (XStream.Error "") in
           parse __strm)
        end
    | Some c ->
        begin
          XStream.junk __strm; (let s = __strm in begin store c; parse s end)
        end
    | _ -> Buffer.contents buf in
  parse (XStream.of_string s)