open Format
open FanUtil
module Debug  = struct let mode (_) = false end
type  section =  string  
let out_channel = begin try
  let f = (Sys.getenv "CAMLP4_DEBUG_FILE") in
  (open_out_gen ( [Open_wronly;Open_creat;Open_append;Open_text] ) 438 f)
  with | Not_found  ->   Pervasives.stderr end
let mode = begin try
  let str = (Sys.getenv "CAMLP4_DEBUG") in
  let rec loop (acc) (i) = begin try
    let pos = (String.index_from str i ':') in
    (loop ( (SSet.add ( (String.sub str i ( (pos - i) )) ) acc) ) ( (pos + 1)
      ))
    with
    | Not_found  ->
      (SSet.add ( (String.sub str i ( (( (String.length str) ) - i) )) ) acc)
    end in
  let sections = (loop SSet.empty 0) in
  if (SSet.mem "*" sections) then begin (fun (_) -> true)
  end else begin (fun (x) -> (SSet.mem x sections))
  end with | Not_found  ->   (fun (_) -> false) end
let formatter =
  let header = "camlp4-debug: " in
  let at_bol = (ref true ) in
  (make_formatter (
    (fun (buf) ->
      (fun (pos) ->
        (fun (len) ->
          for i = pos to  (( (pos + len) ) - 1) do
            begin
            if at_bol.contents then begin
            (output_string out_channel header)
            end else begin ()
            end;
            let ch = buf.[i] in
            begin
              (output_char out_channel ch);
              (( at_bol.contents ) := ( (ch = '\n') ))
              end
            end done))) ) ( (fun (() ) -> (flush out_channel)) ))
let printf (section) (fmt) = (fprintf formatter ( ("%s: " ^^ fmt) ) section)