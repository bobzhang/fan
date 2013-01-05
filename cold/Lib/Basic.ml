open Format
open LibUtil
exception Unhandled of Ast.ctyp
exception Finished of Ast.expr
let _loc = FanLoc.ghost
let unit_literal = `ExId (_loc, (`IdUid (_loc, "()")))
let x ?(off= 0)  (i : int) =
  if off > 25
  then invalid_arg "unsupported offset in x "
  else
    (let base = let open Char in ((code 'a') + off) |> chr in
     (String.of_char base) ^ (string_of_int i))
let xid ?(off= 0)  (i : int) = (`IdLid (_loc, (x ~off i)) : Ast.ident )
let allx ?(off= 0)  i = "all_" ^ (x ~off i)
let allxid ?(off= 0)  i = `IdLid (_loc, (allx ~off i))
let check_valid str =
  let len = String.length str in
  if
    not
      ((len > 1) &&
         ((not (Char.is_digit (str.[1]))) &&
            (not (String.starts_with str "all_"))))
  then
    (eprintf "%s is not a valid name" str;
     eprintf
       "For valid name its length should be more than 1\ncan not be a-[digit], can not start with [all_]";
     exit 2)
  else ()
let error_report (loc,s) =
  prerr_endline (FanLoc.to_string loc);
  (let (start_bol,stop_bol,start_off,stop_off) =
     let open FanLoc in
       ((start_bol loc), (stop_bol loc), (start_off loc), (stop_off loc)) in
   let abs_start_off = start_bol + start_off in
   let abs_stop_off = stop_bol + stop_off in
   let err_location =
     String.sub s abs_start_off ((abs_stop_off - abs_start_off) + 1) in
   prerr_endline (sprintf "err: ^%s^" err_location))
let parse_string_of_entry ?(loc= FanLoc.mk "<string>")  entry s =
  try Gram.parse_string entry ~loc s
  with
  | FanLoc.Exc_located (loc,e) ->
      (eprintf "%s" (Printexc.to_string e);
       error_report (loc, s);
       FanLoc.raise loc e)
let wrap_stream_parser ?(loc= FanLoc.mk "<stream>")  p s =
  try p ~loc s
  with
  | FanLoc.Exc_located (loc,e) ->
      (eprintf "error: %s" (FanLoc.to_string loc); FanLoc.raise loc e)
let is_antiquot_data_ctor s = String.ends_with s "Ant"
let conversion_table: (string,string) Hashtbl.t = Hashtbl.create 50
