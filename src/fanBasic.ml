(*
   Basic module contains utility functions to manipulate FanAst
   This module is mainly provided to generate code. For simplicity,
   we don't take care of Location.(Should be FIXED later)
 *)

open LibUtil
open Format

let error_report (loc,s) = begin
  prerr_endline (FLoc.to_string loc);
  let (start_bol,stop_bol,
         start_off, stop_off) =
    FLoc.( (start_bol loc,
             stop_bol loc,
             start_off loc,
             stop_off loc)
           ) in
  let abs_start_off = start_bol + start_off in
  let abs_stop_off = stop_bol + stop_off in
  let err_location = String.sub s abs_start_off
      (abs_stop_off - abs_start_off + 1) in
  prerr_endline (sprintf "err: ^%s^" err_location);
end 

let parse_string_of_entry ?(loc=FLoc.mk "<string>") entry  s =
  try
    Gram.parse_string entry  ~loc s
  with
  |FLoc.Exc_located(loc, e) -> begin
      eprintf "%s" (Printexc.to_string e);
      error_report (loc,s);
      FLoc.raise loc e ;
  end

let wrap_stream_parser ?(loc=FLoc.mk "<stream>") p s =
  try p ~loc s
  with
  |FLoc.Exc_located(loc,e) -> begin
      eprintf "error: %s" (FLoc.to_string loc) ;
      FLoc.raise loc e;
    end 


(* FIXME will Ast2pt do the check, and then some partial FAst node will not be able
   to be dumped
 *)

let p_exp f  e =
  pp f "@[%a@]@." AstPrint.expression (Ast2pt.exp e)
(* let p_ident = eprintf "@[%a@]@." opr#ident ;     *)
let p_pat f e =
  pp f "@[%a@]@." AstPrint.pattern (Ast2pt.pat e)
  
let p_stru f e =
  pp f "@[%a@]@." AstPrint.structure (Ast2pt.stru e)

(* FIXME allow more interfaces later *)  
(* let p_ident f e = *)
(*   eprintf "@[%a@]@." Pprintast.fmt_longident (Ast2pt.ident e) ;     *)
let p_ctyp f e =
  pp f "@[%a@]@." AstPrint.core_type (Ast2pt.ctyp e) 
  

(* Add something to the above, make sure it ends with a slash. *)
let add_include_dir str =
  if str <> "" then
    let str =
      if String.get str ((String.length str)-1) = '/'
      then str else str ^ "/"
    in Ref.modify FConfig.include_dirs (fun x -> cons str x)
 
let parse_include_file entry =
  let dir_ok file dir = Sys.file_exists (dir ^ file) in
  fun file ->
    let file =
      try (List.find (dir_ok file) ( "./" :: !FConfig.include_dirs )) ^ file
      with | Not_found -> file  in
    let ch = open_in file in
    let st = XStream.of_channel ch in
      Gram.parse entry (FLoc.mk file) st
    
