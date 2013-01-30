(*
   Basic module contains utility functions to manipulate FanAst
   This module is mainly provided to generate code. For simplicity,
   we don't take care of Location.(Should be FIXED later)
 *)
(* open Ast; *)
open LibUtil;
open Format;

let error_report (loc,s) = begin
  prerr_endline (FanLoc.to_string loc);
  let (start_bol,stop_bol,
         start_off, stop_off) =
    FanLoc.( (start_bol loc,
             stop_bol loc,
             start_off loc,
             stop_off loc)
           ) in
  let abs_start_off = start_bol + start_off in
  let abs_stop_off = stop_bol + stop_off in
  let err_location = String.sub s abs_start_off
      (abs_stop_off - abs_start_off + 1) in
  prerr_endline (sprintf "err: ^%s^" err_location);
end ;

let parse_string_of_entry ?(loc=FanLoc.mk "<string>") entry  s =
  try
    Gram.parse_string entry  ~loc s
  with
    [FanLoc.Exc_located(loc, e) -> begin
      eprintf "%s" (Printexc.to_string e);
      error_report (loc,s);
      FanLoc.raise loc e ;
    end ];

let wrap_stream_parser ?(loc=FanLoc.mk "<stream>") p s =
  try p ~loc s
  with
    [FanLoc.Exc_located(loc,e) -> begin
      eprintf "error: %s" (FanLoc.to_string loc) ;
      FanLoc.raise loc e;
    end 
   ];


(* FIXME will Ast2pt do the check, and then some partial Ast node will not be able
   to be dumped
 *)

let p_expr f  e =
  pp f "@[%a@]@." AstPrint.expression (Ast2pt.expr e);
(* let p_ident = eprintf "@[%a@]@." opr#ident ;     *)
let p_patt f e =
  pp f "@[%a@]@." AstPrint.pattern (Ast2pt.patt e);
  
let p_str_item f e =
  pp f "@[%a@]@." AstPrint.structure (Ast2pt.str_item e);

(* FIXME allow more interfaces later *)  
(* let p_ident f e = *)
(*   eprintf "@[%a@]@." Pprintast.fmt_longident (Ast2pt.ident e) ;     *)
let p_ctyp f e =
  pp f "@[%a@]@." AstPrint.core_type (Ast2pt.ctyp e) ;
  

(* Add something to the above, make sure it ends with a slash. *)
let add_include_dir str =
  if str <> "" then
    let str =
      if String.get str ((String.length str)-1) = '/'
      then str else str ^ "/"
    in Ref.modify FanConfig.include_dirs (fun x -> cons str x);
 
let parse_include_file entry =
  let dir_ok file dir = Sys.file_exists (dir ^ file) in
  fun file ->
    let file =
      try (List.find (dir_ok file) ( ["./" :: !FanConfig.include_dirs] )) ^ file
      with [ Not_found -> file ] in
    let ch = open_in file in
    let st = XStream.of_channel ch in
      Gram.parse entry (FanLoc.mk file) st;

(* let parse_include_file rule file  = *)
(*   if Sys.file_exists file then *)
(*     let ch = open_in file in *)
(*     let st = XStream.of_channel ch in  *)
(*     Gram.parse rule (FanLoc.mk file) st *)
(*   else  failwithf "@[file: %s not found@]@." file; *)
    
(* (\* to be exported   *\) *)
(* let parse_include_file_smart file = let open Filename in *)
(*   if check_suffix file ".ml" then *)
(*     `Str (GramLib.parse_include_file str_items file) *)
(*   else if check_suffix file ".mli" then *)
(*     `Sig (GramLib.parse_include_file sig_items file) *)
(*   else begin *)
(*   eprintf "file input should ends with either .ml or .mli"; *)
(*   invalid_arg ("parse_include_file_smart: " ^ file ) *)
(* end; *)

(* let parse_module_type str = *)
(*   try *)
(*     match  Gram.parse_string module_type  str with *)
(*     [ {:module_type| $id:i |}  -> i *)
(*     | _ -> begin *)
(*         eprintf "the module type %s is not a simple module type" str; *)
(*         exit 2; *)
(*     end ] *)
(*   with *)
(*     [ _ -> begin *)
(*       eprintf "%s is not a valid module_type" str; *)
(*       exit 2; *)
(*     end]; *)

    
