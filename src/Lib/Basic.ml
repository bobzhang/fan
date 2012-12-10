open Format;
open LibUtil;

(*
   Basic module contains utility functions to manipulate Camlp4Ast
   This module is mainly provided to generate code. For simplicity,
   we don't take care of Location.
 *)
exception Unhandled of Ast.ctyp ;
exception Finished of Ast.expr;

let _loc =FanLoc.ghost ;
  
let unit_literal = {:expr| () |} ;
  
let x ?(off=0) (i:int)    =
  if off > 25 then invalid_arg "unsupported offset in x "
  else
    let base = Char.(code 'a' + off |> chr) in
    String.of_char base ^ string_of_int i;
    
let xid ?(off=0) (i:int) : Ast.ident  =
  {:ident| $(lid:x ~off i) |} ;
  
let allx ?(off=0) i =  "all_" ^x ~off i ;
  
let allxid ?(off=0) i = {:ident| $(lid:allx ~off i) |};
  
let check_valid str =
  let len = String.length str in
  if
    not
      (len > 1 &&
       (not (Char.is_digit str.[1]))
         && (not (String.starts_with str "all_"))) then begin
           eprintf "%s is not a valid name" str;
           eprintf "For valid name its length should be more than 1\n\
             can not be a-[digit], can not start with [all_]";
           exit 2;
         end 
  else ();    

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


let parse_string_of_entry ?(_loc=FanLoc.mk "<string>") entry  s =
  try
    Gram.parse_string entry  _loc s
  with
    [FanLoc.Exc_located(loc, e) -> begin
      eprintf "%s" (Printexc.to_string e);
      error_report (loc,s);
      FanLoc.raise loc e ;
    end ];

let wrap_stream_parser ?(_loc=FanLoc.mk "<stream>") p s =
  try p _loc s
  with
    [FanLoc.Exc_located(loc,e) -> begin
      eprintf "error: %s" (FanLoc.to_string loc) ;
      FanLoc.raise loc e;
    end 
   ];


let is_antiquot_data_ctor s = String.ends_with s "Ant";
    
