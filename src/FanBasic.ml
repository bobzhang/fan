(*
   Basic module contains utility functions to manipulate FanAst
   This module is mainly provided to generate code. For simplicity,
   we don't take care of Location.(Should be FIXED later)
 *)
(* open Ast; *)
open LibUtil;
open Format;
exception Unhandled of Ast.ctyp ;
exception Finished of Ast.expr;
let _loc = FanLoc.ghost;
let unit_literal = {:expr| () |} ;

(* generate name *)  
let x ?(off=0) (i:int)    =
  if off > 25 then invalid_arg "unsupported offset in x "
  else
    let base = Char.(code 'a' + off |> chr) in
    String.of_char base ^ string_of_int i;
    
let xid ?(off=0) (i:int) : Ast.ident  =
  {:ident| $(lid:x ~off i) |} ;
  
let allx ?(off=0) i =  "all_" ^x ~off i ;
  
let allxid ?(off=0) i = {:ident| $(lid:allx ~off i) |};

(* check whether the introduced name is valid or not *)  
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
  

