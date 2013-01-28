open Format;
open LibUtil;
open Ast;

(*
   Basic module contains utility functions to manipulate Ast
   This module is mainly provided to generate code. For simplicity,
   we don't take care of Location.
 *)
exception Unhandled of ctyp ;
exception Finished of expr;

let _loc =FanLoc.ghost ;

let unit_literal = {:expr| () |} ;
  
let x ?(off=0) (i:int)    =
  if off > 25 then invalid_arg "unsupported offset in x "
  else
    let base = Char.(code 'a' + off |> chr) in
    "_"^String.of_char base ^ string_of_int i;
    
let xid ?(off=0) (i:int)   =
  {:ident| $(lid:x ~off i) |} ;
  
let allx ?(off=0) i =  "all" ^x ~off i ;
  
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

  




let conversion_table : Hashtbl.t string string = Hashtbl.create 50;
