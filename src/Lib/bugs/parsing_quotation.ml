open Format;
open Camlp4.PreCast;
open Common;
open Camlp4ext;
open Astutil;

(**
   This module changes the quotation at parsing time
   to remove some bolierpolate code
   There are two problems with "default_quotation", one
   is that it's a toplevel phrase, we want its position to be
   more flexibile.

   The other is that it seems to be buggy when considering location 
 *)

eprintf "fuck yor ";
value filter = let f = fun 
  [ <:str_item@_loc< $exp: <:expr< Default_quotation  .$str:quot$.  >> $ >>  -> begin
    eprintf "quotation switched to %s" quot;
    Syntax.Quotation.default.val := quot;
     <:str_item< >>    
  end 
  |  x -> begin
      eprintf "x";
      x
  end 
  ] in (Ast.map_str_item f )#str_item
;
(**
   value filter =
  let f =
    fun
    [ Ast.StExp _loc
        (Ast.ExApp _ (Ast.ExId _ (Ast.IdUid _ "Default_quotation"))
           (Ast.ExStr _ quot))
        -> (Syntax.Quotation.default.val := "expr"; Ast.StNil _loc)
    | x -> x ]
  in (Ast.map_str_item f)#str_item;

 *)  
begin
  AstFilters.register_str_item_filter filter;
end;


















