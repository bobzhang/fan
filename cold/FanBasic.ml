open LibUtil
exception Unhandled of  Ast.ctyp 
exception Finished of  Ast.expr 
let  _loc = FanLoc.ghost
let  unit_literal = Ast.ExId ((_loc,( Ast.IdUid ((_loc,"()")) )))
let  x ?(off=0)  ((i :  int )) =
  if
  (off > 25)
  then
  begin
  (invalid_arg "unsupported offset in x ")
  end
  else
  begin
  
  let  base = let open Char in(( (( (code 'a') ) + off) ) |> chr) in
  (( (String.of_char base) ) ^ ( (string_of_int i) ))
  end
let  xid ?(off=0)  ((i :  int )) =
  (Ast.IdLid ((_loc,( (x ~off:off i) ))) : Ast.ident  )
let  allx ?(off=0)  (i) = ("all_" ^ ( (x ~off:off i) ))
let  allxid ?(off=0)  (i) = Ast.IdLid ((_loc,( (allx ~off:off i) )))
let  check_valid (str) =
  
  let  len = (String.length str) in
  if
  (not (
    (( (len > 1) ) && (
      (( (not ( (Char.is_digit ( str.[1] )) )) ) && (
        (not ( (String.starts_with str "all_") )) )) )) ))
  then
  begin
  begin
  (eprintf "%s is not a valid name" str);
  (eprintf
    "For valid name its length should be more than 1\ncan not be a-[digit], can not start with [all_]");
  (exit 2)
  end
  end
  else
  begin
  ()
  end
let  p_expr (fmt) (e) =
  (eprintf "@[%a@]@." AstPrint.expression ( (Ast2pt.expr e) ))
let  p_patt (fmt) (e) =
  (eprintf "@[%a@]@." AstPrint.pattern ( (Ast2pt.pattern e) ))
let  p_str_item (fmt) (e) =
  (eprintf "@[%a@]@." AstPrint.structure ( (Ast2pt.str_item e) ))
let  p_ctyp (fmt) (e) =
  (eprintf "@[%a@]@." AstPrint.core_type ( (Ast2pt.ctyp e) ))