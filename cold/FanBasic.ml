open Ast
open LibUtil
open Format
exception Unhandled of ctyp
exception Finished of expr
let _loc = FanLoc.ghost
let unit_literal = `ExId (_loc, (`Uid (_loc, "()")))
let x ?(off= 0)  (i : int) =
  if off > 25
  then invalid_arg "unsupported offset in x "
  else
    (let base = let open Char in ((code 'a') + off) |> chr in
     (String.of_char base) ^ (string_of_int i))
let xid ?(off= 0)  (i : int) = (`Lid (_loc, (x ~off i)) : ident )
let allx ?(off= 0)  i = "all_" ^ (x ~off i)
let allxid ?(off= 0)  i = `Lid (_loc, (allx ~off i))
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
let p_expr f e = pp f "@[%a@]@." AstPrint.expression (Ast2pt.expr e)
let p_patt f e = pp f "@[%a@]@." AstPrint.pattern (Ast2pt.patt e)
let p_str_item f e = pp f "@[%a@]@." AstPrint.structure (Ast2pt.str_item e)
let p_ctyp f e = pp f "@[%a@]@." AstPrint.core_type (Ast2pt.ctyp e)
