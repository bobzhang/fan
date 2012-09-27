open LibUtil

exception Unhandled of Ast.ctyp

exception Finished of Ast.expr


let _loc = FanLoc.ghost

let unit_literal =
                          (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "()")) )))


let x =
 fun ?(off = 0) ->
  fun (i :
    int) ->
   if (off > 25) then ( (invalid_arg "unsupported offset in x ") )
   else
    let base = let open Char in (( (( (code 'a') ) + off) ) |> chr) in
    (( (String.of_char base) ) ^ ( (string_of_int i) ))

let xid =
                                                          fun ?(off = 0) ->
                                                           fun (i :
                                                             int) ->
                                                            ((Ast.IdLid
                                                               (_loc, (
                                                                (x ~off:off
                                                                  i) ))) :
                                                              Ast.ident)


let allx = fun ?(off = 0) -> fun i -> ("all_" ^ ( (x ~off:off i) ))


let allxid =
 fun ?(off = 0) -> fun i -> (Ast.IdLid (_loc, ( (allx ~off:off i) )))


let check_valid =
 fun str ->
  let len = (String.length str) in
  if (not (
       (( (len > 1) ) && (
         (( (not ( (Char.is_digit ( (String.get str 1) )) )) ) && (
           (not ( (String.starts_with str "all_") )) )) )) ))
  then
   begin
   (
   (eprintf "%s is not a valid name" str)
   );
   (
   (eprintf
     "For valid name its length should be more than 1\ncan not be a-[digit], can not start with [all_]")
   );
   (exit 2)
  end else ()

let p_expr =
                fun fmt ->
                 fun e ->
                  (eprintf "@[%a@]@." Pprintast.print_expression (
                    (Ast2pt.expr e) ))

let p_patt =
                                         fun fmt ->
                                          fun e ->
                                           (eprintf "@[%a@]@."
                                             Pprintast.print_pattern (
                                             (Ast2pt.pattern e) ))

let p_str_item =
                                                                    fun fmt ->
                                                                    fun e ->
                                                                    (eprintf
                                                                    "@[%a@]@."
                                                                    Pprintast.print_structure
                                                                    (
                                                                    (Ast2pt.str_item
                                                                    e) ))


let p_ident =
 fun fmt ->
  fun e -> (eprintf "@[%a@]@." Pprintast.fmt_longident ( (Ast2pt.ident e) ))


let p_ctyp =
 fun fmt ->
  fun e -> (eprintf "@[%a@]@." Pprintast.core_type ( (Ast2pt.ctyp e) ))
