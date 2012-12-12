type key = string 
type expander = Ast.expr -> Ast.expr 
let macro_expanders: (key,expander) Hashtbl.t = Hashtbl.create 40
let register_macro (k,f) = Hashtbl.replace macro_expanders k f
let rec fib =
  function
  | 0|1 -> 1
  | n when n > 0 -> (fib (n - 1)) + (fib (n - 2))
  | _ -> invalid_arg "fib"
let fibm y =
  match y with
  | Ast.ExInt (_loc,x) ->
      Ast.ExInt (_loc, (string_of_int (fib (int_of_string x))))
  | x ->
      let _loc = Camlp4Ast.loc_of_expr x in
      Ast.ExApp (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "fib")))), x)
let _ = register_macro ("FIB", fibm)
open LibUtil
let generate_fibs =
  function
  | Ast.ExInt (_loc,x) ->
      let j = int_of_string x in
      let res =
        zfold_left ~until:j ~acc:(Ast.ExNil _loc)
          (fun acc  i  ->
             Ast.ExSem
               (_loc, acc,
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExId (_loc, (Ast.IdLid (_loc, "print_int")))),
                      (Ast.ExApp
                         (_loc, (Ast.ExId (_loc, (Ast.IdUid (_loc, "FIB")))),
                           (Ast.ExInt (_loc, (string_of_int i))))))))) in
      Ast.ExSeq (_loc, res)
  | e -> e
let _ = register_macro ("GFIB", generate_fibs)