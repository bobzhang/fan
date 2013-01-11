open Ast
open LibUtil
type key = string 
let inject_expr_tbl: (key,expr) Hashtbl.t = Hashtbl.create 40
let inject_str_item_tbl: (key,str_item) Hashtbl.t = Hashtbl.create 40
let inject_class_str_item_tbl: (key,class_str_item) Hashtbl.t =
  Hashtbl.create 40
let register_inject_expr (k,f) = Hashtbl.replace inject_expr_tbl k f
let register_inject_str_item (k,f) = Hashtbl.replace inject_str_item_tbl k f
let register_inject_class_str_item (k,f) =
  Hashtbl.replace inject_class_str_item_tbl k f
let inject_expr = Gram.mk "inject_expr"
let inject_str_item = Gram.mk "inject_str_item"
let inject_class_str_item = Gram.mk "inject_class_str_item"
let _ =
  Gram.extend (inject_expr : 'inject_expr Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `Lid _ -> true | _ -> false)),
                (`Normal, "`Lid _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Lid x ->
                      ((try Hashtbl.find inject_expr_tbl x
                        with
                        | Not_found  ->
                            failwithf "inject.expr %s not found" x) : 
                      'inject_expr )
                  | _ -> assert false)))])]);
  Gram.extend (inject_str_item : 'inject_str_item Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `Lid _ -> true | _ -> false)),
                (`Normal, "`Lid _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Lid x ->
                      ((try Hashtbl.find inject_str_item_tbl x
                        with
                        | Not_found  ->
                            failwithf "inject.expr %s not found" x) : 
                      'inject_str_item )
                  | _ -> assert false)))])]);
  Gram.extend (inject_class_str_item : 'inject_class_str_item Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `Lid _ -> true | _ -> false)),
                (`Normal, "`Lid _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Lid x ->
                      ((try Hashtbl.find inject_class_str_item_tbl x
                        with
                        | Not_found  ->
                            failwithf "inject.expr %s not found" x) : 
                      'inject_class_str_item )
                  | _ -> assert false)))])])
let _ =
  let open AstQuotation in
    of_expr ~name:"inject.expr" ~entry:inject_expr;
    of_str_item ~name:"inject.str_item" ~entry:inject_str_item;
    of_class_str_item ~name:"inject.class_str_item"
      ~entry:inject_class_str_item