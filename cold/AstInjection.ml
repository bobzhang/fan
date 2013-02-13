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
  Gram.extend_single (inject_expr : 'inject_expr Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid x ->\n         ((try Hashtbl.find inject_expr_tbl x\n           with | Not_found  -> failwithf \"inject.expr %s not found\" x) : \n         'inject_expr )\n     | _ ->\n         failwith\n           \"try Hashtbl.find inject_expr_tbl x\nwith | Not_found  -> failwithf \"inject.expr %s not found\" x\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid x ->
                       ((try Hashtbl.find inject_expr_tbl x
                         with
                         | Not_found  ->
                             failwithf "inject.expr %s not found" x) : 
                       'inject_expr )
                   | _ ->
                       failwith
                         "try Hashtbl.find inject_expr_tbl x\nwith | Not_found  -> failwithf \"inject.expr %s not found\" x\n"))))]));
  Gram.extend_single (inject_str_item : 'inject_str_item Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid x ->\n         ((try Hashtbl.find inject_str_item_tbl x\n           with | Not_found  -> failwithf \"inject.expr %s not found\" x) : \n         'inject_str_item )\n     | _ ->\n         failwith\n           \"try Hashtbl.find inject_str_item_tbl x\nwith | Not_found  -> failwithf \"inject.expr %s not found\" x\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid x ->
                       ((try Hashtbl.find inject_str_item_tbl x
                         with
                         | Not_found  ->
                             failwithf "inject.expr %s not found" x) : 
                       'inject_str_item )
                   | _ ->
                       failwith
                         "try Hashtbl.find inject_str_item_tbl x\nwith | Not_found  -> failwithf \"inject.expr %s not found\" x\n"))))]));
  Gram.extend_single (inject_class_str_item : 'inject_class_str_item Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid x ->\n         ((try Hashtbl.find inject_class_str_item_tbl x\n           with | Not_found  -> failwithf \"inject.expr %s not found\" x) : \n         'inject_class_str_item )\n     | _ ->\n         failwith\n           \"try Hashtbl.find inject_class_str_item_tbl x\nwith | Not_found  -> failwithf \"inject.expr %s not found\" x\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid x ->
                       ((try Hashtbl.find inject_class_str_item_tbl x
                         with
                         | Not_found  ->
                             failwithf "inject.expr %s not found" x) : 
                       'inject_class_str_item )
                   | _ ->
                       failwith
                         "try Hashtbl.find inject_class_str_item_tbl x\nwith | Not_found  -> failwithf \"inject.expr %s not found\" x\n"))))]))
let _ =
  let open AstQuotation in
    of_expr ~name:((`Absolute ["Fan"; "Inject"]), "expr") ~entry:inject_expr;
    of_str_item ~name:((`Absolute ["Fan"; "Inject"]), "str_item")
      ~entry:inject_str_item;
    of_class_str_item ~name:((`Absolute ["Fan"; "Inject"]), "class_str_item")
      ~entry:inject_class_str_item