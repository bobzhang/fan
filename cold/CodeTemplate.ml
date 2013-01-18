open Ast
open LibUtil
let _loc = FanLoc.ghost
type ty_meta = 
  {
  str: string;
  print: [ `Exist | `Custom of str_item | `Fmt of string];
  eq: [ `Def | `Custom of str_item]} 
let base1_types =
  [("int", `Exist, `Def);
  ("int32", (`Fmt "%ld"), `Def);
  ("int64", (`Fmt "%Ld"), `Def);
  ("nativeint", (`Fmt "%nd"), `Def);
  ("float", `Exist, `Def);
  ("string", (`Fmt "%S"), `Def);
  ("bool", `Exist, `Def);
  ("char", `Exist, `Def);
  ("unit",
    (`Custom
       (`Value
          (_loc, (`ReNil _loc),
            (`Bind
               (_loc, (`Id (_loc, (`Lid (_loc, "pp_print_unit")))),
                 (`Constraint_exp
                    (_loc,
                      (`Fun
                         (_loc,
                           (`Case
                              (_loc, (`Id (_loc, (`Lid (_loc, "fmt")))),
                                (`Nil _loc),
                                (`Fun
                                   (_loc,
                                     (`Case
                                        (_loc, (`Any _loc), (`Nil _loc),
                                          (`ExApp
                                             (_loc,
                                               (`ExApp
                                                  (_loc,
                                                    (`Id
                                                       (_loc,
                                                         (`IdAcc
                                                            (_loc,
                                                              (`Uid
                                                                 (_loc,
                                                                   "Format")),
                                                              (`Lid
                                                                 (_loc,
                                                                   "fprintf")))))),
                                                    (`Id
                                                       (_loc,
                                                         (`Lid (_loc, "fmt")))))),
                                               (`Str (_loc, "()")))))))))))),
                      (`Arrow
                         (_loc,
                           (`Id
                              (_loc,
                                (`IdAcc
                                   (_loc, (`Uid (_loc, "Format")),
                                     (`Lid (_loc, "formatter")))))),
                           (`Arrow
                              (_loc, (`Id (_loc, (`Lid (_loc, "unit")))),
                                (`Id (_loc, (`Lid (_loc, "unit"))))))))))))))),
    (`Custom
       (`Value
          (_loc, (`ReNil _loc),
            (`Bind
               (_loc, (`Id (_loc, (`Lid (_loc, "eq_unit")))),
                 (`Constraint_exp
                    (_loc,
                      (`Fun
                         (_loc,
                           (`Case
                              (_loc, (`Any _loc), (`Nil _loc),
                                (`Fun
                                   (_loc,
                                     (`Case
                                        (_loc, (`Any _loc), (`Nil _loc),
                                          (`Id (_loc, (`Lid (_loc, "true")))))))))))),
                      (`Arrow
                         (_loc, (`Id (_loc, (`Lid (_loc, "unit")))),
                           (`Arrow
                              (_loc, (`Id (_loc, (`Lid (_loc, "unit")))),
                                (`Id (_loc, (`Lid (_loc, "bool"))))))))))))))))]
let ty_metas =
  base1_types |> (List.map (fun (str,print,eq)  -> { str; print; eq }))
let print_base1 =
  let items =
    ty_metas |>
      (List.map
         (fun { str; print;_}  ->
            let ty =
              `Arrow
                (_loc,
                  (`Id
                     (_loc,
                       (`IdAcc
                          (_loc, (`Uid (_loc, "Format")),
                            (`Lid (_loc, "formatter")))))),
                  (`Arrow
                     (_loc, (`Id (_loc, (`Lid (_loc, str)))),
                       (`Id (_loc, (`Lid (_loc, "unit"))))))) in
            let name = "pp_print_" ^ str in
            match print with
            | `Exist ->
                `Value
                  (_loc, (`ReNil _loc),
                    (`Bind
                       (_loc, (`Id (_loc, (`Lid (_loc, name)))),
                         (`Id (_loc, (`Lid (_loc, name)))))))
            | `Custom s -> s
            | `Fmt c ->
                `Value
                  (_loc, (`ReNil _loc),
                    (`Bind
                       (_loc, (`Id (_loc, (`Lid (_loc, name)))),
                         (`Constraint_exp
                            (_loc,
                              (`Fun
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`Id (_loc, (`Lid (_loc, "fmt")))),
                                        (`Nil _loc),
                                        (`Fun
                                           (_loc,
                                             (`Case
                                                (_loc,
                                                  (`Id
                                                     (_loc,
                                                       (`Lid (_loc, "a")))),
                                                  (`Nil _loc),
                                                  (`ExApp
                                                     (_loc,
                                                       (`ExApp
                                                          (_loc,
                                                            (`ExApp
                                                               (_loc,
                                                                 (`Id
                                                                    (_loc,
                                                                    (`IdAcc
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Format")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "fprintf")))))),
                                                                 (`Id
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "fmt")))))),
                                                            (`Str (_loc, c)))),
                                                       (`Id
                                                          (_loc,
                                                            (`Lid (_loc, "a")))))))))))))),
                              ty))))))) in
  FanAst.sem_of_list items
let (map_class_str_item_base_1,map_class_str_item_base_2,fold_class_str_item_base_1,fold_class_str_item_base_2,print_class_str_item_base,iter_class_str_item_base_1,eq_class_str_item_base_2)
  =
  let ty_names = ty_metas |> (List.map (fun { str;_}  -> str)) in
  let v1 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              `Arrow
                (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                  (`Id (_loc, (`Lid (_loc, x))))) in
            let exp =
              `Fun
                (_loc,
                  (`Case
                     (_loc, (`Id (_loc, (`Lid (_loc, "x")))), (`Nil _loc),
                       (`Id (_loc, (`Lid (_loc, "x"))))))) in
            `CrMth
              (_loc, (`Lid (_loc, x)), (`OvNil _loc), (`PrNil _loc), exp, ty))) in
  let v2 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              `Arrow
                (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                  (`Arrow
                     (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                       (`Id (_loc, (`Lid (_loc, x))))))) in
            let exp =
              `Fun
                (_loc,
                  (`Case
                     (_loc, (`Id (_loc, (`Lid (_loc, "x")))), (`Nil _loc),
                       (`Fun
                          (_loc,
                            (`Case
                               (_loc, (`Any _loc), (`Nil _loc),
                                 (`Id (_loc, (`Lid (_loc, "x"))))))))))) in
            `CrMth
              (_loc, (`Lid (_loc, x)), (`OvNil _loc), (`PrNil _loc), exp, ty))) in
  let v3 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              `Arrow
                (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                  (`Quote
                     (_loc, (`Normal _loc),
                       (`Some (`Lid (_loc, "self_type")))))) in
            let exp =
              `Fun
                (_loc,
                  (`Case
                     (_loc, (`Any _loc), (`Nil _loc),
                       (`Id (_loc, (`Lid (_loc, "self"))))))) in
            `CrMth
              (_loc, (`Lid (_loc, x)), (`OvNil _loc), (`PrNil _loc), exp, ty))) in
  let v4 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              `Arrow
                (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                  (`Arrow
                     (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                       (`Quote
                          (_loc, (`Normal _loc),
                            (`Some (`Lid (_loc, "self_type")))))))) in
            let exp =
              `Fun
                (_loc,
                  (`Case
                     (_loc, (`Any _loc), (`Nil _loc),
                       (`Fun
                          (_loc,
                            (`Case
                               (_loc, (`Any _loc), (`Nil _loc),
                                 (`Id (_loc, (`Lid (_loc, "self"))))))))))) in
            `CrMth
              (_loc, (`Lid (_loc, x)), (`OvNil _loc), (`PrNil _loc), exp, ty))) in
  let v5 =
    ty_names |>
      (List.map
         (fun x  ->
            let exp = `Id (_loc, (`Lid (_loc, ("pp_print_" ^ x)))) in
            let ty = `Nil _loc in
            `CrMth
              (_loc, (`Lid (_loc, x)), (`OvNil _loc), (`PrNil _loc), exp, ty))) in
  let v6 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              `Arrow
                (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                  (`Id (_loc, (`Lid (_loc, "unit"))))) in
            let exp =
              `Fun
                (_loc,
                  (`Case
                     (_loc, (`Any _loc), (`Nil _loc),
                       (`Id (_loc, (`Uid (_loc, "()"))))))) in
            `CrMth
              (_loc, (`Lid (_loc, x)), (`OvNil _loc), (`PrNil _loc), exp, ty))) in
  let v7 =
    ty_names |>
      (List.map
         (fun x  ->
            let exp =
              `Fun
                (_loc,
                  (`Case
                     (_loc, (`Id (_loc, (`Lid (_loc, "x")))), (`Nil _loc),
                       (`Fun
                          (_loc,
                            (`Case
                               (_loc, (`Id (_loc, (`Lid (_loc, "y")))),
                                 (`Nil _loc),
                                 (`ExApp
                                    (_loc,
                                      (`ExApp
                                         (_loc,
                                           (`Id (_loc, (`Lid (_loc, "=")))),
                                           (`Id (_loc, (`Lid (_loc, "x")))))),
                                      (`Id (_loc, (`Lid (_loc, "y"))))))))))))) in
            let ty =
              `Arrow
                (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                  (`Arrow
                     (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                       (`Id (_loc, (`Lid (_loc, "bool"))))))) in
            `CrMth
              (_loc, (`Lid (_loc, x)), (`OvNil _loc), (`PrNil _loc), exp, ty))) in
  ((FanAst.sem_of_list v1), (FanAst.sem_of_list v2), (FanAst.sem_of_list v3),
    (FanAst.sem_of_list v4), (FanAst.sem_of_list v5),
    (FanAst.sem_of_list v6), (FanAst.sem_of_list v7))
let eq_base1 =
  let items =
    ty_metas |>
      (List.map
         (fun { str; eq;_}  ->
            let ty =
              `Arrow
                (_loc, (`Id (_loc, (`Lid (_loc, str)))),
                  (`Arrow
                     (_loc, (`Id (_loc, (`Lid (_loc, str)))),
                       (`Id (_loc, (`Lid (_loc, "bool"))))))) in
            let name = "eq_" ^ str in
            match eq with
            | `Def ->
                `Value
                  (_loc, (`ReNil _loc),
                    (`Bind
                       (_loc, (`Id (_loc, (`Lid (_loc, name)))),
                         (`Constraint_exp
                            (_loc, (`Id (_loc, (`Lid (_loc, "=")))), ty)))))
            | `Custom s -> s)) in
  FanAst.sem_of_list items
let _ =
  let open AstInjection in
    register_inject_class_str_item
      ("map_class_str_item_base_1", map_class_str_item_base_1);
    register_inject_class_str_item
      ("map_class_str_item_base_2", map_class_str_item_base_2);
    register_inject_class_str_item
      ("fold_class_str_item_base_1", fold_class_str_item_base_1);
    register_inject_class_str_item
      ("fold_class_str_item_base_2", fold_class_str_item_base_2);
    register_inject_class_str_item
      ("print_class_str_item_base", print_class_str_item_base);
    register_inject_class_str_item
      ("iter_class_str_item_base_1", iter_class_str_item_base_1);
    register_inject_class_str_item
      ("eq_class_str_item_base_2", eq_class_str_item_base_2);
    register_inject_str_item ("eq_base1", eq_base1);
    register_inject_str_item ("print_base1", print_base1)