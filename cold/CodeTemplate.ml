open Ast
module Ast = FanAst
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
       (`StVal
          (_loc, `ReNil,
            (`BiEq
               (_loc, (`PaId (_loc, (`Lid (_loc, "pp_print_unit")))),
                 (`ExTyc
                    (_loc,
                      (`ExFun
                         (_loc,
                           (`McArr
                              (_loc, (`PaId (_loc, (`Lid (_loc, "fmt")))),
                                (`ExNil _loc),
                                (`ExFun
                                   (_loc,
                                     (`McArr
                                        (_loc, (`PaAny _loc), (`ExNil _loc),
                                          (`ExApp
                                             (_loc,
                                               (`ExApp
                                                  (_loc,
                                                    (`ExId
                                                       (_loc,
                                                         (`IdAcc
                                                            (_loc,
                                                              (`Uid
                                                                 (_loc,
                                                                   "Format")),
                                                              (`Lid
                                                                 (_loc,
                                                                   "fprintf")))))),
                                                    (`ExId
                                                       (_loc,
                                                         (`Lid
                                                            (_loc, "fmt")))))),
                                               (`ExStr (_loc, "()")))))))))))),
                      (`TyArr
                         (_loc,
                           (`TyId
                              (_loc,
                                (`IdAcc
                                   (_loc, (`Uid (_loc, "Format")),
                                     (`Lid (_loc, "formatter")))))),
                           (`TyArr
                              (_loc, (`TyId (_loc, (`Lid (_loc, "unit")))),
                                (`TyId (_loc, (`Lid (_loc, "unit"))))))))))))))),
    (`Custom
       (`StVal
          (_loc, `ReNil,
            (`BiEq
               (_loc, (`PaId (_loc, (`Lid (_loc, "eq_unit")))),
                 (`ExTyc
                    (_loc,
                      (`ExFun
                         (_loc,
                           (`McArr
                              (_loc, (`PaAny _loc), (`ExNil _loc),
                                (`ExFun
                                   (_loc,
                                     (`McArr
                                        (_loc, (`PaAny _loc), (`ExNil _loc),
                                          (`ExId
                                             (_loc, (`Lid (_loc, "true")))))))))))),
                      (`TyArr
                         (_loc, (`TyId (_loc, (`Lid (_loc, "unit")))),
                           (`TyArr
                              (_loc, (`TyId (_loc, (`Lid (_loc, "unit")))),
                                (`TyId (_loc, (`Lid (_loc, "bool"))))))))))))))))]
let ty_metas =
  base1_types |> (List.map (fun (str,print,eq)  -> { str; print; eq }))
let print_base1 =
  let items =
    ty_metas |>
      (List.map
         (fun { str; print;_}  ->
            let ty =
              `TyArr
                (_loc,
                  (`TyId
                     (_loc,
                       (`IdAcc
                          (_loc, (`Uid (_loc, "Format")),
                            (`Lid (_loc, "formatter")))))),
                  (`TyArr
                     (_loc, (`TyId (_loc, (`Lid (_loc, str)))),
                       (`TyId (_loc, (`Lid (_loc, "unit"))))))) in
            let name = "pp_print_" ^ str in
            match print with
            | `Exist ->
                `StVal
                  (_loc, `ReNil,
                    (`BiEq
                       (_loc, (`PaId (_loc, (`Lid (_loc, name)))),
                         (`ExId (_loc, (`Lid (_loc, name)))))))
            | `Custom s -> s
            | `Fmt c ->
                `StVal
                  (_loc, `ReNil,
                    (`BiEq
                       (_loc, (`PaId (_loc, (`Lid (_loc, name)))),
                         (`ExTyc
                            (_loc,
                              (`ExFun
                                 (_loc,
                                   (`McArr
                                      (_loc,
                                        (`PaId (_loc, (`Lid (_loc, "fmt")))),
                                        (`ExNil _loc),
                                        (`ExFun
                                           (_loc,
                                             (`McArr
                                                (_loc,
                                                  (`PaId
                                                     (_loc,
                                                       (`Lid (_loc, "a")))),
                                                  (`ExNil _loc),
                                                  (`ExApp
                                                     (_loc,
                                                       (`ExApp
                                                          (_loc,
                                                            (`ExApp
                                                               (_loc,
                                                                 (`ExId
                                                                    (_loc,
                                                                    (`IdAcc
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Format")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "fprintf")))))),
                                                                 (`ExId
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "fmt")))))),
                                                            (`ExStr (_loc, c)))),
                                                       (`ExId
                                                          (_loc,
                                                            (`Lid
                                                               (_loc, "a")))))))))))))),
                              ty))))))) in
  FanAst.stSem_of_list items
let (map_class_str_item_base_1,map_class_str_item_base_2,fold_class_str_item_base_1,fold_class_str_item_base_2,print_class_str_item_base)
  =
  let ty_names = ty_metas |> (List.map (fun { str;_}  -> str)) in
  let v1 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              `TyArr
                (_loc, (`TyId (_loc, (`Lid (_loc, x)))),
                  (`TyId (_loc, (`Lid (_loc, x))))) in
            `CrMth
              (_loc, x, `OvNil, `PrNil,
                (`ExFun
                   (_loc,
                     (`McArr
                        (_loc, (`PaId (_loc, (`Lid (_loc, "x")))),
                          (`ExNil _loc),
                          (`ExId (_loc, (`Lid (_loc, "x")))))))), ty))) in
  let v2 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              `TyArr
                (_loc, (`TyId (_loc, (`Lid (_loc, x)))),
                  (`TyArr
                     (_loc, (`TyId (_loc, (`Lid (_loc, x)))),
                       (`TyId (_loc, (`Lid (_loc, x))))))) in
            `CrMth
              (_loc, x, `OvNil, `PrNil,
                (`ExFun
                   (_loc,
                     (`McArr
                        (_loc, (`PaId (_loc, (`Lid (_loc, "x")))),
                          (`ExNil _loc),
                          (`ExFun
                             (_loc,
                               (`McArr
                                  (_loc, (`PaAny _loc), (`ExNil _loc),
                                    (`ExId (_loc, (`Lid (_loc, "x")))))))))))),
                ty))) in
  let v3 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              `TyArr
                (_loc, (`TyId (_loc, (`Lid (_loc, x)))),
                  (`TyQuo (_loc, "self_type"))) in
            `CrMth
              (_loc, x, `OvNil, `PrNil,
                (`ExFun
                   (_loc,
                     (`McArr
                        (_loc, (`PaAny _loc), (`ExNil _loc),
                          (`ExId (_loc, (`Lid (_loc, "self")))))))), ty))) in
  let v4 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              `TyArr
                (_loc, (`TyId (_loc, (`Lid (_loc, x)))),
                  (`TyArr
                     (_loc, (`TyId (_loc, (`Lid (_loc, x)))),
                       (`TyQuo (_loc, "self_type"))))) in
            `CrMth
              (_loc, x, `OvNil, `PrNil,
                (`ExFun
                   (_loc,
                     (`McArr
                        (_loc, (`PaAny _loc), (`ExNil _loc),
                          (`ExFun
                             (_loc,
                               (`McArr
                                  (_loc, (`PaAny _loc), (`ExNil _loc),
                                    (`ExId (_loc, (`Lid (_loc, "self")))))))))))),
                ty))) in
  let v5 =
    ty_names |>
      (List.map
         (fun x  ->
            `CrMth
              (_loc, x, `OvNil, `PrNil,
                (`ExId (_loc, (`Lid (_loc, ("pp_print_" ^ x))))),
                (`TyNil _loc)))) in
  ((FanAst.crSem_of_list v1), (FanAst.crSem_of_list v2),
    (FanAst.crSem_of_list v3), (FanAst.crSem_of_list v4),
    (FanAst.crSem_of_list v5))
let eq_base1 =
  let items =
    ty_metas |>
      (List.map
         (fun { str; eq;_}  ->
            let ty =
              `TyArr
                (_loc, (`TyId (_loc, (`Lid (_loc, str)))),
                  (`TyArr
                     (_loc, (`TyId (_loc, (`Lid (_loc, str)))),
                       (`TyId (_loc, (`Lid (_loc, "bool"))))))) in
            let name = "eq_" ^ str in
            match eq with
            | `Def ->
                `StVal
                  (_loc, `ReNil,
                    (`BiEq
                       (_loc, (`PaId (_loc, (`Lid (_loc, name)))),
                         (`ExTyc
                            (_loc, (`ExId (_loc, (`Lid (_loc, "=")))), ty)))))
            | `Custom s -> s)) in
  FanAst.stSem_of_list items
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
    register_inject_str_item ("eq_base1", eq_base1);
    register_inject_str_item ("print_base1", print_base1)
