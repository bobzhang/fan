module Ast = Camlp4Ast
open LibUtil
let _loc = FanLoc.ghost
type ty_meta = 
  {
  str: string;
  print: [ `Exist | `Custom of Ast.str_item | `Fmt of string];
  eq: [ `Def | `Custom of Ast.str_item]} 
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
       (Ast.StVal
          (_loc, Ast.ReNil,
            (Ast.BiEq
               (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "pp_print_unit")))),
                 (Ast.ExTyc
                    (_loc,
                      (Ast.ExFun
                         (_loc,
                           (Ast.McArr
                              (_loc,
                                (Ast.PaId (_loc, (Ast.IdLid (_loc, "fmt")))),
                                (Ast.ExNil _loc),
                                (Ast.ExFun
                                   (_loc,
                                     (Ast.McArr
                                        (_loc, (Ast.PaAny _loc),
                                          (Ast.ExNil _loc),
                                          (Ast.ExApp
                                             (_loc,
                                               (Ast.ExApp
                                                  (_loc,
                                                    (Ast.ExId
                                                       (_loc,
                                                         (Ast.IdAcc
                                                            (_loc,
                                                              (Ast.IdUid
                                                                 (_loc,
                                                                   "Format")),
                                                              (Ast.IdLid
                                                                 (_loc,
                                                                   "fprintf")))))),
                                                    (Ast.ExId
                                                       (_loc,
                                                         (Ast.IdLid
                                                            (_loc, "fmt")))))),
                                               (Ast.ExStr (_loc, "()")))))))))))),
                      (Ast.TyArr
                         (_loc,
                           (Ast.TyId
                              (_loc,
                                (Ast.IdAcc
                                   (_loc, (Ast.IdUid (_loc, "Format")),
                                     (Ast.IdLid (_loc, "formatter")))))),
                           (Ast.TyArr
                              (_loc,
                                (Ast.TyId (_loc, (Ast.IdLid (_loc, "unit")))),
                                (Ast.TyId (_loc, (Ast.IdLid (_loc, "unit"))))))))))))))),
    (`Custom
       (Ast.StVal
          (_loc, Ast.ReNil,
            (Ast.BiEq
               (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "eq_unit")))),
                 (Ast.ExTyc
                    (_loc,
                      (Ast.ExFun
                         (_loc,
                           (Ast.McArr
                              (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                                (Ast.ExFun
                                   (_loc,
                                     (Ast.McArr
                                        (_loc, (Ast.PaAny _loc),
                                          (Ast.ExNil _loc),
                                          (Ast.ExId
                                             (_loc,
                                               (Ast.IdLid (_loc, "true")))))))))))),
                      (Ast.TyArr
                         (_loc,
                           (Ast.TyId (_loc, (Ast.IdLid (_loc, "unit")))),
                           (Ast.TyArr
                              (_loc,
                                (Ast.TyId (_loc, (Ast.IdLid (_loc, "unit")))),
                                (Ast.TyId (_loc, (Ast.IdLid (_loc, "bool"))))))))))))))))]
let ty_metas =
  base1_types |> (List.map (fun (str,print,eq)  -> { str; print; eq }))
let print_base1 =
  let items =
    ty_metas |>
      (List.map
         (fun { str; print;_}  ->
            let ty =
              Ast.TyArr
                (_loc,
                  (Ast.TyId
                     (_loc,
                       (Ast.IdAcc
                          (_loc, (Ast.IdUid (_loc, "Format")),
                            (Ast.IdLid (_loc, "formatter")))))),
                  (Ast.TyArr
                     (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, str)))),
                       (Ast.TyId (_loc, (Ast.IdLid (_loc, "unit"))))))) in
            let name = "pp_print_" ^ str in
            match print with
            | `Exist ->
                Ast.StVal
                  (_loc, Ast.ReNil,
                    (Ast.BiEq
                       (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, name)))),
                         (Ast.ExId (_loc, (Ast.IdLid (_loc, name)))))))
            | `Custom s -> s
            | `Fmt c ->
                Ast.StVal
                  (_loc, Ast.ReNil,
                    (Ast.BiEq
                       (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, name)))),
                         (Ast.ExTyc
                            (_loc,
                              (Ast.ExFun
                                 (_loc,
                                   (Ast.McArr
                                      (_loc,
                                        (Ast.PaId
                                           (_loc, (Ast.IdLid (_loc, "fmt")))),
                                        (Ast.ExNil _loc),
                                        (Ast.ExFun
                                           (_loc,
                                             (Ast.McArr
                                                (_loc,
                                                  (Ast.PaId
                                                     (_loc,
                                                       (Ast.IdLid (_loc, "a")))),
                                                  (Ast.ExNil _loc),
                                                  (Ast.ExApp
                                                     (_loc,
                                                       (Ast.ExApp
                                                          (_loc,
                                                            (Ast.ExApp
                                                               (_loc,
                                                                 (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdAcc
                                                                    (_loc,
                                                                    (Ast.IdUid
                                                                    (_loc,
                                                                    "Format")),
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "fprintf")))))),
                                                                 (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "fmt")))))),
                                                            (Ast.ExStr
                                                               (_loc, c)))),
                                                       (Ast.ExId
                                                          (_loc,
                                                            (Ast.IdLid
                                                               (_loc, "a")))))))))))))),
                              ty))))))) in
  Ast.stSem_of_list items
let (map_class_str_item_base_1,map_class_str_item_base_2,fold_class_str_item_base_1,fold_class_str_item_base_2,print_class_str_item_base)
  =
  let ty_names = ty_metas |> (List.map (fun { str;_}  -> str)) in
  let v1 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              Ast.TyArr
                (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, x)))),
                  (Ast.TyId (_loc, (Ast.IdLid (_loc, x))))) in
            Ast.CrMth
              (_loc, x, Ast.OvNil, Ast.PrNil,
                (Ast.ExFun
                   (_loc,
                     (Ast.McArr
                        (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "x")))),
                          (Ast.ExNil _loc),
                          (Ast.ExId (_loc, (Ast.IdLid (_loc, "x")))))))), ty))) in
  let v2 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              Ast.TyArr
                (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, x)))),
                  (Ast.TyArr
                     (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, x)))),
                       (Ast.TyId (_loc, (Ast.IdLid (_loc, x))))))) in
            Ast.CrMth
              (_loc, x, Ast.OvNil, Ast.PrNil,
                (Ast.ExFun
                   (_loc,
                     (Ast.McArr
                        (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "x")))),
                          (Ast.ExNil _loc),
                          (Ast.ExFun
                             (_loc,
                               (Ast.McArr
                                  (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                                    (Ast.ExId (_loc, (Ast.IdLid (_loc, "x")))))))))))),
                ty))) in
  let v3 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              Ast.TyArr
                (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, x)))),
                  (Ast.TyQuo (_loc, "self_type"))) in
            Ast.CrMth
              (_loc, x, Ast.OvNil, Ast.PrNil,
                (Ast.ExFun
                   (_loc,
                     (Ast.McArr
                        (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                          (Ast.ExId (_loc, (Ast.IdLid (_loc, "self")))))))),
                ty))) in
  let v4 =
    ty_names |>
      (List.map
         (fun x  ->
            let ty =
              Ast.TyArr
                (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, x)))),
                  (Ast.TyArr
                     (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, x)))),
                       (Ast.TyQuo (_loc, "self_type"))))) in
            Ast.CrMth
              (_loc, x, Ast.OvNil, Ast.PrNil,
                (Ast.ExFun
                   (_loc,
                     (Ast.McArr
                        (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                          (Ast.ExFun
                             (_loc,
                               (Ast.McArr
                                  (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                                    (Ast.ExId
                                       (_loc, (Ast.IdLid (_loc, "self")))))))))))),
                ty))) in
  let v5 =
    ty_names |>
      (List.map
         (fun x  ->
            Ast.CrMth
              (_loc, x, Ast.OvNil, Ast.PrNil,
                (Ast.ExId (_loc, (Ast.IdLid (_loc, ("pp_print_" ^ x))))),
                (Ast.TyNil _loc)))) in
  ((Ast.crSem_of_list v1), (Ast.crSem_of_list v2), (Ast.crSem_of_list v3),
    (Ast.crSem_of_list v4), (Ast.crSem_of_list v5))
let eq_base1 =
  let items =
    ty_metas |>
      (List.map
         (fun { str; eq;_}  ->
            let ty =
              Ast.TyArr
                (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, str)))),
                  (Ast.TyArr
                     (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, str)))),
                       (Ast.TyId (_loc, (Ast.IdLid (_loc, "bool"))))))) in
            let name = "eq_" ^ str in
            match eq with
            | `Def ->
                Ast.StVal
                  (_loc, Ast.ReNil,
                    (Ast.BiEq
                       (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, name)))),
                         (Ast.ExTyc
                            (_loc,
                              (Ast.ExId (_loc, (Ast.IdLid (_loc, "=")))), ty)))))
            | `Custom s -> s)) in
  Ast.stSem_of_list items
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