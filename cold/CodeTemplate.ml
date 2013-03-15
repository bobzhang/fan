open AstLoc
open LibUtil
let _loc = FanLoc.ghost
type ty_meta = 
  {
  str: string;
  print: [ `Exist | `Custom of stru | `Fmt of string];
  eq: [ `Def | `Custom of stru]} 
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
                 (`Constraint
                    (_loc,
                      (`Fun
                         (_loc,
                           (`Case
                              (_loc, (`Id (_loc, (`Lid (_loc, "fmt")))),
                                (`Fun
                                   (_loc,
                                     (`Case
                                        (_loc, (`Any _loc),
                                          (`App
                                             (_loc,
                                               (`App
                                                  (_loc,
                                                    (`Id
                                                       (_loc,
                                                         (`Dot
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
                                (`Dot
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
                 (`Constraint
                    (_loc,
                      (`Fun
                         (_loc,
                           (`Case
                              (_loc, (`Any _loc),
                                (`Fun
                                   (_loc,
                                     (`Case
                                        (_loc, (`Any _loc),
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
                       (`Dot
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
                         (`Constraint
                            (_loc,
                              (`Fun
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`Id (_loc, (`Lid (_loc, "fmt")))),
                                        (`Fun
                                           (_loc,
                                             (`Case
                                                (_loc,
                                                  (`Id
                                                     (_loc,
                                                       (`Lid (_loc, "a")))),
                                                  (`App
                                                     (_loc,
                                                       (`App
                                                          (_loc,
                                                            (`App
                                                               (_loc,
                                                                 (`Id
                                                                    (_loc,
                                                                    (`Dot
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
  sem_of_list items
let (map_cstru_base_1,map_cstru_base_2,fold_cstru_base_1,fold_cstru_base_2,print_cstru_base,iter_cstru_base_1,eq_cstru_base_2)
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
                     (_loc, (`Id (_loc, (`Lid (_loc, "x")))),
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
                     (_loc, (`Id (_loc, (`Lid (_loc, "x")))),
                       (`Fun
                          (_loc,
                            (`Case
                               (_loc, (`Any _loc),
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
                  (`Quote (_loc, (`Normal _loc), (`Lid (_loc, "self_type"))))) in
            let exp =
              `Fun
                (_loc,
                  (`Case
                     (_loc, (`Any _loc), (`Id (_loc, (`Lid (_loc, "self"))))))) in
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
                          (_loc, (`Normal _loc), (`Lid (_loc, "self_type"))))))) in
            let exp =
              `Fun
                (_loc,
                  (`Case
                     (_loc, (`Any _loc),
                       (`Fun
                          (_loc,
                            (`Case
                               (_loc, (`Any _loc),
                                 (`Id (_loc, (`Lid (_loc, "self"))))))))))) in
            `CrMth
              (_loc, (`Lid (_loc, x)), (`OvNil _loc), (`PrNil _loc), exp, ty))) in
  let v5 =
    ty_names |>
      (List.map
         (fun x  ->
            let exp = `Id (_loc, (`Lid (_loc, ("pp_print_" ^ x)))) in
            `CrMthS
              (_loc, (`Lid (_loc, x)), (`OvNil _loc), (`PrNil _loc), exp))) in
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
                     (_loc, (`Any _loc), (`Id (_loc, (`Uid (_loc, "()"))))))) in
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
                     (_loc, (`Id (_loc, (`Lid (_loc, "x")))),
                       (`Fun
                          (_loc,
                            (`Case
                               (_loc, (`Id (_loc, (`Lid (_loc, "y")))),
                                 (`App
                                    (_loc,
                                      (`App
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
  ((sem_of_list v1), (sem_of_list v2), (sem_of_list v3), (sem_of_list v4),
    (sem_of_list v5), (sem_of_list v6), (sem_of_list v7))
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
                         (`Constraint
                            (_loc, (`Id (_loc, (`Lid (_loc, "=")))), ty)))))
            | `Custom s -> s)) in
  sem_of_list items
let _ =
  let open AstInjection in
    register_inject_cstru ("map_cstru_base_1", map_cstru_base_1);
    register_inject_cstru ("map_cstru_base_2", map_cstru_base_2);
    register_inject_cstru ("fold_cstru_base_1", fold_cstru_base_1);
    register_inject_cstru ("fold_cstru_base_2", fold_cstru_base_2);
    register_inject_cstru ("print_cstru_base", print_cstru_base);
    register_inject_cstru ("iter_cstru_base_1", iter_cstru_base_1);
    register_inject_cstru ("eq_cstru_base_2", eq_cstru_base_2);
    register_inject_stru ("eq_base1", eq_base1);
    register_inject_stru ("print_base1", print_base1)