open Astfn
open Astn_util
let extract info =
  info |>
    (Listf.concat_map
       (fun (x : Ctyp.ty_info)  -> [x.name_exp; (x.id_ep :>exp)]))
let mkfmt pre sep post fields =
  let s = pre ^ ((String.concat sep fields) ^ post) in
  (`App
     ((`App ((`Dot ((`Uid "Format"), (`Lid "fprintf"))), (`Lid "fmt"))),
       (`Str s)) :>Astfn.exp)
let mk_variant cons params =
  let len = List.length params in
  let pre =
    match cons with
    | Some cons when len >= 1 ->
        (mkfmt ("@[<1>(" ^ (cons ^ "@ ")) "@ " ")@]") @@
          (Listf.init len (fun _  -> "%a"))
    | Some cons -> mkfmt cons "" "" []
    | None  ->
        (mkfmt "@[<1>(" ",@," ")@]") @@ (Listf.init len (fun _  -> "%a")) in
  appl_of_list (pre :: (extract params))
let mk_record cols =
  let pre =
    (cols |> (List.map (fun (x : Ctyp.record_col)  -> x.label ^ ":%a"))) |>
      (mkfmt "@[<hv 1>{" ";@," "}@]") in
  appl_of_list (pre ::
    ((cols |> (List.map (fun (x : Ctyp.record_col)  -> x.info))) |> extract))
let default: Derive_stru.param =
  {
    arity = 1;
    default = None;
    id = (`Pre "pp_print_");
    names = ["fmt"];
    mk_record = (Some mk_record);
    annot =
      (Some
         (fun s  ->
            ((`Arrow
                ((`Dot ((`Uid "Format"), (`Lid "formatter"))),
                  (`Arrow ((`Lid s), (`Lid "unit")))) :>Astfn.ctyp),
              (`Lid "unit" :>Astfn.ctyp))));
    mk_variant = (Some mk_variant);
    plugin_name = "Print";
    excludes = ["loc"];
    builtin_tbl =
      [((`Lid "int" :>Astfn.ctyp),
         (`Dot ((`Uid "Format"), (`Lid "pp_print_int")) :>Astfn.exp));
      ((`Lid "int32" :>Astfn.ctyp),
        (`Fun
           (`Case
              ((`Lid "fmt"),
                (`App
                   ((`Dot ((`Uid "Format"), (`Lid "fprintf"))), (`Str "%ld"))))) :>
        Astfn.exp));
      ((`Lid "int64" :>Astfn.ctyp),
        (`Fun
           (`Case
              ((`Lid "fmt"),
                (`App
                   ((`Dot ((`Uid "Format"), (`Lid "fprintf"))), (`Str "%Ld"))))) :>
        Astfn.exp));
      ((`Lid "nativeint" :>Astfn.ctyp),
        (`Fun
           (`Case
              ((`Lid "fmt"),
                (`App
                   ((`Dot ((`Uid "Format"), (`Lid "fprintf"))), (`Str "%nd"))))) :>
        Astfn.exp));
      ((`Lid "float" :>Astfn.ctyp),
        (`Dot ((`Uid "Format"), (`Lid "pp_print_float")) :>Astfn.exp));
      ((`Lid "string" :>Astfn.ctyp),
        (`Fun
           (`Case
              ((`Lid "fmt"),
                (`App
                   ((`App
                       ((`Dot ((`Uid "Format"), (`Lid "fprintf"))),
                         (`Lid "fmt"))), (`Str "%S"))))) :>Astfn.exp));
      ((`Lid "bool" :>Astfn.ctyp),
        (`Dot ((`Uid "Format"), (`Lid "pp_print_bool")) :>Astfn.exp));
      ((`Lid "char" :>Astfn.ctyp),
        (`Dot ((`Uid "Format"), (`Lid "pp_print_char")) :>Astfn.exp));
      ((`Lid "unit" :>Astfn.ctyp),
        (`Fun
           (`Case
              ((`Lid "fmt"),
                (`Fun
                   (`Case
                      ((`Constraint (`Any, (`Lid "unit"))),
                        (`App
                           ((`App
                               ((`Dot ((`Uid "Format"), (`Lid "fprintf"))),
                                 (`Lid "fmt"))), (`Str "()")))))))) :>
        Astfn.exp));
      ((`Lid "list" :>Astfn.ctyp),
        (`Fun
           (`Case
              ((`Lid "mf_a"),
                (`Fun
                   (`Case
                      ((`Lid "fmt"),
                        (`Fun
                           (`Case
                              ((`Lid "lst"),
                                (`App
                                   ((`App
                                       ((`App
                                           ((`App
                                               ((`Dot
                                                   ((`Uid "Format"),
                                                     (`Lid "fprintf"))),
                                                 (`Lid "fmt"))),
                                             (`Str "@[<1>[%a]@]"))),
                                         (`Fun
                                            (`Case
                                               ((`Lid "fmt"),
                                                 (`App
                                                    ((`Dot
                                                        ((`Uid "List"),
                                                          (`Lid "iter"))),
                                                      (`Fun
                                                         (`Case
                                                            ((`Lid "x"),
                                                              (`App
                                                                 ((`App
                                                                    ((`App
                                                                    ((`App
                                                                    ((`Dot
                                                                    ((`Uid
                                                                    "Format"),
                                                                    (`Lid
                                                                    "fprintf"))),
                                                                    (`Lid
                                                                    "fmt"))),
                                                                    (`Str
                                                                    "%a@ "))),
                                                                    (`Lid
                                                                    "mf_a"))),
                                                                   (`Lid "x"))))))))))))),
                                     (`Lid "lst"))))))))))) :>Astfn.exp));
      ((`Lid "option" :>Astfn.ctyp),
        (`Fun
           (`Case
              ((`Lid "mf_a"),
                (`Fun
                   (`Case
                      ((`Lid "fmt"),
                        (`Fun
                           (`Case
                              ((`Lid "v"),
                                (`Match
                                   ((`Lid "v"),
                                     (`Bar
                                        ((`Case
                                            ((`Uid "None"),
                                              (`App
                                                 ((`App
                                                     ((`Dot
                                                         ((`Uid "Format"),
                                                           (`Lid "fprintf"))),
                                                       (`Lid "fmt"))),
                                                   (`Str "None"))))),
                                          (`Case
                                             ((`App
                                                 ((`Uid "Some"), (`Lid "v"))),
                                               (`App
                                                  ((`App
                                                      ((`App
                                                          ((`App
                                                              ((`Dot
                                                                  ((`Uid
                                                                    "Format"),
                                                                    (
                                                                    `Lid
                                                                    "fprintf"))),
                                                                (`Lid "fmt"))),
                                                            (`Str
                                                               "Some @[%a@]"))),
                                                        (`Lid "mf_a"))),
                                                    (`Lid "v"))))))))))))))))) :>
        Astfn.exp));
      ((`Lid "arrow" :>Astfn.ctyp),
        (`Fun
           (`Case
              (`Any,
                (`Fun
                   (`Case
                      (`Any,
                        (`Fun
                           (`Case
                              ((`Lid "fmt"),
                                (`Fun
                                   (`Case
                                      (`Any,
                                        (`App
                                           ((`App
                                               ((`Dot
                                                   ((`Uid "Formatf"),
                                                     (`Lid "fprintf"))),
                                                 (`Lid "fmt"))),
                                             (`Str "<fun>")))))))))))))) :>
        Astfn.exp))]
  }
