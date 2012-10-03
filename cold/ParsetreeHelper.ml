open Parsetree

open Longident

open LibUtil

open Location

let with_loc =
                                                              fun txt ->
                                                               fun loc ->
                                                                (Location.mkloc
                                                                  txt loc)


let lident = fun s -> (Lident (s))

let lident_with_loc =
                                     fun s ->
                                      fun loc ->
                                       (with_loc ( (Lident (s)) ) loc)


let ldot = fun l -> fun s -> (Ldot (l, s))

let lapply =
                                             fun l ->
                                              fun s -> (Lapply (l, s))


let mkghloc = fun loc -> (FanLoc.ghostify loc)

let error =
                                                 fun loc ->
                                                  fun str ->
                                                   (FanLoc.raise loc (
                                                     (Failure (str)) ))


let mksig = fun loc -> fun d -> {psig_desc = d; psig_loc = loc}

let mkmod =
                                                                  fun loc ->
                                                                   fun d ->
                                                                    {pmod_desc =
                                                                    d;
                                                                    pmod_loc =
                                                                    loc}


let mkexp = fun loc -> fun d -> {pexp_desc = d; pexp_loc = loc}

let mkstr =
                                                                  fun loc ->
                                                                   fun d ->
                                                                    {pstr_desc =
                                                                    d;
                                                                    pstr_loc =
                                                                    loc}


let mkfield = fun loc -> fun d -> {pfield_desc = d; pfield_loc = loc}


let mkcty = fun loc -> fun d -> {pcty_desc = d; pcty_loc = loc}

let mkcl =
                                                                  fun loc ->
                                                                   fun d ->
                                                                    {pcl_desc =
                                                                    d;
                                                                    pcl_loc =
                                                                    loc}


let mkcf = fun loc -> fun d -> {pcf_desc = d; pcf_loc = loc}

let mkctf =
                                                               fun loc ->
                                                                fun d ->
                                                                 {pctf_desc =
                                                                   d;
                                                                  pctf_loc =
                                                                   loc}


let mktyp = fun loc -> fun d -> {ptyp_desc = d; ptyp_loc = loc}

let mkpat =
                                                                  fun loc ->
                                                                   fun d ->
                                                                    {ppat_desc =
                                                                    d;
                                                                    ppat_loc =
                                                                    loc}


let mkghpat =
 fun loc -> fun d -> {ppat_desc = d; ppat_loc = ( (mkghloc loc) )}

let mkmty =
                                                                    fun loc ->
                                                                    fun d ->
                                                                    {pmty_desc =
                                                                    d;
                                                                    pmty_loc =
                                                                    loc}


let mkpolytype =
 fun t ->
  (match t.ptyp_desc with
   | Ptyp_poly (_, _) -> t
   | _ -> {t with ptyp_desc = ( (Ptyp_poly ([] , t)) )})

let array_function_no_loc =
                                                           fun str ->
                                                            fun name ->
                                                             (ldot (
                                                               (lident str) )
                                                               (
                                                               if FanConfig.unsafe.contents then
                                                                (
                                                                ("unsafe_" ^
                                                                  name)
                                                                )
                                                               else name ))


let array_function =
 fun loc ->
  fun str -> fun name -> (with_loc ( (array_function_no_loc str name) ) loc)


let mkli =
 fun sloc ->
  fun s ->
   fun (list :
     string list) ->
    let aux =
     function
     | [] -> (lident s)
     | (x :: []) -> (ldot ( (lident x) ) s)
     | (x :: y :: z) ->
        (List.fold_left ldot ( (ldot ( (lident x) ) y) ) ( (z @ ( [s] )) )) in
    (with_loc ( (aux list) ) sloc)

let varify_constructors =
                                     fun var_names ->
                                      let rec loop =
                                       fun t ->
                                        let desc =
                                         (match t.ptyp_desc with
                                          | Ptyp_any -> (Ptyp_any)
                                          | Ptyp_var (x) -> (Ptyp_var (x))
                                          | Ptyp_arrow
                                             (label, core_type, core_type') ->
                                             (Ptyp_arrow
                                               (label, ( (loop core_type) ),
                                                ( (loop core_type') )))
                                          | Ptyp_tuple (lst) ->
                                             (Ptyp_tuple (List.map loop lst))
                                          | Ptyp_constr
                                             ({txt = Lident (s); _ }, [])
                                             when (List.mem s var_names) ->
                                             (Ptyp_var ("&" ^ s))
                                          | Ptyp_constr (longident, lst) ->
                                             (Ptyp_constr
                                               (longident, (
                                                (List.map loop lst) )))
                                          | Ptyp_object (lst) ->
                                             (Ptyp_object
                                               (List.map loop_core_field lst))
                                          | Ptyp_class
                                             (longident, lst, lbl_list) ->
                                             (Ptyp_class
                                               (longident, (
                                                (List.map loop lst) ),
                                                lbl_list))
                                          | Ptyp_alias (core_type, string) ->
                                             (Ptyp_alias
                                               (( (loop core_type) ), string))
                                          | Ptyp_variant
                                             (row_field_list, flag,
                                              lbl_lst_option) ->
                                             (Ptyp_variant
                                               ((
                                                (List.map loop_row_field
                                                  row_field_list) ), flag,
                                                lbl_lst_option))
                                          | Ptyp_poly (string_lst, core_type) ->
                                             (Ptyp_poly
                                               (string_lst, (
                                                (loop core_type) )))
                                          | Ptyp_package (longident, lst) ->
                                             (Ptyp_package
                                               (longident, (
                                                (List.map (
                                                  fun (n, typ) ->
                                                   (n, ( (loop typ) )) ) lst)
                                                )))) in
                                        {t with ptyp_desc = desc}
                                      and loop_core_field =
                                       fun t ->
                                        let desc =
                                         (match t.pfield_desc with
                                          | Pfield (n, typ) ->
                                             (Pfield (n, ( (loop typ) )))
                                          | Pfield_var -> (Pfield_var)) in
                                        {t with pfield_desc = desc}
                                      and loop_row_field =
                                       fun x ->
                                        (match x with
                                         | Rtag (label, flag, lst) ->
                                            (Rtag
                                              (label, flag, (
                                               (List.map loop lst) )))
                                         | Rinherit (t) ->
                                            (Rinherit (loop t))) in
                                      loop
