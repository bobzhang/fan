let pp_print_string = Format.pp_print_string
let bprintf = Format.bprintf
let fprintf = Format.fprintf
let paths: Tokenf.domains list ref =
  ref
    [`Absolute ["Fan"; "Lang"];
    `Absolute ["Fan"; "Lang"; "Meta"];
    `Absolute ["Fan"; "Lang"; "Filter"]]
let concat_domain =
  function
  | (`Absolute xs,`Sub ys) -> `Absolute (xs @ ys)
  | _ -> invalid_arg "concat_domain"
let names_tbl: (Tokenf.domains,Setf.String.t) Hashtbl.t = Hashtbl.create 30
let dump_names_tbl () =
  names_tbl |>
    (Hashtbl.iter
       (fun k  s  ->
          fprintf Format.std_formatter "Domain:%a\n" Tokenf.pp_print_domains
            k;
          Setf.String.iter
            (fun v  -> fprintf Format.std_formatter "\t%s\n" v) s))
let resolve_name (n : Tokenf.name) =
  match n with
  | ((`Sub _ as x),v) ->
      (match Listf.find_opt
               (fun path  ->
                  match (Hashtblf.find_opt names_tbl) @@
                          (concat_domain (path, x))
                  with
                  | None  -> false
                  | Some set -> Setf.String.mem v set) (!paths)
       with
       | None  -> None
       | Some r -> Some ((concat_domain (r, x)), v))
  | x -> Some x
module ExpKey = Dyn_tag.Pack(struct type 'a t = unit  end)
module ExpFun = Dyn_tag.Pack(struct type 'a t = 'a Tokenf.expand_fun  end)
let current_loc_name = ref None
let stack = Stack.create ()
let current_quot () =
  try Stack.pop stack
  with | Stack.Empty  -> failwith "it's not in a quotation context"
let dump_file = ref None
type key = (Tokenf.name* ExpKey.pack) 
module QMap = Mapf.Make(struct type t = key 
                               let compare = compare end)
let map = ref Mapf.String.empty
let update (pos,(str : Tokenf.name)) =
  map := (Mapf.String.add pos str (!map))
let default_at_pos pos str = update (pos, str)
let default: Tokenf.name option ref = ref None
let set_default s = default := (Some s)
let clear_map () = map := Mapf.String.empty
let clear_default () = default := None
let expander_name ~pos  (name : Tokenf.name) =
  match name with
  | (`Sub [],"") ->
      (try Some (Mapf.String.find pos (!map)) with | Not_found  -> !default)
  | (`Sub _,_) -> resolve_name name
  | (`Absolute _,_) -> Some name
let expanders_table = ref QMap.empty
let add ((domain,n) as name) (tag : 'a Dyn_tag.t) (f : 'a Tokenf.expand_fun)
  =
  let (k,v) = ((name, (ExpKey.pack tag ())), (ExpFun.pack tag f)) in
  let s =
    try Hashtbl.find names_tbl domain with | Not_found  -> Setf.String.empty in
  Hashtbl.replace names_tbl domain (Setf.String.add n s);
  expanders_table := (QMap.add k v (!expanders_table))
let expand (x : Tokenf.quot) (tag : 'a Dyn_tag.t) =
  (let pos_tag = Dyn_tag.of_string tag in
   let name = x.name in
   match expander_name ~pos:pos_tag name with
   | None  ->
       (Locf.failf x.loc "DDSL `%s' not found") @@
         (Tokenf.string_of_name name)
   | Some absolute_name ->
       let pack =
         try
           QMap.find (absolute_name, (ExpKey.pack tag ())) (!expanders_table)
         with
         | Not_found  ->
             Locf.failf x.loc "DDSL expander `%s' at position `%s' not found"
               (Tokenf.string_of_name name) pos_tag in
       let expander = ExpFun.unpack tag pack in Tokenf.quot_expand expander x : 
  'a )
let add_quotation ~exp_filter  ~pat_filter  ~mexp  ~mpat  name entry =
  let entry_eoi = Gramlib.eoi_entry entry in
  let expand_exp loc loc_name_opt s =
    Ref.protect2 (Configf.antiquotations, true)
      (current_loc_name, loc_name_opt)
      (fun _  ->
         ((Gramf.parse_string entry_eoi ~loc s) |> (mexp loc)) |> exp_filter) in
  let expand_stru loc loc_name_opt s =
    let exp_ast = expand_exp loc loc_name_opt s in `StExp (loc, exp_ast) in
  let expand_pat _loc loc_name_opt s =
    Ref.protect Configf.antiquotations true
      (fun _  ->
         let ast = Gramf.parse_string entry_eoi ~loc:_loc s in
         let meta_ast = mpat _loc ast in
         let exp_ast = pat_filter meta_ast in
         let rec subst_first_loc name (x : FAst.pat) =
           (match x with
            | `App (loc,`Vrn (_,u),`Par (_,`Com (_,`Any _,rest))) ->
                `App
                  (loc, (`Vrn (loc, u)),
                    (`Par (loc, (`Com (loc, (`Lid (_loc, name)), rest)))))
            | `App (_loc,`Vrn (_,u),`Any _) ->
                `App (_loc, (`Vrn (_loc, u)), (`Lid (_loc, name)))
            | `Constraint (_loc,a,ty) ->
                `Constraint (_loc, (subst_first_loc name a), ty)
            | p -> p : FAst.pat ) in
         match loc_name_opt with
         | None  -> subst_first_loc (!Locf.name) exp_ast
         | Some "_" -> exp_ast
         | Some name -> subst_first_loc name exp_ast) in
  add name Dyn_tag.exp expand_exp;
  add name Dyn_tag.pat expand_pat;
  add name Dyn_tag.stru expand_stru
let make_parser ?(lexer= Flex_lib.from_stream)  entry loc loc_name_opt s =
  Ref.protect2 (Configf.antiquotations, true)
    (current_loc_name, loc_name_opt)
    (fun _  -> Gramf.parse_string ~lexer (Gramlib.eoi_entry entry) ~loc s)
let of_stru ?lexer  ~name  ~entry  () =
  add name Dyn_tag.stru (make_parser ?lexer entry)
let of_stru_with_filter ?lexer  ~name  ~entry  ~filter  () =
  add name Dyn_tag.stru
    (fun loc  loc_name_opt  s  ->
       filter (make_parser ?lexer entry loc loc_name_opt s))
let of_pat ?lexer  ~name  ~entry  () =
  add name Dyn_tag.pat (make_parser ?lexer entry)
let of_pat_with_filter ?lexer  ~name  ~entry  ~filter  () =
  add name Dyn_tag.pat
    (fun loc  loc_name_opt  s  ->
       filter (make_parser ?lexer entry loc loc_name_opt s))
let of_clfield ?lexer  ~name  ~entry  () =
  add name Dyn_tag.clfield (make_parser ?lexer entry)
let of_clfield_with_filter ?lexer  ~name  ~entry  ~filter  () =
  (add name Dyn_tag.clfield) @@
    (fun loc  loc_name_opt  s  ->
       filter (make_parser ?lexer entry loc loc_name_opt s))
let of_case ?lexer  ~name  ~entry  () =
  add name Dyn_tag.case (make_parser ?lexer entry)
let of_case_with_filter ?lexer  ~name  ~entry  ~filter  () =
  add name Dyn_tag.case
    (fun loc  loc_name_opt  s  ->
       filter (make_parser ?lexer entry loc loc_name_opt s))
let of_exp ?lexer  ~name  ~entry  () =
  let expand_fun = make_parser ?lexer entry in
  let mk_fun loc loc_name_opt s =
    (`StExp (loc, (expand_fun loc loc_name_opt s)) : FAst.stru ) in
  add name Dyn_tag.exp expand_fun; add name Dyn_tag.stru mk_fun
let of_exp_with_filter ?lexer  ~name  ~entry  ~filter  () =
  let expand_fun loc loc_name_opt s =
    filter (make_parser ?lexer entry loc loc_name_opt s) in
  let mk_fun loc loc_name_opt s =
    (`StExp (loc, (expand_fun loc loc_name_opt s)) : FAst.stru ) in
  add name Dyn_tag.exp expand_fun; add name Dyn_tag.stru mk_fun
