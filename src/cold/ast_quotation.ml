open LibUtil
let pp_print_string = Format.pp_print_string
let bprintf = Format.bprintf
let paths: Ftoken.domains list ref =
  ref
    [`Absolute ["Fan"; "Lang"];
    `Absolute ["Fan"; "Lang"; "Meta"];
    `Absolute ["Fan"; "Lang"; "Filter"]]
let concat_domain =
  function
  | (`Absolute xs,`Sub ys) -> `Absolute (xs @ ys)
  | _ -> invalid_arg "concat_domain"
let names_tbl: (Ftoken.domains,SSet.t) Hashtbl.t = Hashtbl.create 30
let resolve_name (n : Ftoken.name) =
  match n with
  | ((`Sub _ as x),v) ->
      (match List.find_opt
               (fun path  ->
                  match (Hashtbl.find_opt names_tbl) @@
                          (concat_domain (path, x))
                  with
                  | None  -> false
                  | Some set -> SSet.mem v set) paths.contents
       with
       | None  -> None
       | Some r -> Some ((concat_domain (r, x)), v))
  | x -> Some x
type 'a expand_fun = FLoc.t -> string option -> string -> 'a 
module ExpKey = FDyn.Pack(struct type 'a t = unit  end)
module ExpFun = FDyn.Pack(struct type 'a t = 'a expand_fun  end)
let current_loc_name = ref None
let stack = Stack.create ()
let current_quot () =
  try Stack.pop stack
  with | Stack.Empty  -> failwith "it's not in a quotation context"
let dump_file = ref None
type key = (Ftoken.name* ExpKey.pack) 
module QMap = MapMake(struct type t = key 
                             let compare = compare end)
let map = ref SMap.empty
let update (pos,(str : Ftoken.name)) = map := (SMap.add pos str map.contents)
let default_at_pos pos str = update (pos, str)
let default: Ftoken.name option ref = ref None
let set_default s = default := (Some s)
let clear_map () = map := SMap.empty
let clear_default () = default := None
let expander_name ~pos  (name : Ftoken.name) =
  match name with
  | (`Sub [],"") ->
      (try Some (SMap.find pos map.contents)
       with | Not_found  -> default.contents)
  | (`Sub _,_) -> resolve_name name
  | (`Absolute _,_) -> Some name
let expanders_table = ref QMap.empty
let add ((domain,n) as name) (tag : 'a FDyn.tag) (f : 'a expand_fun) =
  let (k,v) = ((name, (ExpKey.pack tag ())), (ExpFun.pack tag f)) in
  let s = try Hashtbl.find names_tbl domain with | Not_found  -> SSet.empty in
  Hashtbl.replace names_tbl domain (SSet.add n s);
  expanders_table := (QMap.add k v expanders_table.contents)
let expand (x : Ftoken.quot) (tag : 'a FDyn.tag) =
  (let pos_tag = FDyn.string_of_tag tag in
   let name = x.name in
   match expander_name ~pos:pos_tag name with
   | None  ->
       (FLoc.failf x.loc "DDSL `%s' not found") @@
         (Ftoken.string_of_name name)
   | Some absolute_name ->
       let pack =
         try
           QMap.find (absolute_name, (ExpKey.pack tag ()))
             expanders_table.contents
         with
         | Not_found  ->
             FLoc.failf x.loc "DDSL expander `%s' at position `%s' not found"
               (Ftoken.string_of_name name) pos_tag in
       let expander = ExpFun.unpack tag pack in
       let loc = Location_util.join (FLoc.move `start x.shift x.loc) in
       let content =
         String.sub x.content x.shift
           (((String.length x.content) - x.retract) - x.shift) in
       expander loc x.meta content : 'a )
let add_quotation ~exp_filter  ~pat_filter  ~mexp  ~mpat  name entry =
  let entry_eoi = Fgram.eoi_entry entry in
  let expand_exp loc loc_name_opt s =
    Ref.protect2 (FConfig.antiquotations, true)
      (current_loc_name, loc_name_opt)
      (fun _  ->
         ((Fgram.parse_string entry_eoi ~loc s) |> (mexp loc)) |> exp_filter) in
  let expand_stru loc loc_name_opt s =
    let exp_ast = expand_exp loc loc_name_opt s in `StExp (loc, exp_ast) in
  let expand_pat _loc loc_name_opt s =
    Ref.protect FConfig.antiquotations true
      (fun _  ->
         let ast = Fgram.parse_string entry_eoi ~loc:_loc s in
         let meta_ast = mpat _loc ast in
         let exp_ast = pat_filter meta_ast in
         let rec subst_first_loc name (x : FAst.pat) =
           (match x with
            | `App (loc,`Vrn (_,u),`Par (_,`Com (_,_,rest))) ->
                `App
                  (loc, (`Vrn (loc, u)),
                    (`Par (loc, (`Com (loc, (`Lid (_loc, name)), rest)))))
            | `App (_loc,`Vrn (_,u),`Any _) ->
                `App (_loc, (`Vrn (_loc, u)), (`Lid (_loc, name)))
            | `App (_loc,a,b) -> `App (_loc, (subst_first_loc name a), b)
            | `Constraint (_loc,a,ty) ->
                `Constraint (_loc, (subst_first_loc name a), ty)
            | p -> p : FAst.pat ) in
         match loc_name_opt with
         | None  -> subst_first_loc FLoc.name.contents exp_ast
         | Some "_" -> exp_ast
         | Some name -> subst_first_loc name exp_ast) in
  add name FDyn.exp_tag expand_exp;
  add name FDyn.pat_tag expand_pat;
  add name FDyn.stru_tag expand_stru
let make_parser ?(lexer= Flex_lib.from_stream)  entry loc loc_name_opt s =
  Ref.protect2 (FConfig.antiquotations, true)
    (current_loc_name, loc_name_opt)
    (fun _  -> Fgram.parse_string ~lexer (Fgram.eoi_entry entry) ~loc s)
let of_stru ?lexer  ~name  ~entry  () =
  add name FDyn.stru_tag (make_parser ?lexer entry)
let of_stru_with_filter ?lexer  ~name  ~entry  ~filter  () =
  add name FDyn.stru_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser ?lexer entry loc loc_name_opt s))
let of_pat ?lexer  ~name  ~entry  () =
  add name FDyn.pat_tag (make_parser ?lexer entry)
let of_pat_with_filter ?lexer  ~name  ~entry  ~filter  () =
  add name FDyn.pat_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser ?lexer entry loc loc_name_opt s))
let of_clfield ?lexer  ~name  ~entry  () =
  add name FDyn.clfield_tag (make_parser ?lexer entry)
let of_clfield_with_filter ?lexer  ~name  ~entry  ~filter  () =
  (add name FDyn.clfield_tag) @@
    (fun loc  loc_name_opt  s  ->
       filter (make_parser ?lexer entry loc loc_name_opt s))
let of_case ?lexer  ~name  ~entry  () =
  add name FDyn.case_tag (make_parser ?lexer entry)
let of_case_with_filter ?lexer  ~name  ~entry  ~filter  () =
  add name FDyn.case_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser ?lexer entry loc loc_name_opt s))
let of_exp ?lexer  ~name  ~entry  () =
  let expand_fun = make_parser ?lexer entry in
  let mk_fun loc loc_name_opt s =
    (`StExp (loc, (expand_fun loc loc_name_opt s)) : FAst.stru ) in
  add name FDyn.exp_tag expand_fun; add name FDyn.stru_tag mk_fun
let of_exp_with_filter ?lexer  ~name  ~entry  ~filter  () =
  let expand_fun loc loc_name_opt s =
    filter (make_parser ?lexer entry loc loc_name_opt s) in
  let mk_fun loc loc_name_opt s =
    (`StExp (loc, (expand_fun loc loc_name_opt s)) : FAst.stru ) in
  add name FDyn.exp_tag expand_fun; add name FDyn.stru_tag mk_fun