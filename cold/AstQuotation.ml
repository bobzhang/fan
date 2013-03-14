open Ast
open LibUtil
open FanToken
open Format
let _ = ()
type quotation_error_message =  
  | Finding
  | Expanding
  | ParsingResult of FanLoc.t* string
  | NoName 
type quotation_error = (name* string* quotation_error_message* exn) 
exception QuotationError of quotation_error
type 'a expand_fun = FanLoc.t -> string option -> string -> 'a 
module ExpKey = DynAst.Pack(struct type 'a t = unit  end)
module ExpFun = DynAst.Pack(struct type 'a t = 'a expand_fun  end)
let current_loc_name = ref None
let stack = Stack.create ()
let current_quot () =
  try Stack.pop stack
  with | Stack.Empty  -> failwith "it's not in a quotation context"
let dump_file = ref None
type key = (name* ExpKey.pack) 
module QMap = MapMake(struct type t = key 
                             let compare = compare end)
let map = ref SMap.empty
let update (pos,(str : name)) = map := (SMap.add pos str map.contents)
let fan_default = ((`Absolute ["Fan"]), "")
let default: name ref = ref fan_default
let set_default s = default := s
let clear_map () = map := SMap.empty
let clear_default () = default := fan_default
let expander_name ~pos:(pos : string)  (name : name) =
  match name with
  | (`Sub [],"") ->
      SMap.find_default ~default:(default.contents) pos map.contents
  | (`Sub _,_) -> FanToken.resolve_name name
  | _ -> name
let default_at_pos pos str = update (pos, str)
let expanders_table = ref QMap.empty
let add ((domain,n) as name) (tag : 'a DynAst.tag) (f : 'a expand_fun) =
  let (k,v) = ((name, (ExpKey.pack tag ())), (ExpFun.pack tag f)) in
  let s = try Hashtbl.find names_tbl domain with | Not_found  -> SSet.empty in
  Hashtbl.replace names_tbl domain (SSet.add n s);
  expanders_table := (QMap.add k v expanders_table.contents)
let expand_quotation loc ~expander  pos_tag quot =
  let open FanToken in
    let loc_name_opt = if quot.q_loc = "" then None else Some (quot.q_loc) in
    try expander loc loc_name_opt quot.q_contents
    with | FanLoc.Exc_located (_,QuotationError _) as exc -> raise exc
    | FanLoc.Exc_located (iloc,exc) ->
        let exc1 = QuotationError ((quot.q_name), pos_tag, Expanding, exc) in
        raise (FanLoc.Exc_located (iloc, exc1))
    | exc ->
        let exc1 = QuotationError ((quot.q_name), pos_tag, Expanding, exc) in
        raise (FanLoc.Exc_located (loc, exc1))
let find loc name tag =
  let key =
    ((expander_name ~pos:(DynAst.string_of_tag tag) name),
      (ExpKey.pack tag ())) in
  (try
     let pack = QMap.find key expanders_table.contents in
     fun ()  -> ExpFun.unpack tag pack
   with
   | Not_found  ->
       (fun ()  ->
          let pos_tag = DynAst.string_of_tag tag in
          match name with
          | (`Sub [],"") ->
              FanLoc.raise loc
                (QuotationError (name, pos_tag, NoName, Not_found))
          | _ -> raise Not_found)
   | e -> (fun ()  -> raise e)) ()
let expand loc (quotation : FanToken.quotation) tag =
  let open FanToken in
    let pos_tag = DynAst.string_of_tag tag in
    let name = quotation.q_name in
    (try
       let expander = find loc name tag
       and loc = FanLoc.join (FanLoc.move `start quotation.q_shift loc) in
       fun ()  ->
         Stack.push quotation.q_name stack;
         finally (fun _  -> Stack.pop stack)
           (fun _  -> expand_quotation ~expander loc pos_tag quotation) ()
     with
     | FanLoc.Exc_located (_,QuotationError _) as exc ->
         (fun ()  -> raise exc)
     | FanLoc.Exc_located (qloc,exc) ->
         (fun ()  ->
            raise
              (FanLoc.Exc_located
                 (qloc, (QuotationError (name, pos_tag, Finding, exc)))))
     | exc ->
         (fun ()  ->
            raise
              (FanLoc.Exc_located
                 (loc, (QuotationError (name, pos_tag, Finding, exc)))))) ()
let quotation_error_to_string (name,position,ctx,exn) =
  let ppf = Buffer.create 30 in
  let name = expander_name ~pos:position name in
  let pp x =
    bprintf ppf "@?@[<2>While %s %S in a position of %S:" x
      (string_of_name name) position in
  let () =
    match ctx with
    | Finding  ->
        (pp "finding quotation";
         bprintf ppf "@ @[<hv2>Available quotation expanders are:@\n";
         QMap.iter
           (fun (s,t)  _  ->
              bprintf ppf "@[<2>%s@ (in@ a@ position@ of %a)@]@ "
                (string_of_name s) ExpKey.print_tag t)
           expanders_table.contents;
         bprintf ppf "@]")
    | Expanding  -> pp "expanding quotation"
    | ParsingResult (loc,str) ->
        (pp "parsing result of quotation";
         (match dump_file.contents with
          | Some dump_file ->
              let () = bprintf ppf " dumping result...\n" in
              (try
                 let oc = open_out_bin dump_file in
                 output_string oc str;
                 output_string oc "\n";
                 flush oc;
                 close_out oc;
                 bprintf ppf "%a:" FanLoc.print
                   (FanLoc.set_file_name dump_file loc)
               with
               | _ ->
                   bprintf ppf
                     "Error while dumping result in file %S; dump aborted"
                     dump_file)
          | None  ->
              bprintf ppf
                "\n(consider setting variable AstQuotation.dump_file, or using the -QD option)"))
    | NoName  -> pp "No default quotation name" in
  let () = bprintf ppf "@\n%s@]@." (Printexc.to_string exn) in
  Buffer.contents ppf
let _ =
  Printexc.register_printer
    (function
     | QuotationError x -> Some (quotation_error_to_string x)
     | _ -> None)
let parse_quotation_result parse loc quot pos_tag str =
  let open FanToken in
    try parse loc str
    with
    | FanLoc.Exc_located (iloc,QuotationError (n,pos_tag,Expanding ,exc)) ->
        let ctx = ParsingResult (iloc, (quot.q_contents)) in
        let exc1 = QuotationError (n, pos_tag, ctx, exc) in
        FanLoc.raise iloc exc1
    | FanLoc.Exc_located (iloc,(QuotationError _ as exc)) ->
        FanLoc.raise iloc exc
    | FanLoc.Exc_located (iloc,exc) ->
        let ctx = ParsingResult (iloc, (quot.q_contents)) in
        let exc1 = QuotationError ((quot.q_name), pos_tag, ctx, exc) in
        FanLoc.raise iloc exc1
let add_quotation ~expr_filter  ~patt_filter  ~mexpr  ~mpatt  name entry =
  let entry_eoi = Gram.eoi_entry entry in
  let expand_expr loc loc_name_opt s =
    Ref.protect2 (FanConfig.antiquotations, true)
      (current_loc_name, loc_name_opt)
      (fun _  ->
         ((Gram.parse_string entry_eoi ~loc s) |> (mexpr loc)) |> expr_filter) in
  let expand_str_item loc loc_name_opt s =
    let exp_ast = expand_expr loc loc_name_opt s in `StExp (loc, exp_ast) in
  let expand_patt _loc loc_name_opt s =
    Ref.protect FanConfig.antiquotations true
      (fun _  ->
         let ast = Gram.parse_string entry_eoi ~loc:_loc s in
         let meta_ast = mpatt _loc ast in
         let exp_ast = patt_filter meta_ast in
         let rec subst_first_loc name =
           (function
            | `App (loc,`Vrn (_,u),`Tup (_,`Com (_,_,rest))) ->
                `App
                  (loc, (`Vrn (loc, u)),
                    (`Tup
                       (loc,
                         (`Com (loc, (`Id (_loc, (`Lid (_loc, name)))), rest)))))
            | `App (_loc,`Vrn (_,u),`Any _) ->
                `App
                  (_loc, (`Vrn (_loc, u)), (`Id (_loc, (`Lid (_loc, name)))))
            | `App (_loc,a,b) -> `App (_loc, (subst_first_loc name a), b)
            | p -> p : patt -> patt ) in
         match loc_name_opt with
         | None  -> subst_first_loc FanLoc.name.contents exp_ast
         | Some "_" -> exp_ast
         | Some name -> subst_first_loc name exp_ast) in
  add name DynAst.expr_tag expand_expr;
  add name DynAst.patt_tag expand_patt;
  add name DynAst.str_item_tag expand_str_item
let make_parser entry loc loc_name_opt s =
  Ref.protect2 (FanConfig.antiquotations, true)
    (current_loc_name, loc_name_opt)
    (fun _  -> Gram.parse_string (Gram.eoi_entry entry) ~loc s)
let _ = ()
let _ = ()
let of_str_item ~name  ~entry  =
  add name DynAst.str_item_tag (make_parser entry)
let of_str_item_with_filter ~name  ~entry  ~filter  =
  add name DynAst.str_item_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser entry loc loc_name_opt s))
let of_patt ~name  ~entry  = add name DynAst.patt_tag (make_parser entry)
let of_patt_with_filter ~name  ~entry  ~filter  =
  add name DynAst.patt_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser entry loc loc_name_opt s))
let of_class_str_item ~name  ~entry  =
  add name DynAst.class_str_item_tag (make_parser entry)
let of_class_str_item_with_filter ~name  ~entry  ~filter  =
  add name DynAst.class_str_item_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser entry loc loc_name_opt s))
let of_case ~name  ~entry  = add name DynAst.case_tag (make_parser entry)
let of_case_with_filter ~name  ~entry  ~filter  =
  add name DynAst.case_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser entry loc loc_name_opt s))
let of_expr ~name  ~entry  =
  let expand_fun = make_parser entry in
  let mk_fun loc loc_name_opt s =
    `StExp (loc, (expand_fun loc loc_name_opt s)) in
  add name DynAst.expr_tag expand_fun; add name DynAst.str_item_tag mk_fun
let of_expr_with_filter ~name  ~entry  ~filter  =
  let expand_fun loc loc_name_opt s =
    filter (make_parser entry loc loc_name_opt s) in
  let mk_fun loc loc_name_opt s =
    `StExp (loc, (expand_fun loc loc_name_opt s)) in
  add name DynAst.expr_tag expand_fun; add name DynAst.str_item_tag mk_fun