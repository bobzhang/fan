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
let resolve_name loc (n : Ftoken.name) =
  (match n with
   | ((`Sub _ as x),v) ->
       ((try
           let r =
             List.find
               (fun path  ->
                  (try
                     let set =
                       Hashtbl.find names_tbl (concat_domain (path, x)) in
                     fun ()  -> SSet.mem v set
                   with | Not_found  -> (fun ()  -> false)) ())
               paths.contents in
           fun ()  -> ((concat_domain (r, x)), v)
         with
         | Not_found  ->
             (fun ()  ->
                (FLoc.errorf loc "resolve_name `%s' failed") @@
                  (Ftoken.string_of_name n)))) ()
   | x -> x : Ftoken.name )
type quotation_error_message =  
  | Finding
  | Expanding
  | ParsingResult of FLoc.t* string
  | NoName 
let pp_print_quotation_error_message:
  Format.formatter -> quotation_error_message -> unit =
  fun fmt  ->
    function
    | Finding  -> Format.fprintf fmt "Finding"
    | Expanding  -> Format.fprintf fmt "Expanding"
    | ParsingResult (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(ParsingResult@ %a@ %a)@]" FLoc.pp_print_t
          _a0 pp_print_string _a1
    | NoName  -> Format.fprintf fmt "NoName"
type quotation_error =
  (FLoc.t* Ftoken.name* string* quotation_error_message* exn) 
exception QuotationError of quotation_error
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
let fan_default = ((`Absolute ["Fan"]), "")
let default: Ftoken.name ref = ref fan_default
let set_default s = default := s
let clear_map () = map := SMap.empty
let clear_default () = default := fan_default
let expander_name loc ~pos:(pos : string)  (name : Ftoken.name) =
  match name with
  | (`Sub [],"") ->
      SMap.find_default ~default:(default.contents) pos map.contents
  | (`Sub _,_) -> resolve_name loc name
  | _ -> name
let default_at_pos pos str = update (pos, str)
let expanders_table = ref QMap.empty
let add ((domain,n) as name) (tag : 'a FDyn.tag) (f : 'a expand_fun) =
  let (k,v) = ((name, (ExpKey.pack tag ())), (ExpFun.pack tag f)) in
  let s = try Hashtbl.find names_tbl domain with | Not_found  -> SSet.empty in
  Hashtbl.replace names_tbl domain (SSet.add n s);
  expanders_table := (QMap.add k v expanders_table.contents)
let expand_quotation ~expander  pos_tag (x : Ftoken.quot) =
  let loc = Location_util.join (FLoc.move `start x.shift x.loc) in
  try expander loc x.meta x.content
  with | FLoc.Exc_located (_,QuotationError _) as exc -> raise exc
  | FLoc.Exc_located (iloc,exc) ->
      let exc1 = QuotationError (iloc, (x.name), pos_tag, Expanding, exc) in
      raise (FLoc.Exc_located (iloc, exc1))
  | exc ->
      let exc1 = QuotationError (loc, (x.name), pos_tag, Expanding, exc) in
      raise (FLoc.Exc_located (loc, exc1))
let find loc name tag =
  let key =
    ((expander_name loc ~pos:(FDyn.string_of_tag tag) name),
      (ExpKey.pack tag ())) in
  (try
     let pack = QMap.find key expanders_table.contents in
     fun ()  -> ExpFun.unpack tag pack
   with
   | Not_found  ->
       (fun ()  ->
          let pos_tag = FDyn.string_of_tag tag in
          match name with
          | (`Sub [],"") ->
              FLoc.raise loc
                (QuotationError (loc, name, pos_tag, NoName, Not_found))
          | _ -> raise Not_found)
   | e -> (fun ()  -> raise e)) ()
let expand (x : Ftoken.quot) (tag : 'a FDyn.tag) =
  (let pos_tag = FDyn.string_of_tag tag in
   (try
      let expander = find x.loc x.name tag in
      fun ()  ->
        Stack.push x.name stack;
        (finally ~action:(fun _  -> Stack.pop stack) ()) @@
          ((fun _  -> expand_quotation ~expander pos_tag x))
    with
    | FLoc.Exc_located (_,QuotationError _) as exc -> (fun ()  -> raise exc)
    | FLoc.Exc_located (qloc,exc) ->
        (fun ()  ->
           raise
             (FLoc.Exc_located
                (qloc,
                  (QuotationError (qloc, (x.name), pos_tag, Finding, exc)))))
    | exc ->
        (fun ()  ->
           raise
             (FLoc.Exc_located
                ((x.loc),
                  (QuotationError ((x.loc), (x.name), pos_tag, Finding, exc))))))
     () : 'a )
let quotation_error_to_string (loc,name,position,ctx,exn) =
  let ppf = Buffer.create 30 in
  let name = expander_name loc ~pos:position name in
  let pp x =
    bprintf ppf "@?@[<2>While %s %S in a position of %S:" x
      (Ftoken.string_of_name name) position in
  let () =
    match ctx with
    | Finding  ->
        (pp "finding quotation";
         bprintf ppf "@ @[<hv2>Available quotation expanders are:@\n";
         QMap.iter
           (fun (s,t)  _  ->
              bprintf ppf "@[<2>%s@ (in@ a@ position@ of %a)@]@ "
                (Ftoken.string_of_name s) ExpKey.print_tag t)
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
                 bprintf ppf "%a:" FLoc.print
                   (FLoc.set_file_name dump_file loc)
               with
               | _ ->
                   bprintf ppf
                     "Error while dumping result in file %S; dump aborted"
                     dump_file)
          | None  ->
              bprintf ppf
                "\n(consider setting variable Ast_quotation.dump_file, or using the -QD option)"))
    | NoName  -> pp "No default quotation name" in
  let () = bprintf ppf "@\n%s@]@." (Printexc.to_string exn) in
  Buffer.contents ppf
let _ =
  Printexc.register_printer
    (function
     | QuotationError x -> Some (quotation_error_to_string x)
     | _ -> None)
let parse_quotation_result parse loc (q_name,_q_loc,_q_shift,q_contents)
  pos_tag str =
  try parse loc str
  with
  | FLoc.Exc_located (iloc,QuotationError (_,n,pos_tag,Expanding ,exc)) ->
      let ctx = ParsingResult (iloc, q_contents) in
      let exc1 = QuotationError (iloc, n, pos_tag, ctx, exc) in
      FLoc.raise iloc exc1
  | FLoc.Exc_located (iloc,(QuotationError _ as exc)) -> FLoc.raise iloc exc
  | FLoc.Exc_located (iloc,exc) ->
      let ctx = ParsingResult (iloc, q_contents) in
      let exc1 = QuotationError (iloc, q_name, pos_tag, ctx, exc) in
      FLoc.raise iloc exc1
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