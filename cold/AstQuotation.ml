open Ast
open LibUtil
open FanUtil
open Lib.Meta
open Format
open StdLib
let _ = ()
type quotation_error_message =  
  | Finding
  | Expanding
  | ParsingResult of FanLoc.t* string
  | NoName 
type quotation_error = (string* string* quotation_error_message* exn) 
let pp_print_quotation_error_message:
  'fmt -> quotation_error_message -> 'result =
  fun fmt  ->
    function
    | Finding  -> Format.fprintf fmt "Finding"
    | Expanding  -> Format.fprintf fmt "Expanding"
    | ParsingResult (a0,a1) ->
        Format.fprintf fmt "@[<1>(ParsingResult@ %a@ %a)@]" FanLoc.pp_print_t
          a0 pp_print_string a1
    | NoName  -> Format.fprintf fmt "NoName"
let pp_print_quotation_error: 'fmt -> quotation_error -> 'result =
  fun fmt  a0  ->
    (fun fmt  (a0,a1,a2,a3)  ->
       Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_string a0
         pp_print_string a1 pp_print_quotation_error_message a2 pp_print_exn
         a3) fmt a0
exception QuotationError of quotation_error
type 'a expand_fun = FanLoc.t -> string option -> string -> 'a 
module ExpKey = DynAst.Pack(struct
  type 'a t = unit 
  end)
module ExpFun = DynAst.Pack(struct
  type 'a t = 'a expand_fun 
  end)
let current_loc_name = ref None
let stack = Stack.create ()
let current_quot () =
  try Stack.pop stack
  with | Stack.Empty  -> failwith "it's not in a quotation context"
let dump_file = ref None
let default = ref ""
let map = ref SMap.empty
let update (pos,str) = map := (SMap.add pos str map.contents)
let translate = ref (fun x  -> x)
let clear_map () = map := SMap.empty
let clear_default () = default := ""
let default_at_pos pos str = update (pos, str)
let expanders_table: ((string* ExpKey.pack)* ExpFun.pack) list ref = ref []
let set_default s = default := s
let expander_name ~pos:(pos : string)  (name : string) =
  let u = translate.contents name in
  if u = ""
  then SMap.find_default ~default:(default.contents) pos map.contents
  else u
let add name tag f =
  let elt = ((name, (ExpKey.pack tag ())), (ExpFun.pack tag f)) in
  expanders_table := (elt :: (expanders_table.contents))
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
let expand loc (quotation : FanToken.quotation) tag =
  let open FanToken in
    let pos_tag = DynAst.string_of_tag tag in
    let name = quotation.q_name in
    let find name tag =
      let key =
        ((expander_name ~pos:(DynAst.string_of_tag tag) name),
          (ExpKey.pack tag ())) in
      (try
         let pack = List.assoc key expanders_table.contents in
         fun ()  -> ExpFun.unpack tag pack
       with
       | Not_found  ->
           (fun ()  ->
              if name = ""
              then
                raise
                  (FanLoc.Exc_located
                     (loc,
                       (QuotationError (name, pos_tag, NoName, Not_found))))
              else raise Not_found)
       | e -> (fun ()  -> raise e)) () in
    (try
       let expander = find name tag
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
    bprintf ppf "@?@[<2>While %s %S in a position of %S:" x name position in
  let () =
    match ctx with
    | Finding  ->
        (pp "finding quotation";
         bprintf ppf "@ @[<hv2>Available quotation expanders are:@\n";
         List.iter
           (fun ((s,t),_)  ->
              bprintf ppf "@[<2>%s@ (in@ a@ position@ of %a)@]@ " s
                ExpKey.print_tag t) expanders_table.contents;
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
    let exp_ast = expand_expr loc loc_name_opt s in StExp (loc, exp_ast) in
  let expand_patt _loc loc_name_opt s =
    Ref.protect FanConfig.antiquotations true
      (fun _  ->
         let ast = Gram.parse_string entry_eoi ~loc:_loc s in
         let meta_ast = mpatt _loc ast in
         let exp_ast = patt_filter meta_ast in
         let rec subst_first_loc name =
           function
           | PaApp (_loc,PaId (_,IdUid (_,u)),_) ->
               PaApp
                 (_loc, (PaId (_loc, (IdUid (_loc, u)))),
                   (PaId (_loc, (IdLid (_loc, name)))))
           | PaApp (_loc,a,b) -> PaApp (_loc, (subst_first_loc name a), b)
           | p -> p in
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
let of_str_item ~name  ~entry  =
  add name DynAst.str_item_tag (make_parser entry)
let of_str_item_with_filter ~name  ~entry  ~filter  =
  add name DynAst.str_item_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser entry loc loc_name_opt s))
let of_patt ~name  ~entry  = add name DynAst.patt_tag (make_parser entry)
let of_patt_with_filter ~name  ~entry  ~filter  =
  add name DynAst.str_item_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser entry loc loc_name_opt s))
let of_class_str_item ~name  ~entry  =
  add name DynAst.class_str_item_tag (make_parser entry)
let of_class_str_item_with_filter ~name  ~entry  ~filter  =
  add name DynAst.str_item_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser entry loc loc_name_opt s))
let of_match_case ~name  ~entry  =
  add name DynAst.match_case_tag (make_parser entry)
let of_match_case_with_filter ~name  ~entry  ~filter  =
  add name DynAst.str_item_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser entry loc loc_name_opt s))
let of_expr ~name  ~entry  =
  let expand_fun = make_parser entry in
  let mk_fun loc loc_name_opt s =
    StExp (loc, (expand_fun loc loc_name_opt s)) in
  add name DynAst.expr_tag expand_fun; add name DynAst.str_item_tag mk_fun
let of_expr_with_filter ~name  ~entry  ~filter  =
  let expand_fun loc loc_name_opt s =
    filter (make_parser entry loc loc_name_opt s) in
  let mk_fun loc loc_name_opt s =
    StExp (loc, (expand_fun loc loc_name_opt s)) in
  add name DynAst.expr_tag expand_fun; add name DynAst.str_item_tag mk_fun
module MetaLocQuotation =
  struct
  let meta_loc_expr _loc loc =
    match current_loc_name.contents with
    | None  -> ExId (_loc, (IdLid (_loc, (FanLoc.name.contents))))
    | Some "here" -> MetaLoc.meta_loc_expr _loc loc
    | Some x -> ExId (_loc, (IdLid (_loc, x)))
  let meta_loc_patt _loc _ = PaAny _loc
  end
let antiquot_expander ~parse_patt  ~parse_expr  =
  object 
    inherit  Ast.map as super
    method! patt =
      function
      | PaAnt (_loc,s)|PaStr (_loc,s) as p ->
          let mloc _loc = MetaLocQuotation.meta_loc_patt _loc _loc in
          handle_antiquot_in_string ~s ~default:p ~parse:parse_patt ~loc:_loc
            ~decorate:(fun n  e  ->
                         let len = String.length n in
                         match n with
                         | "antisig_item" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "SgAnt")))),
                                      (mloc _loc))), e)
                         | "antistr_item" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "StAnt")))),
                                      (mloc _loc))), e)
                         | "antictyp" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "TyAnt")))),
                                      (mloc _loc))), e)
                         | "antipatt" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "PaAnt")))),
                                      (mloc _loc))), e)
                         | "antiexpr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "ExAnt")))),
                                      (mloc _loc))), e)
                         | "antimodule_type" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "MtAnt")))),
                                      (mloc _loc))), e)
                         | "antimodule_expr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "MeAnt")))),
                                      (mloc _loc))), e)
                         | "anticlass_type" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "CtAnt")))),
                                      (mloc _loc))), e)
                         | "anticlass_expr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "CeAnt")))),
                                      (mloc _loc))), e)
                         | "anticlass_sig_item" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "CgAnt")))),
                                      (mloc _loc))), e)
                         | "anticlass_str_item" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "CrAnt")))),
                                      (mloc _loc))), e)
                         | "antiwith_constr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "WcAnt")))),
                                      (mloc _loc))), e)
                         | "antibinding" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "BiAnt")))),
                                      (mloc _loc))), e)
                         | "antirec_binding" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "RbAnt")))),
                                      (mloc _loc))), e)
                         | "antimatch_case" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "McAnt")))),
                                      (mloc _loc))), e)
                         | "antimodule_binding" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "MbAnt")))),
                                      (mloc _loc))), e)
                         | "antiident" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "IdAnt")))),
                                      (mloc _loc))), e)
                         | "tupexpr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "ExTup")))),
                                      (mloc _loc))), e)
                         | "tuppatt" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "PaTup")))),
                                      (mloc _loc))), e)
                         | "seqexpr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "ExSeq")))),
                                      (mloc _loc))), e)
                         | "uidexpr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "IdUid")))),
                                      (mloc _loc))), e)
                         | "lidexpr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "IdLid")))),
                                      (mloc _loc))), e)
                         | "uidident" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "IdUid")))),
                                      (mloc _loc))), e)
                         | "lidident" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "IdLid")))),
                                      (mloc _loc))), e)
                         | "flopatt" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "PaFlo")))),
                                      (mloc _loc))), e)
                         | "intpatt" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "PaInt")))),
                                      (mloc _loc))), e)
                         | "int32patt" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "PaInt32")))),
                                      (mloc _loc))), e)
                         | "int64patt" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "PaInt64")))),
                                      (mloc _loc))), e)
                         | "nativeintpatt" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId
                                         (_loc,
                                           (IdUid (_loc, "PaNativeInt")))),
                                      (mloc _loc))), e)
                         | "chrpatt" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "PaChr")))),
                                      (mloc _loc))), e)
                         | "strpatt" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "PaStr")))),
                                      (mloc _loc))), e)
                         | "strexpr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "ExStr")))),
                                      (mloc _loc))), e)
                         | "chrexpr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "ExChr")))),
                                      (mloc _loc))), e)
                         | "intexpr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "ExInt")))),
                                      (mloc _loc))), e)
                         | "int32expr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "ExInt32")))),
                                      (mloc _loc))), e)
                         | "int64expr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "ExInt64")))),
                                      (mloc _loc))), e)
                         | "floexpr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId (_loc, (IdUid (_loc, "ExFlo")))),
                                      (mloc _loc))), e)
                         | "nativeintexpr" ->
                             PaApp
                               (_loc,
                                 (PaApp
                                    (_loc,
                                      (PaId
                                         (_loc,
                                           (IdUid (_loc, "ExNativeInt")))),
                                      (mloc _loc))), e)
                         | x when (len > 0) && ((x.[0]) = '`') ->
                             failwith (x ^ "is not allowed in pattern")
                         | _ -> e)
      | p -> super#patt p
    method! expr =
      function
      | ExAnt (_loc,s)|ExStr (_loc,s) as e ->
          let mloc _loc = MetaLocQuotation.meta_loc_expr _loc _loc in
          handle_antiquot_in_string ~s ~default:e ~parse:parse_expr ~loc:_loc
            ~decorate:(fun n  e  ->
                         match n with
                         | "tupexpr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExTup")))),
                                      (mloc _loc))), e)
                         | "tuppatt" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "PaTup")))),
                                      (mloc _loc))), e)
                         | "seqexpr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExSeq")))),
                                      (mloc _loc))), e)
                         | "uidexpr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "IdUid")))),
                                      (mloc _loc))), e)
                         | "lidexpr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "IdLid")))),
                                      (mloc _loc))), e)
                         | "uidident" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "IdUid")))),
                                      (mloc _loc))), e)
                         | "lidident" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "IdLid")))),
                                      (mloc _loc))), e)
                         | "strexpr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExStr")))),
                                      (mloc _loc))), e)
                         | "chrexpr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExChr")))),
                                      (mloc _loc))), e)
                         | "intexpr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExInt")))),
                                      (mloc _loc))), e)
                         | "int32expr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExInt32")))),
                                      (mloc _loc))), e)
                         | "int64expr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExInt64")))),
                                      (mloc _loc))), e)
                         | "floexpr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExFlo")))),
                                      (mloc _loc))), e)
                         | "nativeintexpr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId
                                         (_loc,
                                           (IdUid (_loc, "ExNativeInt")))),
                                      (mloc _loc))), e)
                         | "`nativeintexpr" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc,
                                        (IdAcc
                                           (_loc,
                                             (IdUid (_loc, "Nativeint")),
                                             (IdLid (_loc, "to_string")))))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId
                                         (_loc,
                                           (IdUid (_loc, "ExNativeInt")))),
                                      (mloc _loc))), e)
                         | "`intexpr" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc, (IdLid (_loc, "string_of_int")))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExInt")))),
                                      (mloc _loc))), e)
                         | "`int32expr" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc,
                                        (IdAcc
                                           (_loc, (IdUid (_loc, "Int32")),
                                             (IdLid (_loc, "to_string")))))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExInt32")))),
                                      (mloc _loc))), e)
                         | "`int64expr" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc,
                                        (IdAcc
                                           (_loc, (IdUid (_loc, "Int64")),
                                             (IdLid (_loc, "to_string")))))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExInt64")))),
                                      (mloc _loc))), e)
                         | "`chrexpr" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc,
                                        (IdAcc
                                           (_loc, (IdUid (_loc, "Char")),
                                             (IdLid (_loc, "escaped")))))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExChr")))),
                                      (mloc _loc))), e)
                         | "`strexpr" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc,
                                        (IdAcc
                                           (_loc, (IdUid (_loc, "Ast")),
                                             (IdLid
                                                (_loc, "safe_string_escaped")))))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExStr")))),
                                      (mloc _loc))), e)
                         | "`floexpr" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc,
                                        (IdAcc
                                           (_loc, (IdUid (_loc, "FanUtil")),
                                             (IdLid (_loc, "float_repres")))))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExFlo")))),
                                      (mloc _loc))), e)
                         | "`boolexpr" ->
                             let x =
                               ExApp
                                 (_loc,
                                   (ExApp
                                      (_loc,
                                        (ExId (_loc, (IdUid (_loc, "IdLid")))),
                                        (mloc _loc))),
                                   (ExIfe
                                      (_loc, e, (ExStr (_loc, "true")),
                                        (ExStr (_loc, "false"))))) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExId")))),
                                      (ExId (_loc, (IdLid (_loc, "_loc")))))),
                                 x)
                         | "flopatt" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "PaFlo")))),
                                      (mloc _loc))), e)
                         | "intpatt" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "PaInt")))),
                                      (mloc _loc))), e)
                         | "int32patt" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "PaInt32")))),
                                      (mloc _loc))), e)
                         | "int64patt" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "PaInt64")))),
                                      (mloc _loc))), e)
                         | "nativeintpatt" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId
                                         (_loc,
                                           (IdUid (_loc, "PaNativeInt")))),
                                      (mloc _loc))), e)
                         | "chrpatt" ->
                             ExApp
                               (_loc, (ExId (_loc, (IdUid (_loc, "PaChr")))),
                                 (ExTup
                                    (_loc, (ExCom (_loc, (mloc _loc), e)))))
                         | "strpatt" ->
                             ExApp
                               (_loc, (ExId (_loc, (IdUid (_loc, "PaStr")))),
                                 (ExTup
                                    (_loc, (ExCom (_loc, (mloc _loc), e)))))
                         | "`nativeintpatt" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc,
                                        (IdAcc
                                           (_loc,
                                             (IdUid (_loc, "Nativeint")),
                                             (IdLid (_loc, "to_string")))))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId
                                         (_loc,
                                           (IdUid (_loc, "PaNativeInt")))),
                                      (mloc _loc))), e)
                         | "`intpatt" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc, (IdLid (_loc, "string_of_int")))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "PaInt")))),
                                      (mloc _loc))), e)
                         | "`int32patt" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc,
                                        (IdAcc
                                           (_loc, (IdUid (_loc, "Int32")),
                                             (IdLid (_loc, "to_string")))))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "PaInt32")))),
                                      (mloc _loc))), e)
                         | "`int64patt" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc,
                                        (IdAcc
                                           (_loc, (IdUid (_loc, "Int64")),
                                             (IdLid (_loc, "to_string")))))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "PaInt64")))),
                                      (mloc _loc))), e)
                         | "`chrpatt" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc,
                                        (IdAcc
                                           (_loc, (IdUid (_loc, "Char")),
                                             (IdLid (_loc, "escaped")))))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "PaChr")))),
                                      (mloc _loc))), e)
                         | "`strpatt" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc,
                                        (IdAcc
                                           (_loc, (IdUid (_loc, "Ast")),
                                             (IdLid
                                                (_loc, "safe_string_escaped")))))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "PaStr")))),
                                      (mloc _loc))), e)
                         | "`flopatt" ->
                             let e =
                               ExApp
                                 (_loc,
                                   (ExId
                                      (_loc,
                                        (IdAcc
                                           (_loc, (IdUid (_loc, "FanUtil")),
                                             (IdLid (_loc, "float_repres")))))),
                                   e) in
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "PaFlo")))),
                                      (mloc _loc))), e)
                         | "liststr_item" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "stSem_of_list")))))),
                                 e)
                         | "listsig_item" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "sgSem_of_list")))))),
                                 e)
                         | "listclass_sig_item" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "cgSem_of_list")))))),
                                 e)
                         | "listclass_str_item" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "crSem_of_list")))))),
                                 e)
                         | "listmodule_expr" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "meApp_of_list")))))),
                                 e)
                         | "listmodule_type" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "mtApp_of_list")))))),
                                 e)
                         | "listmodule_binding" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "mbAnd_of_list")))))),
                                 e)
                         | "listbinding" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "biAnd_of_list")))))),
                                 e)
                         | "listbinding;" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "biSem_of_list")))))),
                                 e)
                         | "listrec_binding" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "rbSem_of_list")))))),
                                 e)
                         | "listclass_type" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "ctAnd_of_list")))))),
                                 e)
                         | "listclass_expr" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "ceAnd_of_list")))))),
                                 e)
                         | "listident" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "idAcc_of_list")))))),
                                 e)
                         | "listctypand" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "tyAnd_of_list")))))),
                                 e)
                         | "listctyp;" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "tySem_of_list")))))),
                                 e)
                         | "listctyp*" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "tySta_of_list")))))),
                                 e)
                         | "listctyp|" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "tyOr_of_list")))))),
                                 e)
                         | "listctyp," ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "tyCom_of_list")))))),
                                 e)
                         | "listctyp&" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "tyAmp_of_list")))))),
                                 e)
                         | "listwith_constr" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "wcAnd_of_list")))))),
                                 e)
                         | "listmatch_case" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "mcOr_of_list")))))),
                                 e)
                         | "antimatch_case" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "McAnt")))),
                                      (mloc _loc))), e)
                         | "listmatch_caselettry" ->
                             ExApp
                               (_loc,
                                 (ExSnd
                                    (_loc,
                                      (ExId
                                         (_loc,
                                           (IdAcc
                                              (_loc, (IdUid (_loc, "Ast")),
                                                (IdLid (_loc, "match_pre")))))),
                                      "match_case")),
                                 (ExApp
                                    (_loc,
                                      (ExId
                                         (_loc,
                                           (IdAcc
                                              (_loc, (IdUid (_loc, "Ast")),
                                                (IdLid (_loc, "mcOr_of_list")))))),
                                      e)))
                         | "antimatch_caselettry" ->
                             ExApp
                               (_loc,
                                 (ExSnd
                                    (_loc,
                                      (ExId
                                         (_loc,
                                           (IdAcc
                                              (_loc, (IdUid (_loc, "Ast")),
                                                (IdLid (_loc, "match_pre")))))),
                                      "match_case")),
                                 (ExApp
                                    (_loc,
                                      (ExApp
                                         (_loc,
                                           (ExId
                                              (_loc, (IdUid (_loc, "McAnt")))),
                                           (mloc _loc))), e)))
                         | "match_caselettry" ->
                             ExApp
                               (_loc,
                                 (ExSnd
                                    (_loc,
                                      (ExId
                                         (_loc,
                                           (IdAcc
                                              (_loc, (IdUid (_loc, "Ast")),
                                                (IdLid (_loc, "match_pre")))))),
                                      "match_case")), e)
                         | "listpatt," ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "paCom_of_list")))))),
                                 e)
                         | "listpatt;" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "paSem_of_list")))))),
                                 e)
                         | "listexpr," ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "exCom_of_list")))))),
                                 e)
                         | "listexpr;" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "exSem_of_list")))))),
                                 e)
                         | "listforall" ->
                             ExApp
                               (_loc,
                                 (ExId
                                    (_loc,
                                      (IdAcc
                                         (_loc, (IdUid (_loc, "Ast")),
                                           (IdLid (_loc, "tyVarApp_of_list")))))),
                                 e)
                         | "antisig_item" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "SgAnt")))),
                                      (mloc _loc))), e)
                         | "antistr_item" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "StAnt")))),
                                      (mloc _loc))), e)
                         | "antictyp" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "TyAnt")))),
                                      (mloc _loc))), e)
                         | "antipatt" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "PaAnt")))),
                                      (mloc _loc))), e)
                         | "antiexpr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "ExAnt")))),
                                      (mloc _loc))), e)
                         | "antimodule_type" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "MtAnt")))),
                                      (mloc _loc))), e)
                         | "antimodule_expr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "MeAnt")))),
                                      (mloc _loc))), e)
                         | "anticlass_type" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "CtAnt")))),
                                      (mloc _loc))), e)
                         | "anticlass_expr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "CeAnt")))),
                                      (mloc _loc))), e)
                         | "anticlass_sig_item" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "CgAnt")))),
                                      (mloc _loc))), e)
                         | "anticlass_str_item" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "CrAnt")))),
                                      (mloc _loc))), e)
                         | "antiwith_constr" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "WcAnt")))),
                                      (mloc _loc))), e)
                         | "antibinding" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "BiAnt")))),
                                      (mloc _loc))), e)
                         | "antirec_binding" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "RbAnt")))),
                                      (mloc _loc))), e)
                         | "antimodule_binding" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "MbAnt")))),
                                      (mloc _loc))), e)
                         | "antiident" ->
                             ExApp
                               (_loc,
                                 (ExApp
                                    (_loc,
                                      (ExId (_loc, (IdUid (_loc, "IdAnt")))),
                                      (mloc _loc))), e)
                         | "antidirection_flag" ->
                             ExApp
                               (_loc, (ExId (_loc, (IdUid (_loc, "DiAnt")))),
                                 e)
                         | "antioverride_flag" ->
                             ExApp
                               (_loc, (ExId (_loc, (IdUid (_loc, "OvAnt")))),
                                 e)
                         | "antiprivate_flag" ->
                             ExApp
                               (_loc, (ExId (_loc, (IdUid (_loc, "PrAnt")))),
                                 e)
                         | "antimutable_flag" ->
                             ExApp
                               (_loc, (ExId (_loc, (IdUid (_loc, "MuAnt")))),
                                 e)
                         | "antivirtual_flag" ->
                             ExApp
                               (_loc, (ExId (_loc, (IdUid (_loc, "ViAnt")))),
                                 e)
                         | "antirow_var_flag" ->
                             ExApp
                               (_loc, (ExId (_loc, (IdUid (_loc, "RvAnt")))),
                                 e)
                         | "antirec_flag" ->
                             ExApp
                               (_loc, (ExId (_loc, (IdUid (_loc, "ReAnt")))),
                                 e)
                         | _ -> e)
      | e -> super#expr e
  end