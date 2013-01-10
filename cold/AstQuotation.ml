open Ast
open LibUtil
open FanUtil
open Lib.Meta
open Format
let _ = ()
type quotation_error_message =  
  | Finding
  | Expanding
  | ParsingResult of FanLoc.t* string
  | NoName 
type quotation_error = (string* string* quotation_error_message* exn) 
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
let default = ref ""
let map = ref SMap.empty
let update (pos,str) = map := (SMap.add pos str map.contents)
let translate = ref (fun x  -> x)
let clear_map () = map := SMap.empty
let clear_default () = default := ""
let default_at_pos pos str = update (pos, str)
type key = (string* ExpKey.pack) 
module QMap = MapMake(struct type t = key 
                             let compare = compare end)
let expanders_table = ref QMap.empty
let set_default s = default := s
let expander_name ~pos:(pos : string)  (name : string) =
  let u = translate.contents name in
  if u = ""
  then SMap.find_default ~default:(default.contents) pos map.contents
  else u
let add name (tag : 'a DynAst.tag) (f : 'a expand_fun) =
  let (k,v) = ((name, (ExpKey.pack tag ())), (ExpFun.pack tag f)) in
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
          if name = ""
          then
            raise
              (FanLoc.raise loc
                 (QuotationError (name, pos_tag, NoName, Not_found)))
          else raise Not_found)
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
    bprintf ppf "@?@[<2>While %s %S in a position of %S:" x name position in
  let () =
    match ctx with
    | Finding  ->
        (pp "finding quotation";
         bprintf ppf "@ @[<hv2>Available quotation expanders are:@\n";
         QMap.iter
           (fun (s,t)  _  ->
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
    let exp_ast = expand_expr loc loc_name_opt s in `StExp (loc, exp_ast) in
  let expand_patt _loc loc_name_opt s =
    Ref.protect FanConfig.antiquotations true
      (fun _  ->
         let ast = Gram.parse_string entry_eoi ~loc:_loc s in
         let meta_ast = mpatt _loc ast in
         let exp_ast = patt_filter meta_ast in
         let rec subst_first_loc name =
           (function
            | `PaApp (loc,`PaVrn (_,u),`PaTup (_,`PaCom (_,_,rest))) ->
                `PaApp
                  (loc, (`PaVrn (loc, u)),
                    (`PaTup
                       (loc,
                         (`PaCom
                            (loc, (`Id (_loc, (`Lid (_loc, name)))), rest)))))
            | `PaApp (_loc,`PaVrn (_,u),`Any _) ->
                `PaApp
                  (_loc, (`PaVrn (_loc, u)),
                    (`Id (_loc, (`Lid (_loc, name)))))
            | `PaApp (_loc,a,b) -> `PaApp (_loc, (subst_first_loc name a), b)
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
let of_match_case ~name  ~entry  =
  add name DynAst.match_case_tag (make_parser entry)
let of_match_case_with_filter ~name  ~entry  ~filter  =
  add name DynAst.match_case_tag
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
module MetaLocQuotation =
  struct
    let meta_loc_expr _loc loc =
      match current_loc_name.contents with
      | None  -> `Id (_loc, (`Lid (_loc, (FanLoc.name.contents))))
      | Some "here" -> MetaLoc.meta_loc_expr _loc loc
      | Some x -> `Id (_loc, (`Lid (_loc, x)))
    let meta_loc_patt _loc _ = `Any _loc
  end
let gm () =
  match FanConfig.compilation_unit.contents with
  | Some "FanAst" -> ""
  | Some _ -> "FanAst"
  | None  -> "FanAst"
let antiquot_expander ~parse_patt  ~parse_expr  =
  object 
    inherit  FanAst.map as super
    method! patt =
      function
      | `Ant (_loc,s)|`Str (_loc,s) as p ->
          let mloc _loc = MetaLocQuotation.meta_loc_patt _loc _loc in
          handle_antiquot_in_string ~s ~default:p ~parse:parse_patt ~loc:_loc
            ~decorate:(fun n  e  ->
                         let len = String.length n in
                         match n with
                         | "antisig_item" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "antistr_item" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "antictyp" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "antipatt" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "antiexpr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "antimodule_type" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "antimodule_expr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "anticlass_type" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "anticlass_expr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "anticlass_sig_item" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "anticlass_str_item" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "antiwith_constr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "antibinding" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "antirec_binding" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "antimatch_case" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "antimodule_binding" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "antiident" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Ant")),
                                      (mloc _loc))), e)
                         | "tupexpr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "ExTup")),
                                      (mloc _loc))), e)
                         | "tuppatt" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "PaTup")),
                                      (mloc _loc))), e)
                         | "seqexpr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Seq")),
                                      (mloc _loc))), e)
                         | "uidexpr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Uid")),
                                      (mloc _loc))), e)
                         | "lidexpr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Lid")),
                                      (mloc _loc))), e)
                         | "uidident" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Uid")),
                                      (mloc _loc))), e)
                         | "lidident" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Lid")),
                                      (mloc _loc))), e)
                         | "lida_lident" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Lid")),
                                      (mloc _loc))), e)
                         | "flopatt" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Flo")),
                                      (mloc _loc))), e)
                         | "intpatt" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Int")),
                                      (mloc _loc))), e)
                         | "int32patt" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Int32")),
                                      (mloc _loc))), e)
                         | "int64patt" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Int64")),
                                      (mloc _loc))), e)
                         | "nativeintpatt" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "NativeInt")),
                                      (mloc _loc))), e)
                         | "chrpatt" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Chr")),
                                      (mloc _loc))), e)
                         | "strpatt" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Str")),
                                      (mloc _loc))), e)
                         | "strexpr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Str")),
                                      (mloc _loc))), e)
                         | "chrexpr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Chr")),
                                      (mloc _loc))), e)
                         | "intexpr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Int")),
                                      (mloc _loc))), e)
                         | "int32expr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Int32")),
                                      (mloc _loc))), e)
                         | "int64expr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Int64")),
                                      (mloc _loc))), e)
                         | "floexpr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "Flo")),
                                      (mloc _loc))), e)
                         | "nativeintexpr" ->
                             `PaApp
                               (_loc,
                                 (`PaApp
                                    (_loc, (`PaVrn (_loc, "NativeInt")),
                                      (mloc _loc))), e)
                         | x when (len > 0) && ((x.[0]) = '`') ->
                             failwith (x ^ "is not allowed in pattern")
                         | _ -> e)
      | p -> super#patt p
    method! expr =
      function
      | `Ant (_loc,s)|`Str (_loc,s) as e ->
          let mloc _loc = MetaLocQuotation.meta_loc_expr _loc _loc in
          handle_antiquot_in_string ~s ~default:e ~parse:parse_expr ~loc:_loc
            ~decorate:(fun n  e  ->
                         match n with
                         | "tupexpr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "ExTup")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "tuppatt" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "PaTup")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "seqexpr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Seq")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "uidexpr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Uid")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "lidexpr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Lid")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "uidident" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Uid")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "lidident" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Lid")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "strexpr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Str")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "chrexpr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Chr")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "intexpr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Int")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "int32expr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Int32")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "int64expr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Int64")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "floexpr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Flo")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "nativeintexpr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "NativeInt")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`nativeintexpr" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, "Nativeint")),
                                             (`Lid (_loc, "to_string")))))),
                                   e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "NativeInt")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`intexpr" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc, (`Lid (_loc, "string_of_int")))),
                                   e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Int")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`int32expr" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, "Int32")),
                                             (`Lid (_loc, "to_string")))))),
                                   e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Int32")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`int64expr" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, "Int64")),
                                             (`Lid (_loc, "to_string")))))),
                                   e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Int64")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`chrexpr" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, "Char")),
                                             (`Lid (_loc, "escaped")))))), e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Chr")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`strexpr" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, (gm ()))),
                                             (`Lid
                                                (_loc, "safe_string_escaped")))))),
                                   e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Str")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`floexpr" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, "FanUtil")),
                                             (`Lid (_loc, "float_repres")))))),
                                   e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Flo")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`boolexpr" ->
                             let x =
                               `ExApp
                                 (_loc, (`ExVrn (_loc, "Lid")),
                                   (`ExTup
                                      (_loc,
                                        (`ExCom
                                           (_loc, (mloc _loc),
                                             (`IfThenElse
                                                (_loc, e,
                                                  (`Str (_loc, "true")),
                                                  (`Str (_loc, "false"))))))))) in
                             `ExApp
                               (_loc,
                                 (`ExApp
                                    (_loc, (`ExVrn (_loc, "Id")),
                                      (`Id (_loc, (`Lid (_loc, "_loc")))))),
                                 x)
                         | "flopatt" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Flo")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "intpatt" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Int")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "int32patt" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Int32")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "int64patt" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Int64")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "nativeintpatt" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "NativeInt")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "chrpatt" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Chr")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "strpatt" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Str")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`nativeintpatt" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, "Nativeint")),
                                             (`Lid (_loc, "to_string")))))),
                                   e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "NativeInt")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`intpatt" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc, (`Lid (_loc, "string_of_int")))),
                                   e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Int")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`int32patt" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, "Int32")),
                                             (`Lid (_loc, "to_string")))))),
                                   e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Int32")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`int64patt" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, "Int64")),
                                             (`Lid (_loc, "to_string")))))),
                                   e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Int64")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`chrpatt" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, "Char")),
                                             (`Lid (_loc, "escaped")))))), e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Chr")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`strpatt" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, (gm ()))),
                                             (`Lid
                                                (_loc, "safe_string_escaped")))))),
                                   e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Str")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "`flopatt" ->
                             let e =
                               `ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`IdAcc
                                           (_loc, (`Uid (_loc, "FanUtil")),
                                             (`Lid (_loc, "float_repres")))))),
                                   e) in
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Flo")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "liststr_item" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "stSem_of_list")))))),
                                 e)
                         | "listsig_item" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "sgSem_of_list")))))),
                                 e)
                         | "listclass_sig_item" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "cgSem_of_list")))))),
                                 e)
                         | "listclass_str_item" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "crSem_of_list")))))),
                                 e)
                         | "listmodule_expr" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "meApp_of_list")))))),
                                 e)
                         | "listmodule_type" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "mtApp_of_list")))))),
                                 e)
                         | "listmodule_binding" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "mbAnd_of_list")))))),
                                 e)
                         | "listbinding" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "biAnd_of_list")))))),
                                 e)
                         | "listbinding;" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "biSem_of_list")))))),
                                 e)
                         | "listrec_binding" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "rbSem_of_list")))))),
                                 e)
                         | "listclass_type" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "ctAnd_of_list")))))),
                                 e)
                         | "listclass_expr" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "ceAnd_of_list")))))),
                                 e)
                         | "listident" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "idAcc_of_list")))))),
                                 e)
                         | "listctypand" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "tyAnd_of_list")))))),
                                 e)
                         | "listctyp;" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "tySem_of_list")))))),
                                 e)
                         | "listctyp*" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "tySta_of_list")))))),
                                 e)
                         | "listctyp|" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "tyOr_of_list")))))),
                                 e)
                         | "listctyp," ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "tyCom_of_list")))))),
                                 e)
                         | "listctyp&" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "tyAmp_of_list")))))),
                                 e)
                         | "listwith_constr" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "wcAnd_of_list")))))),
                                 e)
                         | "listmatch_case" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "mcOr_of_list")))))),
                                 e)
                         | "antimatch_case" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "listmatch_caselettry" ->
                             `ExApp
                               (_loc,
                                 (`Send
                                    (_loc,
                                      (`Id
                                         (_loc,
                                           (`IdAcc
                                              (_loc, (`Uid (_loc, (gm ()))),
                                                (`Lid (_loc, "match_pre")))))),
                                      "match_case")),
                                 (`ExApp
                                    (_loc,
                                      (`Id
                                         (_loc,
                                           (`IdAcc
                                              (_loc, (`Uid (_loc, (gm ()))),
                                                (`Lid (_loc, "mcOr_of_list")))))),
                                      e)))
                         | "antimatch_caselettry" ->
                             `ExApp
                               (_loc,
                                 (`Send
                                    (_loc,
                                      (`Id
                                         (_loc,
                                           (`IdAcc
                                              (_loc, (`Uid (_loc, (gm ()))),
                                                (`Lid (_loc, "match_pre")))))),
                                      "match_case")),
                                 (`ExApp
                                    (_loc, (`ExVrn (_loc, "Ant")),
                                      (`ExTup
                                         (_loc,
                                           (`ExCom (_loc, (mloc _loc), e)))))))
                         | "match_caselettry" ->
                             `ExApp
                               (_loc,
                                 (`Send
                                    (_loc,
                                      (`Id
                                         (_loc,
                                           (`IdAcc
                                              (_loc, (`Uid (_loc, (gm ()))),
                                                (`Lid (_loc, "match_pre")))))),
                                      "match_case")), e)
                         | "listpatt," ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "paCom_of_list")))))),
                                 e)
                         | "listpatt;" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "paSem_of_list")))))),
                                 e)
                         | "listexpr," ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "exCom_of_list")))))),
                                 e)
                         | "listexpr;" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "exSem_of_list")))))),
                                 e)
                         | "listforall" ->
                             `ExApp
                               (_loc,
                                 (`Id
                                    (_loc,
                                      (`IdAcc
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Lid (_loc, "tyVarApp_of_list")))))),
                                 e)
                         | "antisig_item" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antistr_item" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antictyp" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antipatt" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antiexpr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antimodule_type" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antimodule_expr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "anticlass_type" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "anticlass_expr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "anticlass_sig_item" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "anticlass_str_item" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antiwith_constr" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antibinding" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antirec_binding" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antimodule_binding" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antiident" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antidirection_flag" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antioverride_flag" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antiprivate_flag" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antimutable_flag" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antivirtual_flag" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antirow_var_flag" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "antirec_flag" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Ant")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | "lida_lident" ->
                             `ExApp
                               (_loc, (`ExVrn (_loc, "Lid")),
                                 (`ExTup
                                    (_loc, (`ExCom (_loc, (mloc _loc), e)))))
                         | _ -> e)
      | e -> super#expr e
  end