
open LibUtil;
open FanUtil;
open Lib.Meta;


module type AntiquotSyntax = sig
    (**generally "expr; EOI". *)
  val parse_expr: FanLoc.t -> string -> Ast.expr;
    (**  generally "patt; EOI". *)
  val parse_patt: FanLoc.t -> string -> Ast.patt;

  val parse_ident: FanLoc.t -> string -> Ast.ident;  
end;

module type S = sig

  (** The [loc] is the initial location. The option string is the optional name
      for the location variable. The string is the quotation contents. *)
  type expand_fun 'a = FanLoc.t -> option string -> string -> 'a;

  (** [add name exp] adds the quotation [name] associated with the
      expander [exp]. *)
  val add : string -> DynAst.tag 'a -> expand_fun 'a -> unit;

  (* (\** [find name] returns the expander of the given quotation name. *\) *)
  (* val find : string -> DynAst.tag 'a -> expand_fun 'a; *)

  (** [default] holds the default quotation name. *)
  val default : ref string;

  (** [default_tbl] mapping position to the default quotation name
      it has higher precedence over default
   *)
  val default_tbl : Hashtbl.t string string ;

  (** [default_at_pos] set the default quotation name for specific pos*)
  val default_at_pos: string -> string -> unit;
      
  (** [parse_quotation_result parse_function loc position_tag quotation quotation_result]
      It's a parser wrapper, this function handles the error reporting for you. *)
    
  val parse_quotation_result :
    (FanLoc.t -> string -> 'a) -> FanLoc.t -> FanToken.quotation -> string -> string -> 'a;

  (** function translating quotation names; default = identity *)
  val translate : ref (string -> string);

  val expand : FanLoc.t -> FanToken.quotation -> DynAst.tag 'a -> 'a;

  (** [dump_file] optionally tells Camlp4 to dump the
      result of an expander if this result is syntactically incorrect.
      If [None] (default), this result is not dumped. If [Some fname], the
      result is dumped in the file [fname]. *)
  val dump_file : ref (option string);


  val add_quotation: string -> Gram.t 'a ->
    (FanLoc.t -> 'a -> Lib.Expr.Ast.expr) ->
      (FanLoc.t -> 'a -> Lib.Expr.Ast.patt) -> unit;

  (* BUG, revised parser can not parse name:string -> unit*)      
  val add_quotation_of_expr: ~name:string -> ~entry: Gram.t Ast.expr -> unit;
  val add_quotation_of_patt: ~name:string -> ~entry: Gram.t Ast.patt -> unit;
  val add_quotation_of_class_str_item: ~name:string -> ~entry: Gram.t Ast.class_str_item -> unit;
  val add_quotation_of_match_case: ~name:string -> ~entry: Gram.t Ast.match_case -> unit;
  val add_quotation_of_str_item: ~name:string -> ~entry: Gram.t Ast.str_item -> unit;
end;



open Format;

type expand_fun 'a = FanLoc.t -> option string -> string -> 'a;
module Exp_key = DynAst.Pack(struct
  type t 'a = unit;
end);

module Exp_fun = DynAst.Pack(struct
  type t 'a = expand_fun 'a;
end);
  
let expanders_table =
  (ref [] : ref (list ((string * Exp_key.pack) * Exp_fun.pack)));

let default = ref "";
  (* create a table mapping from
     (string_of_tag tag) to default quotation expander *)  
let default_tbl : Hashtbl.t string string = Hashtbl.create 50;
let translate = ref (fun x -> x);
(* intentionaly make its value a string to be more flexibile to
   incorporating more tags in the future
 *)  
let default_at_pos pos str =
  Hashtbl.replace default_tbl pos str;

(* First according the [position] to find the default language,
   if not found, then dispatched to default 
 *)  
let expander_name pos_tag name =
  let str = DynAst.string_of_tag pos_tag in 
  match !translate name with
  [ "" ->
    try Hashtbl.find default_tbl str
    with [Not_found -> !default]
  | name -> name ];


let current_loc_name = ref None  ;
let stack = Stack.create ();
let current_quot () =
  try Stack.pop stack
  with [Stack.Empty -> failwith "it's not in a quotation context"];
    
let add name tag f =
  let elt = ((name, Exp_key.pack tag ()), Exp_fun.pack tag f) in
  expanders_table := [elt :: !expanders_table];

let dump_file = ref None;


type quotation_error_message =
    [ Finding
    | Expanding
    | ParsingResult of FanLoc.t and string
    | NoName];

type quotation_error = (string * string * quotation_error_message * exn);

exception QuotationError of quotation_error;

let quotation_error_to_string (name, position, ctx, exn) =
  let ppf = Buffer.create 30 in
  let name = if name = "" then !default else name in
  let pp x = bprintf ppf "@?@[<2>While %s %S in a position of %S:" x name position in
    let () =
      match ctx with
      [ Finding -> begin
        pp "finding quotation";
        bprintf ppf "@ @[<hv2>Available quotation expanders are:@\n";
        List.iter begin fun ((s,t),_) ->
          bprintf ppf "@[<2>%s@ (in@ a@ position@ of %a)@]@ "
            s Exp_key.print_tag t
        end !expanders_table;
        bprintf ppf "@]";
        end
      | Expanding ->  pp "expanding quotation"
      | ParsingResult (loc, str) -> begin
          pp "parsing result of quotation" ;
          match !dump_file with
          [ Some dump_file ->
            let () = bprintf ppf " dumping result...\n" in
            try
              let oc = open_out_bin dump_file in
              begin
                output_string oc str;
                output_string oc "\n";
                flush oc;
                close_out oc;
                bprintf ppf "%a:" FanLoc.print (FanLoc.set_file_name dump_file loc);
              end
            with _ ->
              bprintf ppf
                "Error while dumping result in file %S; dump aborted"
                dump_file
           | None ->
                bprintf ppf
                  "\n(consider setting variable AstQuotation.dump_file, or using the -QD option)"
          ]
      end
      | NoName -> pp "No default quotation name" ] in
    let () = bprintf ppf "@\n%s@]@." (Printexc.to_string exn)in Buffer.contents ppf;

Printexc.register_printer (fun
  [ QuotationError x -> Some (quotation_error_to_string x )
  | _ -> None]);

let parse_quotation_result parse loc quot pos_tag str =
  let open FanToken in 
  try parse loc str with
  [ FanLoc.Exc_located (iloc, (QuotationError (n, pos_tag, Expanding, exc))) ->
      let ctx = ParsingResult iloc quot.q_contents in
      let exc1 = QuotationError (n, pos_tag, ctx, exc) in
      raise (FanLoc.Exc_located iloc exc1)
  | FanLoc.Exc_located (iloc, (QuotationError _ as exc)) ->
      raise (FanLoc.Exc_located iloc exc)
  | FanLoc.Exc_located (iloc, exc) ->
      let ctx = ParsingResult iloc quot.q_contents in
      let exc1 = QuotationError (quot.q_name, pos_tag, ctx, exc) in
      raise (FanLoc.Exc_located iloc exc1) ];

(* called by [expand] *)    
let expand_quotation loc expander pos_tag quot =
  debug quot "expand_quotation: name: %s, str: %S@." quot.q_name quot.q_contents in
  let open FanToken in
  let loc_name_opt = if quot.q_loc = "" then None else Some quot.q_loc in
  try expander loc loc_name_opt quot.q_contents with
  [ FanLoc.Exc_located (_, (QuotationError _)) as exc ->
      raise exc
  | FanLoc.Exc_located (iloc, exc) ->
      let exc1 = QuotationError (quot.q_name, pos_tag, Expanding, exc) in
      raise (FanLoc.Exc_located iloc exc1)
  | exc ->
      let exc1 = QuotationError (quot.q_name, pos_tag, Expanding, exc) in
      raise (FanLoc.Exc_located loc exc1) ];
    
let expand loc quotation tag =
  let open FanToken in 
  let pos_tag = DynAst.string_of_tag tag in
  let name = quotation.q_name in
  debug quot "handle_quotation: name: %s, str: %S@." name quotation.q_contents in
  let find name tag =
    let key = (expander_name tag name, Exp_key.pack tag ()) in
    let try pack = List.assoc key !expanders_table in
    Exp_fun.unpack tag pack
    with
      [Not_found ->
        if name="" then raise
            (FanLoc.Exc_located loc (QuotationError (name,pos_tag,NoName,Not_found)))
        else raise Not_found
     | e -> raise e ] in 
  let try expander = find name tag
  and loc = FanLoc.join (FanLoc.move `start quotation.q_shift loc)  in begin
    Stack.push  quotation.q_name stack;
    try  begin 
      let res = expand_quotation loc expander pos_tag quotation;
      ignore(Stack.pop stack);
      res 
    end
    with
      e -> begin ignore (Stack.pop stack) ; raise e end
  end
  with
    [ FanLoc.Exc_located (_, (QuotationError _)) as exc -> raise exc
    | FanLoc.Exc_located (qloc, exc) ->
        raise (FanLoc.Exc_located qloc (QuotationError (name, pos_tag, Finding, exc)))
    | exc ->
        raise (FanLoc.Exc_located loc (QuotationError (name, pos_tag, Finding, exc))) ];


let add_quotation ~expr_filter ~patt_filter  ~mexpr ~mpatt name entry  =
  let entry_eoi = Gram.eoi_entry entry in 
  let expand_expr loc loc_name_opt s =
    Ref.protect2 (FanConfig.antiquotations,true) (current_loc_name, loc_name_opt)
      (fun _ ->
        Gram.parse_string entry_eoi loc s |> mexpr loc |> expr_filter) in
  let expand_str_item loc loc_name_opt s =
    let exp_ast = expand_expr loc loc_name_opt s in
    {:str_item@loc| $(exp:exp_ast) |} in
  let expand_patt _loc loc_name_opt s =
    Ref.protect FanConfig.antiquotations true begin fun _ ->
      let ast = Gram.parse_string entry_eoi _loc s in
      let meta_ast = mpatt _loc ast in
      let exp_ast = patt_filter meta_ast in
      let rec subst_first_loc name =  with "patt" fun
        [ {@_loc| Ast.$uid:u $_ |} -> {| Ast.$uid:u $lid:name |}
        | {@_loc| $a $b |} -> {| $(subst_first_loc name a) $b |}
        | p -> p ] in 
      match loc_name_opt with
      [ None -> subst_first_loc (!FanLoc.name) exp_ast
      | Some "_" -> exp_ast
      | Some name -> subst_first_loc name exp_ast ]
    end in begin
        add name DynAst.expr_tag expand_expr;
        add name DynAst.patt_tag expand_patt;
        add name DynAst.str_item_tag expand_str_item;
    end;

let make_parser entry =
  fun loc loc_name_opt s  -> 
    Ref.protect2
      (FanConfig.antiquotations, true)
      (current_loc_name,loc_name_opt)
      (fun _ -> Gram.parse_string (Gram.eoi_entry entry) loc  s);
        
let add_quotation_of_str_item ~name ~entry =
  add name DynAst.str_item_tag (make_parser entry);

let add_quotation_of_str_item_with_filter ~name ~entry ~filter =
  add name DynAst.str_item_tag (filter (make_parser entry));


(* both [expr] and [str_item] positions are registered *)  
let add_quotation_of_expr ~name ~entry = 
  let expand_fun =  make_parser entry in
  let mk_fun loc loc_name_opt s =
    {:str_item@loc| $(exp:expand_fun loc loc_name_opt s) |} in begin 
      add name DynAst.expr_tag expand_fun ;
      add name DynAst.str_item_tag mk_fun ;
    end ;
let add_quotation_of_patt ~name ~entry =
  add name DynAst.patt_tag (make_parser entry);
  
let add_quotation_of_class_str_item ~name ~entry =
  add name DynAst.class_str_item_tag (make_parser entry);
  
let add_quotation_of_match_case ~name ~entry =
  add name DynAst.match_case_tag (make_parser  entry);
    
  
module MetaLocQuotation = struct
  let meta_loc_expr _loc loc =
    match !current_loc_name with
    [ None -> {:expr| $(lid:!FanLoc.name) |}
    | Some "here" -> MetaLoc.meta_loc_expr _loc loc
    | Some x -> {:expr| $lid:x |} ];
  let meta_loc_patt _loc _ =  {:patt| _ |}; (* we use [subst_first_loc] *)
end;

module MetaQAst = Camlp4Ast.Meta.Make MetaLocQuotation;
module ME = MetaQAst.Expr;
module MP = MetaQAst.Patt;

let antiquot_expander ~parse_patt ~parse_expr = object
  inherit Ast.map as super;
  method! patt =
    with "patt"
    fun
    [ {@_loc| $anti:s |} | {@_loc| $str:s |} as p ->
      let mloc _loc = MetaLocQuotation.meta_loc_patt _loc _loc in
      handle_antiquot_in_string ~s ~default:p ~parse:parse_patt ~loc:_loc
        ~decorate:(fun n e ->
          let len = String.length n in 
          match n with
          [ "antisig_item" -> {| Ast.SgAnt ($(mloc _loc), $e) |}
          | "antistr_item" -> {| Ast.StAnt ($(mloc _loc), $e) |}
          | "antictyp" -> {| Ast.TyAnt ($(mloc _loc), $e) |}
          | "antipatt" -> {| Ast.PaAnt ($(mloc _loc), $e) |}
          | "antiexpr" -> {| Ast.ExAnt ($(mloc _loc), $e) |}
          | "antimodule_type" -> {| Ast.MtAnt($(mloc _loc), $e) |}
          | "antimodule_expr" -> {| Ast.MeAnt ($(mloc _loc), $e) |}
          | "anticlass_type" -> {| Ast.CtAnt ($(mloc _loc), $e) |}
          | "anticlass_expr" -> {| Ast.CeAnt ($(mloc _loc), $e) |}
          | "anticlass_sig_item" -> {| Ast.CgAnt ($(mloc _loc), $e) |}
          | "anticlass_str_item" -> {| Ast.CrAnt ($(mloc _loc), $e) |}
          | "antiwith_constr" -> {| Ast.WcAnt ($(mloc _loc), $e) |}
          | "antibinding" -> {| Ast.BiAnt ($(mloc _loc), $e) |}
          | "antirec_binding" -> {| Ast.RbAnt ($(mloc _loc), $e) |}
          | "antimatch_case" -> {| Ast.McAnt ($(mloc _loc), $e) |}
          | "antimodule_binding" -> {| Ast.MbAnt ($(mloc _loc), $e) |}
          | "antiident" -> {| Ast.IdAnt ($(mloc _loc), $e) |}
          | "tupexpr" -> {|Ast.ExTup ($(mloc _loc), $e)|}
          | "tuppatt" -> {|Ast.PaTup ($(mloc _loc), $e)|}
          | "seqexpr" -> {|Ast.ExSeq ($(mloc _loc), $e) |}
                
          | "uidexpr" -> {| Ast.IdUid ($(mloc _loc), $e) |} (* use Ant instead *)
          | "lidexpr" -> {| Ast.IdLid ($(mloc _loc), $e) |}
                
          | "uidident" -> {| Ast.IdUid ($(mloc _loc), $e)|}
          | "lidident" -> {| Ast.IdLid ($(mloc _loc), $e)|}

          | "flopatt" -> {| Ast.PaFlo ($(mloc _loc), $e) |}
          | "intpatt" -> {| Ast.PaInt ($(mloc _loc), $e) |}
          | "int32patt" -> {| Ast.PaInt32 ($(mloc _loc), $e)|}
          | "int64patt" -> {| Ast.PaInt64 ($(mloc _loc), $e)|}
          | "nativeintpatt" -> {| Ast.PaNativeInt ($(mloc _loc),$e)|}
          | "chrpatt" -> {|Ast.PaChr ($(mloc _loc), $e) |}
          | "strpatt" -> {|Ast.PaStr ($(mloc _loc),$e) |}
                
          | "strexpr" -> {| Ast.ExStr ($(mloc _loc), $e) |}
          | "chrexpr" -> {| Ast.ExChr ($(mloc _loc), $e) |}
          | "intexpr" -> {| Ast.ExInt ($(mloc _loc), $e) |}
          | "int32expr" -> {| Ast.ExInt32 ($(mloc _loc), $e) |}
          | "int64expr" -> {|Ast.ExInt64 ($(mloc _loc), $e)|}
          | "floexpr" -> {| Ast.ExFlo ($(mloc _loc), $e) |}
          | "nativeintexpr" -> {|Ast.ExNativeInt ($(mloc _loc), $e) |}
          | x when (len > 0 && x.[0] = '`') -> failwith (x ^ "is not allowed in pattern")
          | _ -> e ])
      | p -> super#patt p ];
    method! expr = with "expr" fun (* ExAnt keeps the right location, ExStr does not *)
      [ {@_loc| $anti:s |} | {@_loc| $str:s |} as e ->
          let mloc _loc = MetaLocQuotation.meta_loc_expr _loc _loc in
          handle_antiquot_in_string ~s ~default:e ~parse:parse_expr ~loc:_loc
            ~decorate:(fun n e -> (* e is the parsed Ast node already *)
            match n with
            ["tupexpr" ->   {| Ast.ExTup $(mloc _loc) $e |}
            | "tuppatt" ->  {|Ast.PaTup $(mloc _loc) $e |}
            | "seqexpr" -> {| Ast.ExSeq $(mloc _loc) $e |}

            | "uidexpr" -> {| Ast.IdUid $(mloc _loc) $e |} (* use Ant instead *)
            | "lidexpr" -> {| Ast.IdLid $(mloc _loc) $e |}

            | "uidident"->  {| Ast.IdUid $(mloc _loc) $e |}
            | "lidident" -> {| Ast.IdLid $(mloc _loc) $e |}
            | "strexpr" -> {| Ast.ExStr $(mloc _loc) $e |}
            | "chrexpr" -> {| Ast.ExChr $(mloc _loc) $e |}
            | "intexpr" -> {| Ast.ExInt $(mloc _loc) $e |}
            | "int32expr" -> {| Ast.ExInt32 $(mloc _loc) $e |}
            | "int64expr" -> {|Ast.ExInt64 $(mloc _loc) $e|}
            | "floexpr" -> {| Ast.ExFlo $(mloc _loc) $e |}
            | "nativeintexpr" -> {|Ast.ExNativeInt $(mloc _loc) $e |}
            | "`nativeintexpr" ->
                let e = {| Nativeint.to_string $e |} in
                {|Ast.ExNativeInt $(mloc _loc) $e |}
            | "`intexpr" ->
                let e = {|string_of_int $e |} in
                {|Ast.ExInt $(mloc _loc) $e |}
            | "`int32expr" ->
                let e = {|Int32.to_string $e |} in
                {|Ast.ExInt32 $(mloc _loc) $e |}
            | "`int64expr" ->
                let e = {|Int64.to_string $e |} in
                {|Ast.ExInt64 $(mloc _loc) $e |}
            | "`chrexpr" ->
                let e = {|Char.escaped $e|} in
                {|Ast.ExChr $(mloc _loc) $e |}
            | "`strexpr" ->
                let e = {|Ast.safe_string_escaped $e |} in
                {|Ast.ExStr $(mloc _loc) $e |}
            | "`floexpr" ->
                let e = {| FanUtil.float_repres $e |} in 
                {|Ast.ExFlo $(mloc _loc) $e |}
            | "`boolexpr" ->
                let x = {|Ast.IdLid $(mloc _loc) (if $e then "true" else "false" ) |} in
                {| {| $(id:$x)  |} |}

            | "flopatt" -> {| Ast.PaFlo $(mloc _loc) $e |}
            | "intpatt" -> {| Ast.PaInt $(mloc _loc) $e |}
            | "int32patt" -> {| Ast.PaInt32 $(mloc _loc) $e|}
            | "int64patt" -> {| Ast.PaInt64 $(mloc _loc) $e|}
            | "nativeintpatt" -> {| Ast.PaNativeInt $(mloc _loc) $e|}
            | "chrpatt" -> {|Ast.PaChr ($(mloc _loc), $e) |}
            | "strpatt" -> {|Ast.PaStr ($(mloc _loc),$e) |}

            | "`nativeintpatt" ->
                let e = {| Nativeint.to_string $e |} in
                {|Ast.PaNativeInt $(mloc _loc) $e |}
            | "`intpatt" ->
                let e = {|string_of_int $e |} in
                {|Ast.PaInt $(mloc _loc) $e |}
            | "`int32patt" ->
                let e = {|Int32.to_string $e |} in
                {|Ast.PaInt32 $(mloc _loc) $e |}
            | "`int64patt" ->
                let e = {|Int64.to_string $e |} in
                {|Ast.PaInt64 $(mloc _loc) $e |}
            | "`chrpatt" ->
                let e = {|Char.escaped $e|} in
                {|Ast.PaChr $(mloc _loc) $e |}
            | "`strpatt" ->
                let e = {|Ast.safe_string_escaped $e |} in
                {|Ast.PaStr $(mloc _loc) $e |}
            | "`flopatt" ->
                let e = {| FanUtil.float_repres $e |} in 
                {|Ast.PaFlo $(mloc _loc) $e |}
            (* | "`boolpatt" -> *)
            (*     let x = {|Ast.IdLid $(mloc _loc) (if $e then "true" else "false" ) |} in *)
            (*     {| {:patt| $(id:$x)  |} |} *)
                  
            | "liststr_item" -> {| Ast.stSem_of_list $e |}
            | "listsig_item" -> {| Ast.sgSem_of_list $e |}
            | "listclass_sig_item" -> {| Ast.cgSem_of_list $e |}
            | "listclass_str_item" -> {| Ast.crSem_of_list $e |}
            | "listmodule_expr" -> {| Ast.meApp_of_list $e |}
            | "listmodule_type" -> {| Ast.mtApp_of_list $e |}
            | "listmodule_binding" -> {| Ast.mbAnd_of_list $e |}
            | "listbinding" -> {| Ast.biAnd_of_list $e |}
            | "listbinding;" -> {| Ast.biSem_of_list $e |}
            | "listrec_binding" -> {| Ast.rbSem_of_list $e |}
            | "listclass_type" -> {| Ast.ctAnd_of_list $e |}
            | "listclass_expr" -> {| Ast.ceAnd_of_list $e |}
            | "listident" -> {| Ast.idAcc_of_list $e |}
            | "listctypand" -> {| Ast.tyAnd_of_list $e |}
            | "listctyp;" -> {| Ast.tySem_of_list $e |}
            | "listctyp*" -> {| Ast.tySta_of_list $e |}
            | "listctyp|" -> {| Ast.tyOr_of_list $e |}
            | "listctyp," -> {| Ast.tyCom_of_list $e |}
            | "listctyp&" -> {| Ast.tyAmp_of_list $e |}
            | "listwith_constr" -> {| Ast.wcAnd_of_list $e |}

            (* staging problems here *)      
            | "listmatch_case" -> {| Ast.mcOr_of_list $e |}
            | "antimatch_case" -> {| Ast.McAnt $(mloc _loc) $e |}
            | "listmatch_caselettry" ->
                {| ((Ast.match_pre)#match_case (Ast.mcOr_of_list $e)) |}
            | "antimatch_caselettry" ->
                {| Ast.match_pre#match_case (Ast.McAnt $(mloc _loc) $e) |}
            | "match_caselettry" ->
                {| Ast.match_pre#match_case $e |}
                  
            | "listpatt," -> {| Ast.paCom_of_list $e |}
            | "listpatt;" -> {| Ast.paSem_of_list $e |}
            | "listexpr," -> {| Ast.exCom_of_list $e |}
            | "listexpr;" -> {| Ast.exSem_of_list $e |}
            | "listforall" -> {| Ast.tyVarApp_of_list $e |}
            | "antisig_item" -> {| Ast.SgAnt $(mloc _loc) $e |}
            | "antistr_item" -> {| Ast.StAnt $(mloc _loc) $e |}
            | "antictyp" -> {| Ast.TyAnt $(mloc _loc) $e |}
            | "antipatt" -> {| Ast.PaAnt $(mloc _loc) $e |}
            | "antiexpr" -> {| Ast.ExAnt $(mloc _loc) $e |}
            | "antimodule_type" -> {| Ast.MtAnt $(mloc _loc) $e |}
            | "antimodule_expr" -> {| Ast.MeAnt $(mloc _loc) $e |}
            | "anticlass_type" -> {| Ast.CtAnt $(mloc _loc) $e |}
            | "anticlass_expr" -> {| Ast.CeAnt $(mloc _loc) $e |}
            | "anticlass_sig_item" -> {| Ast.CgAnt $(mloc _loc) $e |}
            | "anticlass_str_item" -> {| Ast.CrAnt $(mloc _loc) $e |}
            | "antiwith_constr" -> {| Ast.WcAnt $(mloc _loc) $e |}
            | "antibinding" -> {| Ast.BiAnt $(mloc _loc) $e |}
            | "antirec_binding" -> {| Ast.RbAnt $(mloc _loc) $e |}

            | "antimodule_binding" -> {| Ast.MbAnt $(mloc _loc) $e |}
            | "antiident" -> {| Ast.IdAnt $(mloc _loc) $e |}
            | "antidirection_flag" -> {| Ast.DiAnt  $e |}
            | "antioverride_flag" -> {| Ast.OvAnt $e |}
            | "antiprivate_flag" -> {|Ast.PrAnt $e |}
            | "antimutable_flag" -> {|Ast.MuAnt $e|}
            | "antivirtual_flag" -> {|Ast.ViAnt $e|}
            | "antirow_var_flag" -> {|Ast.RvAnt $e|}
            | "antirec_flag" -> {|Ast.ReAnt $e|}
            | _ -> e ])
      | e -> super#expr e ];
  end;


  
(* let anti ~parse_patt ~parse_expr = object *)
(*   inherit Ast.map as super; *)
(*   method! patt = *)
(*     with "patt" *)
(*     fun *)
(*     [ {| $anti:s |} | {| $str:s |} as p -> *)
(*       let mloc _loc = MetaLocQuotation.meta_loc_patt _loc _loc in *)
(*       handle_antiquot_in_string ~s ~default:p ~parse:parse_patt ~loc:_loc *)
(*         ~decorate:(fun n e -> *)
(*           let len = String.length n in  *)
(*           match n with *)
(*           [ "tupexpr" -> {|Ast.ExTup ($(mloc _loc), $e)|} *)
(*           | "seqexpr" -> {|Ast.ExSeq ($(mloc _loc), $e) |} *)
(*           | "uidexpr" -> {| Ast.IdUid ($(mloc _loc), $e) |} (\* use Ant instead *\) *)
(*           | "lidexpr" -> {| Ast.IdLid ($(mloc _loc), $e) |} *)
(*           | "strexpr" -> {| Ast.ExStr ($(mloc _loc), $e) |} *)
(*           | "chrexpr" -> {| Ast.ExChr ($(mloc _loc), $e) |} *)
(*           | "intexpr" -> {| Ast.ExInt ($(mloc _loc), $e) |} *)
(*           | "int32expr" -> {| Ast.ExInt32 ($(mloc _loc), $e) |} *)
(*           | "int64expr" -> {|Ast.ExInt64 ($(mloc _loc), $e)|} *)
(*           | "floexpr" -> {| Ast.ExFlo ($(mloc _loc), $e) |} *)
(*           | "nativeintexpr" -> {|Ast.ExNativeInt ($(mloc _loc), $e) |} *)
(*           | x when (len > 0 && x.[0] = '`') -> failwith (x ^ "is not allowed in pattern") *)
(*           | _ -> e ]) *)
(*       | p -> super#patt p ]; *)
(*     method! expr = with "expr" fun (\* ExAnt keeps the right location, ExStr does not *\) *)
(*       [ {| $anti:s |} | {| $str:s |} as e -> *)
(*           let mloc _loc = MetaLocQuotation.meta_loc_expr _loc _loc in *)
(*           handle_antiquot_in_string ~s ~default:e ~parse:parse_expr ~loc:_loc *)
(*             ~decorate:(fun n e -> (\* e is the parsed Ast node already *\) *)
(*             match n with *)
(*             ["tupexpr" ->   {| Ast.ExTup $(mloc _loc) $e |} *)
(*             | "seqexpr" -> {| Ast.ExSeq $(mloc _loc) $e |} *)
(*             | "uidexpr" -> {| Ast.IdUid $(mloc _loc) $e |} (\* use Ant instead *\) *)
(*             | "lidexpr" -> {| Ast.IdLid $(mloc _loc) $e |} *)
(*             | "strexpr" -> {| Ast.ExStr $(mloc _loc) $e |} *)
(*             | "chrexpr" -> {| Ast.ExChr $(mloc _loc) $e |} *)
(*             | "intexpr" -> {| Ast.ExInt $(mloc _loc) $e |} *)
(*             | "int32expr" -> {| Ast.ExInt32 $(mloc _loc) $e |} *)
(*             | "int64expr" -> {|Ast.ExInt64 $(mloc _loc) $e|} *)
(*             | "floexpr" -> {| Ast.ExFlo $(mloc _loc) $e |} *)
(*             | "nativeintexpr" -> {|Ast.ExNativeInt $(mloc _loc) $e |} *)
(*             | "`nativeintexpr" -> *)
(*                 let e = {| Nativeint.to_string $e |} in *)
(*                 {|Ast.ExNativeInt $(mloc _loc) $e |} *)
(*             | "`intexpr" -> *)
(*                 let e = {|string_of_int $e |} in *)
(*                 {|Ast.ExInt $(mloc _loc) $e |} *)
(*             | "`int32expr" -> *)
(*                 let e = {|Int32.to_string $e |} in *)
(*                 {|Ast.ExInt32 $(mloc _loc) $e |} *)
(*             | "`int64expr" -> *)
(*                 let e = {|Int64.to_string $e |} in *)
(*                 {|Ast.ExInt64 $(mloc _loc) $e |} *)
(*             | "`chrexpr" -> *)
(*                 let e = {|Char.escaped $e|} in *)
(*                 {|Ast.ExChr $(mloc _loc) $e |} *)
(*             | "`strexpr" -> *)
(*                 let e = {|Ast.safe_string_escaped $e |} in *)
(*                 {|Ast.ExStr $(mloc _loc) $e |} *)
(*             | "`floexpr" -> *)
(*                 let e = {| FanUtil.float_repres $e |} in  *)
(*                 {|Ast.ExFlo $(mloc _loc) $e |} *)
(*             | "`boolexpr" -> *)
(*                 let x = {|Ast.IdLid $(mloc _loc) (if $e then "true" else "false" ) |} in *)
(*                 {| {| $(id:$x)  |} |} *)
(*             | "antiexpr" -> {| Ast.ExAnt $(mloc _loc) $e |} *)
(*             | _ -> e ]) *)
(*       | e -> super#expr e ]; *)
(*   end; *)

                (* {| {| $(id: $({|Ast.IdLid $(mloc _loc) (if $e then "true" else "false" ) |}))  |} |} *)
                  

                  (* {| $(lid:if e then "true" else "false") |} *)
                  (* {| {| $(lid:if $e then "true" else "false") |} |} *)

                  (* {:expr@here|$`bool:x|} *)
                  (*
                    let _ =
  Ast.ExApp
    (_loc,
      (Ast.ExApp
         (_loc,
           (Ast.ExId
              (_loc,
                (Ast.IdAcc
                   (_loc, (Ast.IdUid (_loc, "Ast")),
                     (Ast.IdUid (_loc, "ExId")))))),
           (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
      (Ast.ExApp
         (_loc,
           (Ast.ExApp
              (_loc,
                (Ast.ExId
                   (_loc,
                     (Ast.IdAcc
                        (_loc, (Ast.IdUid (_loc, "Ast")),
                          (Ast.IdUid (_loc, "IdLid")))))),
                (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
           (Ast.ExIfe
              (_loc, e, (Ast.ExStr (_loc, "true")),
                (Ast.ExStr (_loc, "false")))))))
                   *)
