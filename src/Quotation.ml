open Lib;
open LibUtil;
module type AntiquotSyntax = sig
    (**generally "expr; EOI". *)
  val parse_expr : FanLoc.t -> string -> Ast.expr;
    (**  generally "patt; EOI". *)
  val parse_patt : FanLoc.t -> string -> Ast.patt;
end;

module type S = sig
  (* module Ast : Camlp4Ast; *)
  (* module DynAst : DynAst (\* with module Ast = Ast *\); *)
  (* open Ast; *)

  (** The [loc] is the initial location. The option string is the optional name
      for the location variable. The string is the quotation contents. *)
  type expand_fun 'a = FanLoc.t -> option string -> string -> 'a;

  (** [add name exp] adds the quotation [name] associated with the
      expander [exp]. *)
  val add : string -> DynAst.tag 'a -> expand_fun 'a -> unit;

  (** [find name] returns the expander of the given quotation name. *)
  val find : string -> DynAst.tag 'a -> expand_fun 'a;

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
    (FanLoc.t -> string -> 'a) -> FanLoc.t -> FanSig.quotation -> string -> string -> 'a;

  (** function translating quotation names; default = identity *)
  val translate : ref (string -> string);

  val expand : FanLoc.t -> FanSig.quotation -> DynAst.tag 'a -> 'a;

  (** [dump_file] optionally tells Camlp4 to dump the
      result of an expander if this result is syntactically incorrect.
      If [None] (default), this result is not dumped. If [Some fname], the
      result is dumped in the file [fname]. *)
  val dump_file : ref (option string);

  (* module Error : FanSig.Error; *)
  val add_quotation: string -> Gram.t 'a ->
    (FanLoc.t -> 'a -> Lib.Expr.Ast.expr) ->
      (FanLoc.t -> 'a -> Lib.Expr.Ast.patt) -> unit;

  (* BUG, revised parser can not parse name:string -> unit*)      
  val add_quotation_of_expr: ~name:string -> ~entry: Gram.t Ast.expr -> unit;
  val add_quotation_of_patt: ~name:string -> ~entry: Gram.t Ast.patt -> unit;
  val add_quotation_of_class_str_item: ~name:string -> ~entry: Gram.t Ast.class_str_item -> unit;
  val add_quotation_of_match_case: ~name:string -> ~entry: Gram.t Ast.match_case -> unit;
end;



open Format;
module Make (TheAntiquotSyntax: AntiquotSyntax) : S = struct   

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
    
  let expander_name pos_tag name =
    let str = DynAst.string_of_tag pos_tag in 
    match !translate name with
    [ "" ->
      try Hashtbl.find default_tbl str
      with [Not_found -> !default]
    | name -> name ];

  let find name tag =
    let key = (expander_name tag name, Exp_key.pack tag ()) in
    Exp_fun.unpack tag (List.assoc key !expanders_table);

  let add name tag f =
    let elt = ((name, Exp_key.pack tag ()), Exp_fun.pack tag f) in
    expanders_table := [elt :: !expanders_table];

  let dump_file = ref None;


  type quotation_error_message =
      [ Finding
      | Expanding
      | ParsingResult of FanLoc.t and string];

  type quotation_error = (string * string * quotation_error_message * exn);
  exception Quotation of quotation_error;

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
        | ParsingResult loc str -> begin
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
                    "\n(consider setting variable Quotation.dump_file, or using the -QD option)"
            ]
        end
        ] in
      let () = bprintf ppf "@\n%s@]@." (Printexc.to_string exn)in Buffer.contents ppf;

  Printexc.register_printer (fun
    [ Quotation x -> Some (quotation_error_to_string x )
    | _ -> None]);

  let expand_quotation loc expander pos_tag quot =
    debug quot "expand_quotation: name: %s, str: %S@." quot.q_name quot.q_contents in
    let open FanSig in
    let loc_name_opt = if quot.q_loc = "" then None else Some quot.q_loc in
    try expander loc loc_name_opt quot.q_contents with
    [ FanLoc.Exc_located _ (Quotation _) as exc ->
        raise exc
    | FanLoc.Exc_located iloc exc ->
        let exc1 = Quotation (quot.q_name, pos_tag, Expanding, exc) in
        raise (FanLoc.Exc_located iloc exc1)
    | exc ->
        let exc1 = Quotation (quot.q_name, pos_tag, Expanding, exc) in
        raise (FanLoc.Exc_located loc exc1) ];

  let parse_quotation_result parse loc quot pos_tag str =
    let open FanSig in 
    try parse loc str with
    [ FanLoc.Exc_located iloc (Quotation (n, pos_tag, Expanding, exc)) ->
        let ctx = ParsingResult iloc quot.q_contents in
        let exc1 = Quotation (n, pos_tag, ctx, exc) in
        raise (FanLoc.Exc_located iloc exc1)
    | FanLoc.Exc_located iloc (Quotation _ as exc) ->
        raise (FanLoc.Exc_located iloc exc)
    | FanLoc.Exc_located iloc exc ->
        let ctx = ParsingResult iloc quot.q_contents in
        let exc1 = Quotation (quot.q_name, pos_tag, ctx, exc) in
        raise (FanLoc.Exc_located iloc exc1) ];

  let expand loc quotation tag =
    let open FanSig in 
    let pos_tag = DynAst.string_of_tag tag in
    let name = quotation.q_name in
    debug quot "handle_quotation: name: %s, str: %S@." name quotation.q_contents in
    let expander =
      try find name tag
      with
      [ FanLoc.Exc_located _ (Quotation _) as exc -> raise exc
      | FanLoc.Exc_located qloc exc ->
          raise (FanLoc.Exc_located qloc (Quotation (name, pos_tag, Finding, exc)))
      | exc ->
          raise (FanLoc.Exc_located loc (Quotation (name, pos_tag, Finding, exc))) ] in
    let loc = FanLoc.join (FanLoc.move `start quotation.q_shift loc) in
    expand_quotation loc expander pos_tag quotation;

  let parse_quot_string entry loc loc_name_opt s  =
    BatRef.protect FanConfig.antiquotations true begin fun _ ->
      let res = Gram.parse_string entry loc s in 
      let ()  = Lib.Meta.MetaLocQuotation.loc_name := loc_name_opt in
      (* It's fine, since every quotation will always have a name *)
      res
    end;

  let anti_filter = Expr.antiquot_expander
      ~parse_expr:TheAntiquotSyntax.parse_expr
      ~parse_patt:TheAntiquotSyntax.parse_patt;

  let add_quotation name entry mexpr mpatt =
    let entry_eoi = Gram.eoi_entry entry in 
    let expand_expr loc loc_name_opt s =
      parse_quot_string entry_eoi loc loc_name_opt s |> mexpr loc |> anti_filter#expr  in
    let expand_str_item loc loc_name_opt s =
      let exp_ast = expand_expr loc loc_name_opt s in
      <:str_item@loc< $(exp:exp_ast) >> in
    let expand_patt _loc loc_name_opt s =
      BatRef.protect FanConfig.antiquotations true begin fun _ ->
        let ast = Gram.parse_string entry_eoi _loc s in
        let meta_ast = mpatt _loc ast in
        let exp_ast = anti_filter#patt meta_ast in
        match loc_name_opt with
        [ None -> exp_ast
        | Some name ->
        let rec subst_first_loc =  fun
          [ <:patt@_loc< Ast.$uid:u $_ >> -> {:patt| Ast.$uid:u $lid:name |}
          | <:patt@_loc< $a $b >> -> {:patt| $(subst_first_loc a) $b |}
          | p -> p ] in
        subst_first_loc exp_ast ]
      end in begin
          add name DynAst.expr_tag expand_expr;
          add name DynAst.patt_tag expand_patt;
          add name DynAst.str_item_tag expand_str_item;
      end;
    
    let add_quotation_of_str_item ~name ~entry =
      add name DynAst.str_item_tag
        (parse_quot_string (Gram.eoi_entry entry));
    let add_quotation_of_str_item_with_filter ~name ~entry ~filter =
      add name DynAst.str_item_tag
        (filter (parse_quot_string (Gram.eoi_entry entry) ));

    (* both [expr] and [str_item] positions are registered *)  
    let add_quotation_of_expr ~name ~entry = 
      let expand_fun = parse_quot_string (Gram.eoi_entry entry) in
      let mk_fun loc loc_name_opt s =
        <:str_item@loc< $(exp:expand_fun loc loc_name_opt s) >> in begin 
          add name DynAst.expr_tag expand_fun ;
          add name DynAst.str_item_tag mk_fun ;
        end ;
    let add_quotation_of_patt ~name ~entry =
      add name DynAst.patt_tag (parse_quot_string (Gram.eoi_entry entry));
      
    let add_quotation_of_class_str_item ~name ~entry =
      add name DynAst.class_str_item_tag (parse_quot_string (Gram.eoi_entry entry));
      
    let add_quotation_of_match_case ~name ~entry =
      add name DynAst.match_case_tag
        (parse_quot_string (Gram.eoi_entry entry));
    
  
end;
