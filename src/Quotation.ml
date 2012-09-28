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

  module Error : FanSig.Error;

end;

module Make (U:sig end) : S = struct   
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

  module Error = struct
    type error =
      [ Finding
      | Expanding
      | ParsingResult of FanLoc.t and string
      | Locating ]; (* FIXME locating never used to build values *)
    type t = (string * string * error * exn);
    exception E of t;

    let print ppf (name, position, ctx, exn) =
      let name = if name = "" then !default else name in
      let pp x = fprintf ppf "@?@[<2>While %s %S in a position of %S:" x name position in
      let () =
        match ctx with
        [ Finding -> begin
            pp "finding quotation";
            if expanders_table.contents = [] then
              fprintf ppf "@ There is no quotation expander available."
            else
              begin
                fprintf ppf "@ @[<hv2>Available quotation expanders are:@\n";
                List.iter begin fun ((s,t),_) ->
                  fprintf ppf "@[<2>%s@ (in@ a@ position@ of %a)@]@ "
                    s Exp_key.print_tag t
                end !expanders_table;
                fprintf ppf "@]"
              end
          end
        | Expanding -> pp "expanding quotation"
        | Locating -> pp "parsing"
        | ParsingResult loc str ->
          let () = pp "parsing result of quotation" in
          match !dump_file with
          [ Some dump_file ->
              let () = fprintf ppf " dumping result...\n" in
              try
                let oc = open_out_bin dump_file in
                begin
                  output_string oc str;
                  output_string oc "\n";
                  flush oc;
                  close_out oc;
                  fprintf ppf "%a:" FanLoc.print (FanLoc.set_file_name dump_file loc);
                end
              with _ ->
                fprintf ppf
                  "Error while dumping result in file %S; dump aborted"
                  dump_file
          | None ->
              fprintf ppf
                "\n(consider setting variable Quotation.dump_file, or using the -QD option)"
          ]
        ]
      in fprintf ppf "@\n%a@]@." FanUtil.ErrorHandler.print exn;

    let to_string x =
      let b = Buffer.create 50 in
      let () = bprintf b "%a" print x in Buffer.contents b;
  end;
  let module M = FanUtil.ErrorHandler.Register Error in ();
  open Error;

  let expand_quotation loc expander pos_tag quot =
    debug quot "expand_quotation: name: %s, str: %S@." quot.q_name quot.q_contents in
    let open FanSig in
    let loc_name_opt = if quot.q_loc = "" then None else Some quot.q_loc in
    try expander loc loc_name_opt quot.q_contents with
    [ FanLoc.Exc_located _ (Error.E _) as exc ->
        raise exc
    | FanLoc.Exc_located iloc exc ->
        let exc1 = Error.E (quot.q_name, pos_tag, Expanding, exc) in
        raise (FanLoc.Exc_located iloc exc1)
    | exc ->
        let exc1 = Error.E (quot.q_name, pos_tag, Expanding, exc) in
        raise (FanLoc.Exc_located loc exc1) ];

  let parse_quotation_result parse loc quot pos_tag str =
    let open FanSig in 
    try parse loc str with
    [ FanLoc.Exc_located iloc (Error.E (n, pos_tag, Expanding, exc)) ->
        let ctx = ParsingResult iloc quot.q_contents in
        let exc1 = Error.E (n, pos_tag, ctx, exc) in
        raise (FanLoc.Exc_located iloc exc1)
    | FanLoc.Exc_located iloc (Error.E _ as exc) ->
        raise (FanLoc.Exc_located iloc exc)
    | FanLoc.Exc_located iloc exc ->
        let ctx = ParsingResult iloc quot.q_contents in
        let exc1 = Error.E (quot.q_name, pos_tag, ctx, exc) in
        raise (FanLoc.Exc_located iloc exc1) ];

  let expand loc quotation tag =
    let open FanSig in 
    let pos_tag = DynAst.string_of_tag tag in
    let name = quotation.q_name in
    debug quot "handle_quotation: name: %s, str: %S@." name quotation.q_contents in
    let expander =
      try find name tag
      with
      [ FanLoc.Exc_located _ (Error.E _) as exc -> raise exc
      | FanLoc.Exc_located qloc exc ->
          raise (FanLoc.Exc_located qloc (Error.E (name, pos_tag, Finding, exc)))
      | exc ->
          raise (FanLoc.Exc_located loc (Error.E (name, pos_tag, Finding, exc))) ]
    in
    let loc = FanLoc.join (FanLoc.move `start quotation.q_shift loc) in
    expand_quotation loc expander pos_tag quotation;

end;
