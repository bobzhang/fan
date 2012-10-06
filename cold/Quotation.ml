module type S =
 sig
  type 'a expand_fun = (FanLoc.t -> (string option -> (string -> 'a)))

  val add : (string -> ('a DynAst.tag -> ('a expand_fun -> unit)))

  val find : (string -> ('a DynAst.tag -> 'a expand_fun))

  val default : string ref

  val default_tbl : (string, string) Hashtbl.t

  val default_at_pos : (string -> (string -> unit))

  val parse_quotation_result :
   ((FanLoc.t -> (string -> 'a)) ->
    (FanLoc.t -> (FanSig.quotation -> (string -> (string -> 'a)))))

  val translate : (string -> string) ref

  val expand : (FanLoc.t -> (FanSig.quotation -> ('a DynAst.tag -> 'a)))

  val dump_file : string option ref

 end

module Make =
       functor (U : sig end) ->
        (struct
          open Format

          type 'a expand_fun =
           (FanLoc.t -> (string option -> (string -> 'a)))

          module Exp_key = (DynAst.Pack)(struct type 'a t = unit
 end)

          module Exp_fun =
           (DynAst.Pack)(struct type 'a t = 'a expand_fun
 end)

          let expanders_table =
           ((ref [] ) : ((string * Exp_key.pack) * Exp_fun.pack) list ref)

          let default = (ref "")

          let default_tbl =
           ((Hashtbl.create 50) : (string, string) Hashtbl.t)

          let translate = (ref ( fun x -> x ))

          let default_at_pos =
           fun pos -> fun str -> (Hashtbl.replace default_tbl pos str)

          let expander_name =
           fun pos_tag ->
            fun name ->
             let str = (DynAst.string_of_tag pos_tag) in
             (match ((translate.contents) name) with
              | "" ->
                 (try (Hashtbl.find default_tbl str) with
                  Not_found -> default.contents)
              | name -> name)

          let find =
           fun name ->
            fun tag ->
             let key =
              (( (expander_name tag name) ), ( (Exp_key.pack tag () ) )) in
             (Exp_fun.unpack tag (
               (List.assoc key ( expanders_table.contents )) ))

          let add =
           fun name ->
            fun tag ->
             fun f ->
              let elt =
               ((name, ( (Exp_key.pack tag () ) )), ( (Exp_fun.pack tag f) )) in
              (expanders_table := ( ( elt ) :: expanders_table.contents  ))

          let dump_file = (ref None )

          type quotation_error_message =
             Finding | Expanding | ParsingResult of FanLoc.t * string

          type quotation_error =
           (string * string * quotation_error_message * exn)

          exception Quotation of quotation_error

          let quotation_error_to_string =
           fun (name, position, ctx, exn) ->
            let ppf = (Buffer.create 30) in
            let name = if (name = "") then ( default.contents ) else name in
            let pp =
             fun x ->
              (bprintf ppf "@?@[<2>While %s %S in a position of %S:" x name
                position) in
            let () =
             (match ctx with
              | Finding ->
                 (
                 (pp "finding quotation")
                 );
                 (
                 (bprintf ppf
                   "@ @[<hv2>Available quotation expanders are:@\n")
                 );
                 (
                 (List.iter (
                   fun ((s, t), _) ->
                    (bprintf ppf "@[<2>%s@ (in@ a@ position@ of %a)@]@ " s
                      Exp_key.print_tag t) ) ( expanders_table.contents ))
                 );
                 (bprintf ppf "@]")
              | Expanding -> (pp "expanding quotation")
              | ParsingResult (loc, str) ->
                 (
                 (pp "parsing result of quotation")
                 );
                 (match dump_file.contents with
                  | Some (dump_file) ->
                     let () = (bprintf ppf " dumping result...\n") in
                     (try
                       let oc = (open_out_bin dump_file) in
                       (
                       (output_string oc str)
                       );
                       (
                       (output_string oc "\n")
                       );
                       (
                       (flush oc)
                       );
                       (
                       (close_out oc)
                       );
                       (bprintf ppf "%a:" FanLoc.print (
                         (FanLoc.set_file_name dump_file loc) ))
                      with
                      _ ->
                       (bprintf ppf
                         "Error while dumping result in file %S; dump aborted"
                         dump_file))
                  | None ->
                     (bprintf ppf
                       "\n(consider setting variable Quotation.dump_file, or using the -QD option)"))) in
            let () = (bprintf ppf "@\n%s@]@." ( (Printexc.to_string exn) )) in
            (Buffer.contents ppf)

          let _ = (Printexc.register_printer (
                    function
                    | Quotation (x) -> (Some (quotation_error_to_string x))
                    | _ -> (None) ))

          let expand_quotation =
           fun loc ->
            fun expander ->
             fun pos_tag ->
              fun quot ->
               let open
               FanSig in
               let loc_name_opt =
                if (( quot.q_loc ) = "") then None  else (Some (quot.q_loc)) in
               (try (expander loc loc_name_opt ( quot.q_contents )) with
                | (FanLoc.Exc_located (_, Quotation (_)) as exc) ->
                   (raise exc)
                | FanLoc.Exc_located (iloc, exc) ->
                   let exc1 =
                    (Quotation (( quot.q_name ), pos_tag, Expanding , exc)) in
                   (raise ( (FanLoc.Exc_located (iloc, exc1)) ))
                | exc ->
                   let exc1 =
                    (Quotation (( quot.q_name ), pos_tag, Expanding , exc)) in
                   (raise ( (FanLoc.Exc_located (loc, exc1)) )))

          let parse_quotation_result =
           fun parse ->
            fun loc ->
             fun quot ->
              fun pos_tag ->
               fun str ->
                let open
                FanSig in
                (try (parse loc str) with
                 | FanLoc.Exc_located
                    (iloc, Quotation (n, pos_tag, Expanding, exc)) ->
                    let ctx = (ParsingResult (iloc, ( quot.q_contents ))) in
                    let exc1 = (Quotation (n, pos_tag, ctx, exc)) in
                    (raise ( (FanLoc.Exc_located (iloc, exc1)) ))
                 | FanLoc.Exc_located (iloc, (Quotation (_) as exc)) ->
                    (raise ( (FanLoc.Exc_located (iloc, exc)) ))
                 | FanLoc.Exc_located (iloc, exc) ->
                    let ctx = (ParsingResult (iloc, ( quot.q_contents ))) in
                    let exc1 =
                     (Quotation (( quot.q_name ), pos_tag, ctx, exc)) in
                    (raise ( (FanLoc.Exc_located (iloc, exc1)) )))

          let expand =
           fun loc ->
            fun quotation ->
             fun tag ->
              let open
              FanSig in
              let pos_tag = (DynAst.string_of_tag tag) in
              let name = quotation.q_name in
              let expander =
               (try (find name tag) with
                | (FanLoc.Exc_located (_, Quotation (_)) as exc) ->
                   (raise exc)
                | FanLoc.Exc_located (qloc, exc) ->
                   (raise (
                     (FanLoc.Exc_located
                       (qloc, ( (Quotation (name, pos_tag, Finding , exc)) )))
                     ))
                | exc ->
                   (raise (
                     (FanLoc.Exc_located
                       (loc, ( (Quotation (name, pos_tag, Finding , exc)) )))
                     ))) in
              let loc =
               (FanLoc.join ( (FanLoc.move `start ( quotation.q_shift ) loc)
                 )) in
              (expand_quotation loc expander pos_tag quotation)

         end : S)
