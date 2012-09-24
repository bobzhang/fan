module Make =
 functor (Ast : Sig.Camlp4Ast) ->
  (struct
    module Ast = Ast

    module DynAst = (DynAst.Make)(Ast)

    open Format

    open Sig

    type 'a expand_fun = (FanLoc.t -> (string option -> (string -> 'a)))

    module Exp_key = (DynAst.Pack)(struct type 'a t = unit
 end)

    module Exp_fun = (DynAst.Pack)(struct type 'a t = 'a expand_fun
 end)

    let expanders_table =
     ((ref [] ) : ((string * Exp_key.pack) * Exp_fun.pack) list ref)

    let default = (ref "")

    let default_tbl = ((Hashtbl.create 50) : (string, string) Hashtbl.t)

    let translate = (ref ( fun x -> x ))

    let default_at_pos =
     fun pos -> fun str -> (Hashtbl.replace default_tbl pos str)

    let expander_name =
     fun pos_tag ->
      fun name ->
       let str = (DynAst.string_of_tag pos_tag) in
       (match ((!translate) name) with
        | "" ->
           (try (Hashtbl.find default_tbl str) with
            Not_found -> !default)
        | name -> name)

    let find =
     fun name ->
      fun tag ->
       let key = (( (expander_name tag name) ), ( (Exp_key.pack tag () ) )) in
       (Exp_fun.unpack tag ( (List.assoc key ( !expanders_table )) ))

    let add =
     fun name ->
      fun tag ->
       fun f ->
        let elt =
         ((name, ( (Exp_key.pack tag () ) )), ( (Exp_fun.pack tag f) )) in
        (expanders_table := ( ( elt ) :: !expanders_table  ))

    let dump_file = (ref None )

    module Error =
     struct
      type error =
         Finding | Expanding | ParsingResult of FanLoc.t * string | Locating

      type t = (string * string * error * exn)

      exception E of t

      let print =
       fun ppf ->
        fun (name, position, ctx, exn) ->
         let name = if (name = "") then ( !default ) else name in
         let pp =
          fun x ->
           (fprintf ppf "@?@[<2>While %s %S in a position of %S:" x name
             position) in
         let () =
          (match ctx with
           | Finding ->
              (
              (pp "finding quotation")
              );
              if (( !expanders_table ) = [] ) then
               (
               (fprintf ppf "@ There is no quotation expander available.")
               )
              else begin
               (
               (fprintf ppf "@ @[<hv2>Available quotation expanders are:@\n")
               );
               (
               (List.iter (
                 fun ((s, t), _) ->
                  (fprintf ppf "@[<2>%s@ (in@ a@ position@ of %a)@]@ " s
                    Exp_key.print_tag t) ) ( !expanders_table ))
               );
               (fprintf ppf "@]")
              end
           | Expanding -> (pp "expanding quotation")
           | Locating -> (pp "parsing")
           | ParsingResult (loc, str) ->
              let () = (pp "parsing result of quotation") in
              (match !dump_file with
               | Some (dump_file) ->
                  let () = (fprintf ppf " dumping result...\n") in
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
                    (fprintf ppf "%a:" FanLoc.print (
                      (FanLoc.set_file_name dump_file loc) ))
                   with
                   _ ->
                    (fprintf ppf
                      "Error while dumping result in file %S; dump aborted"
                      dump_file))
               | None ->
                  (fprintf ppf
                    "\n(consider setting variable Quotation.dump_file, or using the -QD option)"))) in
         (fprintf ppf "@\n%a@]@." FanUtil.ErrorHandler.print exn)

      let to_string =
       fun x ->
        let b = (Buffer.create 50) in
        let () = (bprintf b "%a" print x) in (Buffer.contents b)

     end

    let _ = let module M = (FanUtil.ErrorHandler.Register)(Error) in ()

    open Error

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
          | (FanLoc.Exc_located (_, Error.E (_)) as exc) -> (raise exc)
          | FanLoc.Exc_located (iloc, exc) ->
             let exc1 = (Error.E (( quot.q_name ), pos_tag, Expanding , exc)) in
             (raise ( (FanLoc.Exc_located (iloc, exc1)) ))
          | exc ->
             let exc1 = (Error.E (( quot.q_name ), pos_tag, Expanding , exc)) in
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
           | FanLoc.Exc_located (iloc, Error.E (n, pos_tag, Expanding, exc)) ->
              let ctx = (ParsingResult (iloc, ( quot.q_contents ))) in
              let exc1 = (Error.E (n, pos_tag, ctx, exc)) in
              (raise ( (FanLoc.Exc_located (iloc, exc1)) ))
           | FanLoc.Exc_located (iloc, (Error.E (_) as exc)) ->
              (raise ( (FanLoc.Exc_located (iloc, exc)) ))
           | FanLoc.Exc_located (iloc, exc) ->
              let ctx = (ParsingResult (iloc, ( quot.q_contents ))) in
              let exc1 = (Error.E (( quot.q_name ), pos_tag, ctx, exc)) in
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
          | (FanLoc.Exc_located (_, Error.E (_)) as exc) -> (raise exc)
          | FanLoc.Exc_located (qloc, exc) ->
             (raise (
               (FanLoc.Exc_located
                 (qloc, ( (Error.E (name, pos_tag, Finding , exc)) ))) ))
          | exc ->
             (raise (
               (FanLoc.Exc_located
                 (loc, ( (Error.E (name, pos_tag, Finding , exc)) ))) ))) in
        let loc =
         (FanLoc.join ( (FanLoc.move `start ( quotation.q_shift ) loc) )) in
        (expand_quotation loc expander pos_tag quotation)

   end : (Sig.Quotation with module Ast = Ast))
