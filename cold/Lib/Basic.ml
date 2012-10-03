open Format

open LibUtil

exception Unhandled of Ast.ctyp

exception
                                                             Finished of
                                                             Ast.expr


let _loc = FanLoc.ghost

let unit_literal =
                          (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "()")) )))


let x =
 fun ?(off = 0) ->
  fun (i :
    int) ->
   if (off > 25) then ( (invalid_arg "unsupported offset in x ") )
   else
    let base = let open Char in (( (( (code 'a') ) + off) ) |> chr) in
    (( (String.of_char base) ) ^ ( (string_of_int i) ))

let xid =
                                                          fun ?(off = 0) ->
                                                           fun (i :
                                                             int) ->
                                                            ((Ast.IdLid
                                                               (_loc, (
                                                                (x ~off:off
                                                                  i) ))) :
                                                              Ast.ident)


let allx = fun ?(off = 0) -> fun i -> ("all_" ^ ( (x ~off:off i) ))


let allxid =
 fun ?(off = 0) -> fun i -> (Ast.IdLid (_loc, ( (allx ~off:off i) )))


let check_valid =
 fun str ->
  let len = (String.length str) in
  if (not (
       (( (len > 1) ) && (
         (( (not ( (Char.is_digit ( (String.get str 1) )) )) ) && (
           (not ( (String.starts_with str "all_") )) )) )) ))
  then
   begin
   (
   (eprintf "%s is not a valid name" str)
   );
   (
   (eprintf
     "For valid name its length should be more than 1\ncan not be a-[digit], can not start with [all_]")
   );
   (exit 2)
  end else ()

module Make =
                functor (MGram : (Camlp4.Sig.Grammar.Static with module Loc =
                                  Loc and module Loc = Loc and module Token =
                                  Token)) ->
                 (struct
                   type 'a t = 'a MGram.Entry.t

                   type loc = MGram.Loc.t

                   let eoi_entry =
                    fun entry ->
                     let entry_eoi =
                      let open MGram.Entry in (mk ( (name entry) )) in
                     (
                     (MGram.extend ( (entry_eoi : 'entry_eoi MGram.Entry.t) )
                       (
                       ((fun ()
                           ->
                          (None , (
                           [(None , None , (
                             [((
                               [(
                                (MGram.Snterm
                                  (MGram.Entry.obj (
                                    (entry : 'entry MGram.Entry.t) ))) ); (
                                (MGram.Stoken
                                  (( function | EOI -> (true) | _ -> (false)
                                   ), "EOI")) )] ), (
                               (MGram.Action.mk (
                                 fun (__camlp4_0 :
                                   MGram.Token.t) ->
                                  fun (x :
                                    'entry) ->
                                   fun (_loc :
                                     FanLoc.t) ->
                                    (match __camlp4_0 with
                                     | EOI -> (x : 'entry_eoi)
                                     | _ -> assert false) )) ))] ))] ))) () )
                       ))
                     );
                     entry_eoi

                   let parse_quot_string_with_filter =
                    fun entry ->
                     fun f ->
                      fun loc ->
                       fun loc_name_opt ->
                        fun s ->
                         let q = Camlp4_config.antiquotations.contents in
                         (
                         (Camlp4_config.antiquotations := true )
                         );
                         let res = (MGram.parse_string entry loc s) in
                         (
                         (Camlp4_config.antiquotations := q)
                         );
                         (
                         (MetaLoc.loc_name := loc_name_opt)
                         );
                         (f res)

                   let parse_quot_string =
                    fun entry ->
                     fun loc ->
                      fun loc_name_opt ->
                       fun s ->
                        (parse_quot_string_with_filter entry ( fun x -> x )
                          loc loc_name_opt s)

                   let add_quotation =
                    fun ?antiquot_expander ->
                     fun name ->
                      fun ~entry ->
                       fun ~mexpr ->
                        fun ~mpatt ->
                         let anti_expr =
                          (match antiquot_expander with
                           | None -> fun x -> x
                           | Some (obj) -> obj#expr) in
                         let anti_patt =
                          (match antiquot_expander with
                           | None -> fun x -> x
                           | Some (obj) -> obj#patt) in
                         let entry_eoi = (eoi_entry entry) in
                         let expand_expr =
                          fun loc ->
                           fun loc_name_opt ->
                            fun s ->
                             ((
                               ((
                                 (parse_quot_string entry_eoi loc
                                   loc_name_opt s) ) |> ( (mexpr loc) )) ) |>
                               anti_expr) in
                         let expand_str_item =
                          fun loc ->
                           fun loc_name_opt ->
                            fun s ->
                             let exp_ast = (expand_expr loc loc_name_opt s) in
                             (Ast.StExp (loc, exp_ast)) in
                         let expand_patt =
                          fun _loc ->
                           fun loc_name_opt ->
                            fun s ->
                             let exp_ast =
                              ((
                                ((
                                  (parse_quot_string entry_eoi _loc
                                    loc_name_opt s) ) |> ( (mpatt _loc) )) )
                                |> anti_patt) in
                             (match loc_name_opt with
                              | None -> exp_ast
                              | Some (name) ->
                                 let rec subst_first_loc =
                                  function
                                  | Ast.PaApp
                                     (_loc,
                                      Ast.PaId
                                       (_,
                                        Ast.IdAcc
                                         (_, Ast.IdUid (_, "Ast"),
                                          Ast.IdUid (_, u))), Ast.PaAny (_)) ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdUid (_loc, u)) ))) )))
                                        ), (
                                        (Ast.PaId
                                          (_loc, ( (Ast.IdLid (_loc, name))
                                           ))) )))
                                  | Ast.PaApp (_loc, a, b) ->
                                     (Ast.PaApp
                                       (_loc, ( (subst_first_loc a) ), b))
                                  | p -> p in
                                 (subst_first_loc exp_ast)) in
                         let open
                         Quotation in
                         (
                         (add name DynAst.expr_tag expand_expr)
                         );
                         (
                         (add name DynAst.patt_tag expand_patt)
                         );
                         (add name DynAst.str_item_tag expand_str_item)

                   let add = Quotation.add

                   let add_quotation_of_str_item =
                    fun ~name ->
                     fun ~entry ->
                      (add name Quotation.DynAst.str_item_tag (
                        (parse_quot_string ( (eoi_entry entry) )) ))

                   let add_quotation_of_str_item_with_filter =
                    fun ~name ->
                     fun ~entry ->
                      fun ~filter ->
                       (add name Quotation.DynAst.str_item_tag (
                         (parse_quot_string_with_filter ( (eoi_entry entry) )
                           filter) ))

                   let add_quotation_of_expr =
                    fun ~name ->
                     fun ~entry ->
                      let expand_fun =
                       (parse_quot_string & ( (eoi_entry entry) )) in
                      let mk_fun =
                       fun loc ->
                        fun loc_name_opt ->
                         fun s ->
                          (Ast.StExp
                            (_loc, ( (expand_fun loc loc_name_opt s) ))) in
                      let () =
                       (add name Quotation.DynAst.expr_tag expand_fun) in
                      let () =
                       (add name Quotation.DynAst.str_item_tag mk_fun) in
                      ()

                   let add_quotation_of_patt =
                    fun ~name ->
                     fun ~entry ->
                      (add name Quotation.DynAst.patt_tag (
                        (parse_quot_string ( (eoi_entry entry) )) ))

                   let add_quotation_of_class_str_item =
                    fun ~name ->
                     fun ~entry ->
                      (add name Quotation.DynAst.class_str_item_tag (
                        (parse_quot_string ( (eoi_entry entry) )) ))

                   let add_quotation_of_match_case =
                    fun ~name ->
                     fun ~entry ->
                      (add name Quotation.DynAst.match_case_tag (
                        (parse_quot_string ( (eoi_entry entry) )) ))

                  end :
                   (Fan_sig.Grammar with type 'a t = 'a MGram.Entry.t
                    and type 'a t = 'a MGram.Entry.t and type  loc =
                    MGram.Loc.t))

module Fan_camlp4syntax = (Make)(Gram)


let (anti_str_item, anti_expr) =
 let open
 Fan_camlp4syntax in
 (( (eoi_entry Syntax.str_item) ), ( (eoi_entry Syntax.expr) ))

let is_antiquot_data_ctor =
                                                                  fun s ->
                                                                   (String.ends_with
                                                                    s "Ant")


let mk_anti =
 fun ?(c = "") ->
  fun n -> fun s -> ("\\$" ^ ( (n ^ ( (c ^ ( (":" ^ s) )) )) ))

let is_antiquot =
                                                                  fun s ->
                                                                   let len =
                                                                    (String.length
                                                                    s) in
                                                                   ((
                                                                    (len > 2)
                                                                    ) && (
                                                                    ((
                                                                    ((
                                                                    (String.get
                                                                    s 0) ) =
                                                                    '\\') )
                                                                    && (
                                                                    ((
                                                                    (String.get
                                                                    s 1) ) =
                                                                    '$') ))
                                                                    ))


let handle_antiquot_in_string =
 fun s ->
  fun ~term ->
   fun ~parse ->
    fun ~loc ->
     fun ~decorate ->
      if (is_antiquot s) then
       (
       let pos = (String.index s ':') in
       let name = (String.sub s 2 ( (pos - 2) ))
       and code =
        (String.sub s ( (pos + 1) ) ( (( (( (String.length s) ) - pos) ) - 1)
          )) in
       (decorate name ( (parse loc code) ))
       )
      else (term s)
