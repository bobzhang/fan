module Make =
 functor (Ast : (Sig.Camlp4Ast with module Loc = FanLoc)) ->
  functor (Gram : (FanSig.Grammar.Static with module Loc = Ast.Loc
                   and module Loc = Ast.Loc and type  Token.t =
                   FanSig.camlp4_token)) ->
   functor (Quotation : (Sig.Quotation with module Ast =
                         Sig.Camlp4AstToAst(Ast))) ->
    (struct
      module Ast = Ast

      module Gram = Gram

      module Token = Gram.Token

      module Ast2pt = (Struct.Camlp4Ast2OCamlAst.Make)(Ast)

      open Sig

      type warning = (FanLoc.t -> (string -> unit))

      let default_warning =
       fun loc ->
        fun txt -> (Format.eprintf "<W> %a: %s@." FanLoc.print loc txt)

      let current_warning = (ref default_warning)

      let print_warning = fun loc -> fun txt -> ((!current_warning) loc txt)

      let a_CHAR = (Gram.Entry.mk "a_CHAR")

      let a_FLOAT = (Gram.Entry.mk "a_FLOAT")

      let a_INT = (Gram.Entry.mk "a_INT")

      let a_INT32 = (Gram.Entry.mk "a_INT32")

      let a_INT64 = (Gram.Entry.mk "a_INT64")

      let a_LABEL = (Gram.Entry.mk "a_LABEL")

      let a_LIDENT = (Gram.Entry.mk "a_LIDENT")

      let a_NATIVEINT = (Gram.Entry.mk "a_NATIVEINT")

      let a_OPTLABEL = (Gram.Entry.mk "a_OPTLABEL")

      let a_STRING = (Gram.Entry.mk "a_STRING")

      let a_UIDENT = (Gram.Entry.mk "a_UIDENT")

      let a_ident = (Gram.Entry.mk "a_ident")

      let amp_ctyp = (Gram.Entry.mk "amp_ctyp")

      let and_ctyp = (Gram.Entry.mk "and_ctyp")

      let match_case = (Gram.Entry.mk "match_case")

      let match_case0 = (Gram.Entry.mk "match_case0")

      let binding = (Gram.Entry.mk "binding")

      let class_declaration = (Gram.Entry.mk "class_declaration")

      let class_description = (Gram.Entry.mk "class_description")

      let class_expr = (Gram.Entry.mk "class_expr")

      let class_fun_binding = (Gram.Entry.mk "class_fun_binding")

      let class_fun_def = (Gram.Entry.mk "class_fun_def")

      let class_info_for_class_expr =
       (Gram.Entry.mk "class_info_for_class_expr")

      let class_info_for_class_type =
       (Gram.Entry.mk "class_info_for_class_type")

      let class_longident = (Gram.Entry.mk "class_longident")

      let class_longident_and_param =
       (Gram.Entry.mk "class_longident_and_param")

      let class_name_and_param = (Gram.Entry.mk "class_name_and_param")

      let class_sig_item = (Gram.Entry.mk "class_sig_item")

      let class_signature = (Gram.Entry.mk "class_signature")

      let class_str_item = (Gram.Entry.mk "class_str_item")

      let class_structure = (Gram.Entry.mk "class_structure")

      let class_type = (Gram.Entry.mk "class_type")

      let class_type_declaration = (Gram.Entry.mk "class_type_declaration")

      let class_type_longident = (Gram.Entry.mk "class_type_longident")

      let class_type_longident_and_param =
       (Gram.Entry.mk "class_type_longident_and_param")

      let class_type_plus = (Gram.Entry.mk "class_type_plus")

      let comma_ctyp = (Gram.Entry.mk "comma_ctyp")

      let comma_expr = (Gram.Entry.mk "comma_expr")

      let comma_ipatt = (Gram.Entry.mk "comma_ipatt")

      let comma_patt = (Gram.Entry.mk "comma_patt")

      let comma_type_parameter = (Gram.Entry.mk "comma_type_parameter")

      let constrain = (Gram.Entry.mk "constrain")

      let constructor_arg_list = (Gram.Entry.mk "constructor_arg_list")

      let constructor_declaration = (Gram.Entry.mk "constructor_declaration")

      let constructor_declarations =
       (Gram.Entry.mk "constructor_declarations")

      let ctyp = (Gram.Entry.mk "ctyp")

      let cvalue_binding = (Gram.Entry.mk "cvalue_binding")

      let direction_flag = (Gram.Entry.mk "direction_flag")

      let direction_flag_quot = (Gram.Entry.mk "direction_flag_quot")

      let dummy = (Gram.Entry.mk "dummy")

      let entry_eoi = (Gram.Entry.mk "entry_eoi")

      let eq_expr = (Gram.Entry.mk "eq_expr")

      let expr = (Gram.Entry.mk "expr")

      let expr_eoi = (Gram.Entry.mk "expr_eoi")

      let field_expr = (Gram.Entry.mk "field_expr")

      let field_expr_list = (Gram.Entry.mk "field_expr_list")

      let fun_binding = (Gram.Entry.mk "fun_binding")

      let fun_def = (Gram.Entry.mk "fun_def")

      let ident = (Gram.Entry.mk "ident")

      let implem = (Gram.Entry.mk "implem")

      let interf = (Gram.Entry.mk "interf")

      let ipatt = (Gram.Entry.mk "ipatt")

      let ipatt_tcon = (Gram.Entry.mk "ipatt_tcon")

      let label = (Gram.Entry.mk "label")

      let label_declaration = (Gram.Entry.mk "label_declaration")

      let label_declaration_list = (Gram.Entry.mk "label_declaration_list")

      let label_expr = (Gram.Entry.mk "label_expr")

      let label_expr_list = (Gram.Entry.mk "label_expr_list")

      let label_ipatt = (Gram.Entry.mk "label_ipatt")

      let label_ipatt_list = (Gram.Entry.mk "label_ipatt_list")

      let label_longident = (Gram.Entry.mk "label_longident")

      let label_patt = (Gram.Entry.mk "label_patt")

      let label_patt_list = (Gram.Entry.mk "label_patt_list")

      let labeled_ipatt = (Gram.Entry.mk "labeled_ipatt")

      let let_binding = (Gram.Entry.mk "let_binding")

      let meth_list = (Gram.Entry.mk "meth_list")

      let meth_decl = (Gram.Entry.mk "meth_decl")

      let module_binding = (Gram.Entry.mk "module_binding")

      let module_binding0 = (Gram.Entry.mk "module_binding0")

      let module_declaration = (Gram.Entry.mk "module_declaration")

      let module_expr = (Gram.Entry.mk "module_expr")

      let module_longident = (Gram.Entry.mk "module_longident")

      let module_longident_with_app =
       (Gram.Entry.mk "module_longident_with_app")

      let module_rec_declaration = (Gram.Entry.mk "module_rec_declaration")

      let module_type = (Gram.Entry.mk "module_type")

      let package_type = (Gram.Entry.mk "package_type")

      let more_ctyp = (Gram.Entry.mk "more_ctyp")

      let name_tags = (Gram.Entry.mk "name_tags")

      let opt_as_lident = (Gram.Entry.mk "opt_as_lident")

      let opt_class_self_patt = (Gram.Entry.mk "opt_class_self_patt")

      let opt_class_self_type = (Gram.Entry.mk "opt_class_self_type")

      let opt_class_signature = (Gram.Entry.mk "opt_class_signature")

      let opt_class_structure = (Gram.Entry.mk "opt_class_structure")

      let opt_comma_ctyp = (Gram.Entry.mk "opt_comma_ctyp")

      let opt_dot_dot = (Gram.Entry.mk "opt_dot_dot")

      let row_var_flag_quot = (Gram.Entry.mk "row_var_flag_quot")

      let opt_eq_ctyp = (Gram.Entry.mk "opt_eq_ctyp")

      let opt_expr = (Gram.Entry.mk "opt_expr")

      let opt_meth_list = (Gram.Entry.mk "opt_meth_list")

      let opt_mutable = (Gram.Entry.mk "opt_mutable")

      let mutable_flag_quot = (Gram.Entry.mk "mutable_flag_quot")

      let opt_polyt = (Gram.Entry.mk "opt_polyt")

      let opt_private = (Gram.Entry.mk "opt_private")

      let private_flag_quot = (Gram.Entry.mk "private_flag_quot")

      let opt_rec = (Gram.Entry.mk "opt_rec")

      let rec_flag_quot = (Gram.Entry.mk "rec_flag_quot")

      let opt_sig_items = (Gram.Entry.mk "opt_sig_items")

      let opt_str_items = (Gram.Entry.mk "opt_str_items")

      let opt_virtual = (Gram.Entry.mk "opt_virtual")

      let virtual_flag_quot = (Gram.Entry.mk "virtual_flag_quot")

      let opt_override = (Gram.Entry.mk "opt_override")

      let override_flag_quot = (Gram.Entry.mk "override_flag_quot")

      let opt_when_expr = (Gram.Entry.mk "opt_when_expr")

      let patt = (Gram.Entry.mk "patt")

      let patt_as_patt_opt = (Gram.Entry.mk "patt_as_patt_opt")

      let patt_eoi = (Gram.Entry.mk "patt_eoi")

      let patt_tcon = (Gram.Entry.mk "patt_tcon")

      let phrase = (Gram.Entry.mk "phrase")

      let poly_type = (Gram.Entry.mk "poly_type")

      let row_field = (Gram.Entry.mk "row_field")

      let sem_expr = (Gram.Entry.mk "sem_expr")

      let sem_expr_for_list = (Gram.Entry.mk "sem_expr_for_list")

      let sem_patt = (Gram.Entry.mk "sem_patt")

      let sem_patt_for_list = (Gram.Entry.mk "sem_patt_for_list")

      let semi = (Gram.Entry.mk "semi")

      let sequence = (Gram.Entry.mk "sequence")

      let do_sequence = (Gram.Entry.mk "do_sequence")

      let sig_item = (Gram.Entry.mk "sig_item")

      let sig_items = (Gram.Entry.mk "sig_items")

      let star_ctyp = (Gram.Entry.mk "star_ctyp")

      let str_item = (Gram.Entry.mk "str_item")

      let str_items = (Gram.Entry.mk "str_items")

      let top_phrase = (Gram.Entry.mk "top_phrase")

      let type_constraint = (Gram.Entry.mk "type_constraint")

      let type_declaration = (Gram.Entry.mk "type_declaration")

      let type_ident_and_parameters =
       (Gram.Entry.mk "type_ident_and_parameters")

      let type_kind = (Gram.Entry.mk "type_kind")

      let type_longident = (Gram.Entry.mk "type_longident")

      let type_longident_and_parameters =
       (Gram.Entry.mk "type_longident_and_parameters")

      let type_parameter = (Gram.Entry.mk "type_parameter")

      let type_parameters = (Gram.Entry.mk "type_parameters")

      let typevars = (Gram.Entry.mk "typevars")

      let use_file = (Gram.Entry.mk "use_file")

      let val_longident = (Gram.Entry.mk "val_longident")

      let value_let = (Gram.Entry.mk "value_let")

      let value_val = (Gram.Entry.mk "value_val")

      let with_constr = (Gram.Entry.mk "with_constr")

      let expr_quot = (Gram.Entry.mk "quotation of expression")

      let patt_quot = (Gram.Entry.mk "quotation of pattern")

      let ctyp_quot = (Gram.Entry.mk "quotation of type")

      let str_item_quot = (Gram.Entry.mk "quotation of structure item")

      let sig_item_quot = (Gram.Entry.mk "quotation of signature item")

      let class_str_item_quot =
       (Gram.Entry.mk "quotation of class structure item")

      let class_sig_item_quot =
       (Gram.Entry.mk "quotation of class signature item")

      let module_expr_quot = (Gram.Entry.mk "quotation of module expression")

      let module_type_quot = (Gram.Entry.mk "quotation of module type")

      let class_type_quot = (Gram.Entry.mk "quotation of class type")

      let class_expr_quot = (Gram.Entry.mk "quotation of class expression")

      let with_constr_quot = (Gram.Entry.mk "quotation of with constraint")

      let binding_quot = (Gram.Entry.mk "quotation of binding")

      let rec_binding_quot = (Gram.Entry.mk "quotation of record binding")

      let match_case_quot =
       (Gram.Entry.mk "quotation of match_case (try/match/function case)")

      let module_binding_quot =
       (Gram.Entry.mk "quotation of module rec binding")

      let ident_quot = (Gram.Entry.mk "quotation of identifier")

      let prefixop =
       (Gram.Entry.mk "prefix operator (start with '!', '?', '~')")

      let infixop0 =
       (Gram.Entry.mk
         "infix operator (level 0) (comparison operators, and some others)")

      let infixop1 =
       (Gram.Entry.mk "infix operator (level 1) (start with '^', '@')")

      let infixop2 =
       (Gram.Entry.mk "infix operator (level 2) (start with '+', '-')")

      let infixop3 =
       (Gram.Entry.mk "infix operator (level 3) (start with '*', '/', '%')")

      let infixop4 =
       (Gram.Entry.mk
         "infix operator (level 4) (start with \"**\") (right assoc)")

      open FanSig

      let _ = (Gram.extend ( (top_phrase : 'top_phrase Gram.Entry.t) ) (
                ((fun ()
                    ->
                   (None , (
                    [(None , None , (
                      [((
                        [(
                         (Gram.Stoken
                           (( function | EOI -> (true) | _ -> (false) ),
                            "EOI")) )] ), (
                        (Gram.Action.mk (
                          fun (__camlp4_0 :
                            Gram.Token.t) ->
                           fun (_loc :
                             Gram.Loc.t) ->
                            (match __camlp4_0 with
                             | EOI -> ((None) : 'top_phrase)
                             | _ -> assert false) )) ))] ))] ))) () ) ))

      module AntiquotSyntax =
       struct
        module Ast = (Sig.Camlp4AstToAst)(Ast)

        module Gram = Gram

        let antiquot_expr = (Gram.Entry.mk "antiquot_expr")

        let antiquot_patt = (Gram.Entry.mk "antiquot_patt")

        let _ = (
        (Gram.extend ( (antiquot_expr : 'antiquot_expr Gram.Entry.t) ) (
          ((fun ()
              ->
             (None , (
              [(None , None , (
                [((
                  [(
                   (Gram.Snterm
                     (Gram.Entry.obj ( (expr : 'expr Gram.Entry.t) ))) ); (
                   (Gram.Stoken
                     (( function | EOI -> (true) | _ -> (false) ), "EOI")) )]
                  ), (
                  (Gram.Action.mk (
                    fun (__camlp4_0 :
                      Gram.Token.t) ->
                     fun (x :
                       'expr) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (match __camlp4_0 with
                        | EOI -> (x : 'antiquot_expr)
                        | _ -> assert false) )) ))] ))] ))) () ) ))
        );
        (Gram.extend ( (antiquot_patt : 'antiquot_patt Gram.Entry.t) ) (
          ((fun ()
              ->
             (None , (
              [(None , None , (
                [((
                  [(
                   (Gram.Snterm
                     (Gram.Entry.obj ( (patt : 'patt Gram.Entry.t) ))) ); (
                   (Gram.Stoken
                     (( function | EOI -> (true) | _ -> (false) ), "EOI")) )]
                  ), (
                  (Gram.Action.mk (
                    fun (__camlp4_0 :
                      Gram.Token.t) ->
                     fun (x :
                       'patt) ->
                      fun (_loc :
                        Gram.Loc.t) ->
                       (match __camlp4_0 with
                        | EOI -> (x : 'antiquot_patt)
                        | _ -> assert false) )) ))] ))] ))) () ) ))

        let parse_expr =
         fun loc -> fun str -> (Gram.parse_string antiquot_expr loc str)

        let parse_patt =
         fun loc -> fun str -> (Gram.parse_string antiquot_patt loc str)

       end

      module Quotation = Quotation

      let wrap =
       fun directive_handler ->
        fun pa ->
         fun init_loc ->
          fun cs ->
           let rec loop =
            fun loc ->
             let (pl, stopped_at_directive) = (pa loc cs) in
             (match stopped_at_directive with
              | Some (new_loc) ->
                 let pl =
                  (match (List.rev pl) with
                   | [] -> assert false
                   | (x :: xs) ->
                      (match (directive_handler x) with
                       | None -> xs
                       | Some (x) -> ( x ) :: xs )) in
                 (( (List.rev pl) ) @ ( (loop new_loc) ))
              | None -> pl) in
           (loop init_loc)

      let parse_implem =
       fun ?(directive_handler = fun _ -> (None)) ->
        fun _loc ->
         fun cs ->
          let l = (wrap directive_handler ( (Gram.parse implem) ) _loc cs) in
          (Ast.stSem_of_list l)

      let parse_interf =
       fun ?(directive_handler = fun _ -> (None)) ->
        fun _loc ->
         fun cs ->
          let l = (wrap directive_handler ( (Gram.parse interf) ) _loc cs) in
          (Ast.sgSem_of_list l)

      let print_interf =
       fun ?input_file:_ ->
        fun ?output_file:_ -> fun _ -> (failwith "No interface printer")

      let print_implem =
       fun ?input_file:_ ->
        fun ?output_file:_ -> fun _ -> (failwith "No implementation printer")

     end :
      (Sig.Camlp4Syntax with module Ast = Ast and module Ast = Ast
       and module Token = Gram.Token and module Token = Gram.Token
       and module Gram = Gram and module Gram = Gram and module Quotation =
       Quotation))
