module Make =
 functor (Gram : (FanSig.Grammar.Static with type  Token.t =
                  FanSig.camlp4_token)) ->
  (struct
    module Ast = Camlp4Ast

    module Gram = Gram

    module Token = Gram.Token

    type warning = (FanLoc.t -> (string -> unit))

    let default_warning =
     fun loc ->
      fun txt -> (Format.eprintf "<W> %a: %s@." FanLoc.print loc txt)

    let current_warning = (ref default_warning)

    let print_warning =
     fun loc -> fun txt -> ((current_warning.contents) loc txt)

    let a_CHAR = (Gram.mk "a_CHAR")

    let a_FLOAT = (Gram.mk "a_FLOAT")

    let a_INT = (Gram.mk "a_INT")

    let a_INT32 = (Gram.mk "a_INT32")

    let a_INT64 = (Gram.mk "a_INT64")

    let a_LABEL = (Gram.mk "a_LABEL")

    let a_LIDENT = (Gram.mk "a_LIDENT")

    let a_NATIVEINT = (Gram.mk "a_NATIVEINT")

    let a_OPTLABEL = (Gram.mk "a_OPTLABEL")

    let a_STRING = (Gram.mk "a_STRING")

    let a_UIDENT = (Gram.mk "a_UIDENT")

    let a_ident = (Gram.mk "a_ident")

    let amp_ctyp = (Gram.mk "amp_ctyp")

    let and_ctyp = (Gram.mk "and_ctyp")

    let match_case = (Gram.mk "match_case")

    let match_case0 = (Gram.mk "match_case0")

    let binding = (Gram.mk "binding")

    let class_declaration = (Gram.mk "class_declaration")

    let class_description = (Gram.mk "class_description")

    let class_expr = (Gram.mk "class_expr")

    let class_fun_binding = (Gram.mk "class_fun_binding")

    let class_fun_def = (Gram.mk "class_fun_def")

    let class_info_for_class_expr = (Gram.mk "class_info_for_class_expr")

    let class_info_for_class_type = (Gram.mk "class_info_for_class_type")

    let class_longident = (Gram.mk "class_longident")

    let class_longident_and_param = (Gram.mk "class_longident_and_param")

    let class_name_and_param = (Gram.mk "class_name_and_param")

    let class_sig_item = (Gram.mk "class_sig_item")

    let class_signature = (Gram.mk "class_signature")

    let class_str_item = (Gram.mk "class_str_item")

    let class_structure = (Gram.mk "class_structure")

    let class_type = (Gram.mk "class_type")

    let class_type_declaration = (Gram.mk "class_type_declaration")

    let class_type_longident = (Gram.mk "class_type_longident")

    let class_type_longident_and_param =
     (Gram.mk "class_type_longident_and_param")

    let class_type_plus = (Gram.mk "class_type_plus")

    let comma_ctyp = (Gram.mk "comma_ctyp")

    let comma_expr = (Gram.mk "comma_expr")

    let comma_ipatt = (Gram.mk "comma_ipatt")

    let comma_patt = (Gram.mk "comma_patt")

    let comma_type_parameter = (Gram.mk "comma_type_parameter")

    let constrain = (Gram.mk "constrain")

    let constructor_arg_list = (Gram.mk "constructor_arg_list")

    let constructor_declaration = (Gram.mk "constructor_declaration")

    let constructor_declarations = (Gram.mk "constructor_declarations")

    let ctyp = (Gram.mk "ctyp")

    let cvalue_binding = (Gram.mk "cvalue_binding")

    let direction_flag = (Gram.mk "direction_flag")

    let direction_flag_quot = (Gram.mk "direction_flag_quot")

    let dummy = (Gram.mk "dummy")

    let eq_expr = (Gram.mk "eq_expr")

    let expr = (Gram.mk "expr")

    let expr_eoi = (Gram.mk "expr_eoi")

    let field_expr = (Gram.mk "field_expr")

    let field_expr_list = (Gram.mk "field_expr_list")

    let fun_binding = (Gram.mk "fun_binding")

    let fun_def = (Gram.mk "fun_def")

    let ident = (Gram.mk "ident")

    let implem = (Gram.mk "implem")

    let interf = (Gram.mk "interf")

    let ipatt = (Gram.mk "ipatt")

    let ipatt_tcon = (Gram.mk "ipatt_tcon")

    let label = (Gram.mk "label")

    let label_declaration = (Gram.mk "label_declaration")

    let label_declaration_list = (Gram.mk "label_declaration_list")

    let label_expr = (Gram.mk "label_expr")

    let label_expr_list = (Gram.mk "label_expr_list")

    let label_ipatt = (Gram.mk "label_ipatt")

    let label_ipatt_list = (Gram.mk "label_ipatt_list")

    let label_longident = (Gram.mk "label_longident")

    let label_patt = (Gram.mk "label_patt")

    let label_patt_list = (Gram.mk "label_patt_list")

    let labeled_ipatt = (Gram.mk "labeled_ipatt")

    let let_binding = (Gram.mk "let_binding")

    let meth_list = (Gram.mk "meth_list")

    let meth_decl = (Gram.mk "meth_decl")

    let module_binding = (Gram.mk "module_binding")

    let module_binding0 = (Gram.mk "module_binding0")

    let module_declaration = (Gram.mk "module_declaration")

    let module_expr = (Gram.mk "module_expr")

    let module_longident = (Gram.mk "module_longident")

    let module_longident_with_app = (Gram.mk "module_longident_with_app")

    let module_rec_declaration = (Gram.mk "module_rec_declaration")

    let module_type = (Gram.mk "module_type")

    let package_type = (Gram.mk "package_type")

    let more_ctyp = (Gram.mk "more_ctyp")

    let name_tags = (Gram.mk "name_tags")

    let opt_as_lident = (Gram.mk "opt_as_lident")

    let opt_class_self_patt = (Gram.mk "opt_class_self_patt")

    let opt_class_self_type = (Gram.mk "opt_class_self_type")

    let opt_comma_ctyp = (Gram.mk "opt_comma_ctyp")

    let opt_dot_dot = (Gram.mk "opt_dot_dot")

    let row_var_flag_quot = (Gram.mk "row_var_flag_quot")

    let opt_eq_ctyp = (Gram.mk "opt_eq_ctyp")

    let opt_expr = (Gram.mk "opt_expr")

    let opt_meth_list = (Gram.mk "opt_meth_list")

    let opt_mutable = (Gram.mk "opt_mutable")

    let mutable_flag_quot = (Gram.mk "mutable_flag_quot")

    let opt_polyt = (Gram.mk "opt_polyt")

    let opt_private = (Gram.mk "opt_private")

    let private_flag_quot = (Gram.mk "private_flag_quot")

    let opt_rec = (Gram.mk "opt_rec")

    let rec_flag_quot = (Gram.mk "rec_flag_quot")

    let opt_virtual = (Gram.mk "opt_virtual")

    let virtual_flag_quot = (Gram.mk "virtual_flag_quot")

    let opt_override = (Gram.mk "opt_override")

    let override_flag_quot = (Gram.mk "override_flag_quot")

    let opt_when_expr = (Gram.mk "opt_when_expr")

    let patt = (Gram.mk "patt")

    let patt_as_patt_opt = (Gram.mk "patt_as_patt_opt")

    let patt_eoi = (Gram.mk "patt_eoi")

    let patt_tcon = (Gram.mk "patt_tcon")

    let phrase = (Gram.mk "phrase")

    let poly_type = (Gram.mk "poly_type")

    let row_field = (Gram.mk "row_field")

    let sem_expr = (Gram.mk "sem_expr")

    let sem_expr_for_list = (Gram.mk "sem_expr_for_list")

    let sem_patt = (Gram.mk "sem_patt")

    let sem_patt_for_list = (Gram.mk "sem_patt_for_list")

    let semi = (Gram.mk "semi")

    let sequence = (Gram.mk "sequence")

    let do_sequence = (Gram.mk "do_sequence")

    let sig_item = (Gram.mk "sig_item")

    let sig_items = (Gram.mk "sig_items")

    let star_ctyp = (Gram.mk "star_ctyp")

    let str_item = (Gram.mk "str_item")

    let str_items = (Gram.mk "str_items")

    let top_phrase = (Gram.mk "top_phrase")

    let type_constraint = (Gram.mk "type_constraint")

    let type_declaration = (Gram.mk "type_declaration")

    let type_ident_and_parameters = (Gram.mk "type_ident_and_parameters")

    let type_kind = (Gram.mk "type_kind")

    let type_longident = (Gram.mk "type_longident")

    let type_longident_and_parameters =
     (Gram.mk "type_longident_and_parameters")

    let type_parameter = (Gram.mk "type_parameter")

    let type_parameters = (Gram.mk "type_parameters")

    let typevars = (Gram.mk "typevars")

    let use_file = (Gram.mk "use_file")

    let val_longident = (Gram.mk "val_longident")

    let with_constr = (Gram.mk "with_constr")

    let expr_quot = (Gram.mk "quotation of expression")

    let patt_quot = (Gram.mk "quotation of pattern")

    let ctyp_quot = (Gram.mk "quotation of type")

    let str_item_quot = (Gram.mk "quotation of structure item")

    let sig_item_quot = (Gram.mk "quotation of signature item")

    let class_str_item_quot = (Gram.mk "quotation of class structure item")

    let class_sig_item_quot = (Gram.mk "quotation of class signature item")

    let module_expr_quot = (Gram.mk "quotation of module expression")

    let module_type_quot = (Gram.mk "quotation of module type")

    let class_type_quot = (Gram.mk "quotation of class type")

    let class_expr_quot = (Gram.mk "quotation of class expression")

    let with_constr_quot = (Gram.mk "quotation of with constraint")

    let binding_quot = (Gram.mk "quotation of binding")

    let rec_binding_quot = (Gram.mk "quotation of record binding")

    let match_case_quot =
     (Gram.mk "quotation of match_case (try/match/function case)")

    let module_binding_quot = (Gram.mk "quotation of module rec binding")

    let ident_quot = (Gram.mk "quotation of identifier")

    let prefixop = (Gram.mk "prefix operator (start with '!', '?', '~')")

    let infixop0 =
     (Gram.mk
       "infix operator (level 0) (comparison operators, and some others)")

    let infixop1 = (Gram.mk "infix operator (level 1) (start with '^', '@')")

    let infixop2 = (Gram.mk "infix operator (level 2) (start with '+', '-')")

    let infixop3 =
     (Gram.mk "infix operator (level 3) (start with '*', '/', '%')")

    let infixop4 =
     (Gram.mk "infix operator (level 4) (start with \"**\") (right assoc)")

    open FanSig

    let _ = (Gram.extend ( (top_phrase : 'top_phrase Gram.t) ) (
              ((fun ()
                  ->
                 (None , (
                  [(None , None , (
                    [((
                      [(
                       (Gram.Stoken
                         (( function | EOI -> (true) | _ -> (false) ), "EOI"))
                       )] ), (
                      (Gram.mk_action (
                        fun (__camlp4_0 :
                          Gram.token) ->
                         fun (_loc :
                           FanLoc.t) ->
                          (match __camlp4_0 with
                           | EOI -> ((None) : 'top_phrase)
                           | _ -> assert false) )) ))] ))] ))) () ) ))

    module AntiquotSyntax =
     struct
      module Ast = Ast

      module Gram = Gram

      let antiquot_expr = (Gram.mk "antiquot_expr")

      let antiquot_patt = (Gram.mk "antiquot_patt")

      let _ = (
      (Gram.extend ( (antiquot_expr : 'antiquot_expr Gram.t) ) (
        ((fun ()
            ->
           (None , (
            [(None , None , (
              [((
                [( (Gram.Snterm (Gram.obj ( (expr : 'expr Gram.t) ))) ); (
                 (Gram.Stoken
                   (( function | EOI -> (true) | _ -> (false) ), "EOI")) )]
                ), (
                (Gram.mk_action (
                  fun (__camlp4_0 :
                    Gram.token) ->
                   fun (x :
                     'expr) ->
                    fun (_loc :
                      FanLoc.t) ->
                     (match __camlp4_0 with
                      | EOI -> (x : 'antiquot_expr)
                      | _ -> assert false) )) ))] ))] ))) () ) ))
      );
      (Gram.extend ( (antiquot_patt : 'antiquot_patt Gram.t) ) (
        ((fun ()
            ->
           (None , (
            [(None , None , (
              [((
                [( (Gram.Snterm (Gram.obj ( (patt : 'patt Gram.t) ))) ); (
                 (Gram.Stoken
                   (( function | EOI -> (true) | _ -> (false) ), "EOI")) )]
                ), (
                (Gram.mk_action (
                  fun (__camlp4_0 :
                    Gram.token) ->
                   fun (x :
                     'patt) ->
                    fun (_loc :
                      FanLoc.t) ->
                     (match __camlp4_0 with
                      | EOI -> (x : 'antiquot_patt)
                      | _ -> assert false) )) ))] ))] ))) () ) ))

      let parse_expr =
       fun loc -> fun str -> (Gram.parse_string antiquot_expr loc str)

      let parse_patt =
       fun loc -> fun str -> (Gram.parse_string antiquot_patt loc str)

     end

    module Quotation = (Quotation.Make)(struct end)

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

    module AstFilters = (AstFilters.Make)(struct end)

   end :
    (Sig.Camlp4Syntax with module Token = Gram.Token and module Token =
     Gram.Token and module Gram = Gram))