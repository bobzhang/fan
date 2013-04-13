open Ast

open AstLoc

open LibUtil

type warning = FanLoc.t -> string -> unit 

let default_warning loc txt =
  Format.eprintf "<W> %a: %s@." FanLoc.print loc txt

let current_warning = ref default_warning

let print_warning loc txt = current_warning.contents loc txt

let a_ident = Gram.mk "a_ident"

let aident = Gram.mk "aident"

let amp_ctyp = Gram.mk "amp_ctyp"

let and_ctyp = Gram.mk "and_ctyp"

let case = Gram.mk "case"

let case0 = Gram.mk "case0"

let binding = Gram.mk "binding"

let class_declaration = Gram.mk "class_declaration"

let class_description = Gram.mk "class_description"

let clexp = Gram.mk "clexp"

let class_fun_binding = Gram.mk "class_fun_binding"

let class_fun_def = Gram.mk "class_fun_def"

let class_info_for_cltyp = Gram.mk "class_info_for_cltyp"

let class_longident = Gram.mk "class_longident"

let class_name_and_param = Gram.mk "class_name_and_param"

let clsigi = Gram.mk "clsigi"

let class_signature = Gram.mk "class_signature"

let clfield = Gram.mk "clfield"

let class_structure = Gram.mk "class_structure"

let cltyp = Gram.mk "cltyp"

let cltyp_declaration = Gram.mk "cltyp_declaration"

let cltyp_longident = Gram.mk "cltyp_longident"

let cltyp_plus = Gram.mk "cltyp_plus"

let comma_ctyp = Gram.mk "comma_ctyp"

let comma_exp = Gram.mk "comma_exp"

let comma_ipat = Gram.mk "comma_ipat"

let comma_pat = Gram.mk "comma_pat"

let comma_type_parameter = Gram.mk "comma_type_parameter"

let constrain = Gram.mk "constrain"

let constructor_arg_list = Gram.mk "constructor_arg_list"

let constructor_declaration = Gram.mk "constructor_declaration"

let constructor_declarations = Gram.mk "constructor_declarations"

let ctyp = Gram.mk "ctyp"

let cvalue_binding = Gram.mk "cvalue_binding"

let direction_flag = Gram.mk "direction_flag"

let direction_flag_quot = Gram.mk "direction_flag_quot"

let dummy = Gram.mk "dummy"

let eq_exp = Gram.mk "eq_exp"

let exp = Gram.mk "exp"

let exp_eoi = Gram.mk "exp_eoi"

let field_exp = Gram.mk "field_exp"

let field_exp_list = Gram.mk "field_exp_list"

let fun_binding = Gram.mk "fun_binding"

let fun_def = Gram.mk "fun_def"

let ident = Gram.mk "ident"

let implem = Gram.mk "implem"

let interf = Gram.mk "interf"

let ipat = Gram.mk "ipat"

let ipat_tcon = Gram.mk "ipat_tcon"

let pat_tcon = Gram.mk "pat_tcon"

let label_declaration = Gram.mk "label_declaration"

let label_declaration_list = Gram.mk "label_declaration_list"

let label_exp = Gram.mk "label_exp"

let label_exp_list = Gram.mk "label_exp_list"

let label_pat_list = Gram.mk "label_pat_list"

let label_pat = Gram.mk "label_pat"

let label_longident = Gram.mk "label_longident"

let let_binding = Gram.mk "let_binding"

let meth_list = Gram.mk "meth_list"

let meth_decl = Gram.mk "meth_decl"

let mbind = Gram.mk "mbind"

let mbind = Gram.mk "mbind"

let mbind0 = Gram.mk "mbind0"

let mexp = Gram.mk "mexp"

let module_longident = Gram.mk "module_longident"

let module_longident_with_app = Gram.mk "module_longident_with_app"

let module_rec_declaration = Gram.mk "module_rec_declaration"

let mtyp = Gram.mk "mtyp"

let name_tags = Gram.mk "name_tags"

let opt_class_self_pat = Gram.mk "opt_class_self_pat"

let opt_class_self_type = Gram.mk "opt_class_self_type"

let opt_comma_ctyp = Gram.mk "opt_comma_ctyp"

let opt_dot_dot = Gram.mk "opt_dot_dot"

let row_var_flag_quot = Gram.mk "row_var_flag_quot"

let opt_exp = Gram.mk "opt_exp"

let opt_meth_list = Gram.mk "opt_meth_list"

let opt_mutable = Gram.mk "opt_mutable"

let mutable_flag_quot = Gram.mk "mutable_flag_quot"

let opt_polyt = Gram.mk "opt_polyt"

let opt_private = Gram.mk "opt_private"

let private_flag_quot = Gram.mk "private_flag_quot"

let opt_rec = Gram.mk "opt_rec"

let rec_flag_quot = Gram.mk "rec_flag_quot"

let opt_virtual = Gram.mk "opt_virtual"

let virtual_flag_quot = Gram.mk "virtual_flag_quot"

let opt_override = Gram.mk "opt_override"

let override_flag_quot = Gram.mk "override_flag_quot"

let pat = Gram.mk "pat"

let pat_as_pat_opt = Gram.mk "pat_as_pat_opt"

let pat_eoi = Gram.mk "pat_eoi"

let row_field = Gram.mk "row_field"

let sem_exp = Gram.mk "sem_exp"

let sem_exp_for_list = Gram.mk "sem_exp_for_list"

let sem_pat = Gram.mk "sem_pat"

let sem_pat_for_list = Gram.mk "sem_pat_for_list"

let semi = Gram.mk "semi"

let sequence = Gram.mk "sequence"

let sigi = Gram.mk "sigi"

let sigis = Gram.mk "sigis"

let star_ctyp = Gram.mk "star_ctyp"

let stru = Gram.mk "stru"

let strus = Gram.mk "strus"

let top_phrase = Gram.mk "top_phrase"

let type_declaration = Gram.mk "type_declaration"

let type_ident_and_parameters = Gram.mk "type_ident_and_parameters"

let type_longident = Gram.mk "type_longident"

let type_longident_and_parameters = Gram.mk "type_longident_and_parameters"

let type_parameter = Gram.mk "type_parameter"

let type_parameters = Gram.mk "type_parameters"

let typevars = Gram.mk "typevars"

let val_longident = Gram.mk "val_longident"

let constr = Gram.mk "constr"

let exp_quot = Gram.mk "exp_quot"

let pat_quot = Gram.mk "pat_quot"

let ctyp_quot = Gram.mk "ctyp_quot"

let stru_quot = Gram.mk "stru_quot"

let sigi_quot = Gram.mk "sigi_quot"

let clfield_quot = Gram.mk "clfield_quot"

let clsigi_quot = Gram.mk "clsigi_quot"

let mexp_quot = Gram.mk "mexp_quot"

let mtyp_quot = Gram.mk "mtyp_quot"

let cltyp_quot = Gram.mk "cltyp_quot"

let clexp_quot = Gram.mk "clexp_quot"

let constr_quot = Gram.mk "constr_quot"

let binding_quot = Gram.mk "binding_quot"

let rec_exp_quot = Gram.mk "rec_exp_quot"

let module_declaration = Gram.mk "module_declaration"

let type_info = Gram.mk "type_info"

let type_repr = Gram.mk "type_repr"

let infixop0 = Gram.mk "or ||"

let infixop1 = Gram.mk "& &&"

let infixop2 =
  Gram.mk "infix operator (level 2) (comparison operators, and some others)"

let infixop3 = Gram.mk "infix operator (level 3) (start with '^', '@')"

let infixop4 = Gram.mk "infix operator (level 4) (start with '+', '-')"

let infixop5 = Gram.mk "infix operator (level 5) (start with '*', '/', '%')"

let infixop6 =
  Gram.mk "infix operator (level 6) (start with \"**\") (right assoc)"

let prefixop = Gram.mk "prefix operator (start with '!', '?', '~')"

let case_quot = Gram.mk "quotation of case (try/match/function case)"

let module_longident_dot_lparen = Gram.mk "module_longident_dot_lparen"

let sequence' = Gram.mk "sequence'"

let fun_def = Gram.mk "fun_def"

let mbind_quot = Gram.mk "mbind_quot"

let ident_quot = Gram.mk "ident_quot"

let string_list = Gram.mk "string_list"

let method_opt_override = Gram.mk "method_opt_override"

let value_val_opt_override = Gram.mk "value_val_opt_override"

let unquoted_typevars = Gram.mk "unquoted_typevars"

let lang = Gram.mk "lang"

let with_exp_lang = Gram.mk "with_exp_lang"

let with_stru_lang = Gram.mk "with_stru_lang"

let symbol = Gram.mk "symbol"

let rule = Gram.mk "rule"

let meta_rule = Gram.mk "meta_rule"

let rule_list = Gram.mk "rule_list"

let psymbol = Gram.mk "psymbol"

let level = Gram.mk "level"

let level_list = Gram.mk "level_list"

let entry = Gram.mk "entry"

let extend_body = Gram.mk "extend_body"

let delete_rule_body = Gram.mk "delete_rule_body"

let rules = Gram.mk "rules"

let dot_lstrings = Gram.mk "dot_lstrings"

let a_string = Gram.mk "a_string"

let a_lident = Gram.mk "a_lident"

let a_uident = Gram.mk "a_uident"

let luident = Gram.mk "luident"

let uident = Gram.mk "uident"

let vid = Gram.mk "vid"

let astr = Gram.mk "astr"

let dot_namespace = Gram.mk "dot_namespace"

let antiquot_exp = Gram.eoi_entry exp

let antiquot_pat = Gram.eoi_entry pat

let antiquot_ident = Gram.eoi_entry ident

let parse_exp loc str = Gram.parse_string antiquot_exp ~loc str

let parse_pat loc str = Gram.parse_string antiquot_pat ~loc str

let parse_ident loc str = Gram.parse_string antiquot_ident ~loc str

let anti_filter = Ant.antiquot_expander ~parse_exp ~parse_pat

let exp_filter (x : ep) = anti_filter#exp (x :>exp)

let pat_filter (x : ep) = anti_filter#pat (x :>pat)

let wrap directive_handler pa init_loc cs =
  let rec loop loc =
    let (pl,stopped_at_directive) = pa loc cs in
    match stopped_at_directive with
    | Some new_loc ->
        let pl =
          match List.rev pl with
          | [] -> assert false
          | x::xs ->
              (match directive_handler x with
               | None  -> xs
               | Some x -> x :: xs) in
        (List.rev pl) @ (loop (FanLoc.join_end new_loc))
    | None  -> pl in
  loop init_loc

let parse_implem ?(directive_handler= fun _  -> None)  _loc cs =
  let l = wrap directive_handler (Gram.parse implem) _loc cs in
  match l with | [] -> None | l -> Some (sem_of_list l)

let parse_interf ?(directive_handler= fun _  -> None)  _loc cs =
  let l = wrap directive_handler (Gram.parse interf) _loc cs in
  match l with | [] -> None | l -> Some (sem_of_list l)

let print_interf ?input_file:_  ?output_file:_  _ =
  failwith "No interface printer"

let print_implem ?input_file:_  ?output_file:_  _ =
  failwith "No implementation printer"

module Options =
  struct
    type spec_list = (string * FanArg.spec * string) list 
    let init_spec_list = ref []
    let init spec_list = init_spec_list := spec_list
    let add (name,spec,descr) =
      init_spec_list := (init_spec_list.contents @ [(name, spec, descr)])
    let adds ls = init_spec_list := (init_spec_list.contents @ ls)
  end