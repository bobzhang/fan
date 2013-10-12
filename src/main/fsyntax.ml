

open FAst

type warning = FLoc.t -> string -> unit
    
let default_warning loc txt = Format.eprintf "<W> %a: %s@." FLoc.print loc txt
    
let current_warning = ref default_warning
    
let print_warning loc txt = !current_warning loc txt;;


%create{ Fgram
  a_ident aident amp_ctyp and_ctyp case
  case0
  bind
  class_declaration
  class_description
  clexp class_fun_bind class_fun_def  class_info_for_cltyp
  class_longident  class_name_and_param clsigi class_signature
  clfield class_structure cltyp cltyp_declaration
  cltyp_longident 
  cltyp_plus com_ctyp comma_ctyp comma_exp comma_ipat comma_pat comma_type_parameter
  constrain constructor_arg_list constructor_declaration constructor_declarations
  ctyp cvalue_bind flag direction_flag_quot
   eq_exp exp exp_eoi field_exp field_exp_list fun_bind
  fun_def ident implem interf ipat ipat_tcon pat_tcon
  label_declaration  label_declaration_list label_exp label_exp_list 
  label_pat_list label_pat label_longident
  let_bind meth_list meth_decl mbind
  mbind  mbind0 mexp  module_longident  module_longident_with_app  module_rec_declaration
  mtyp name_tags opt_class_self_pat opt_class_self_type  opt_comma_ctyp  opt_dot_dot  row_var_flag_quot
  opt_exp  opt_meth_list  opt_mutable  mutable_flag_quot  opt_polyt  opt_private
  private_flag_quot  opt_rec  rec_flag_quot  opt_virtual  virtual_flag_quot  opt_override
  override_flag_quot  pat  pat_as_pat_opt  pat_eoi
  row_field  sem_exp  sem_exp_for_list  sem_pat  sem_pat_for_list  semi  sequence
  sigi  sigis  star_ctyp  stru  strus  top_phrase
  type_declaration  type_ident_and_parameters  type_longident  type_longident_and_parameters
  type_parameter  type_parameters  typevars  val_longident  constr  exp_quot  pat_quot
  ctyp_quot  stru_quot  sigi_quot  clfield_quot  clsigi_quot  mexp_quot
  mtyp_quot  cltyp_quot  clexp_quot  constr_quot  bind_quot  rec_exp_quot
  module_declaration type_info type_repr
  (infixop0 "or ||")
  (infixop1  "& &&")
  (infixop2 "infix operator (level 2) (comparison operators, and some others)")
  (infixop3 "infix operator (level 3) (start with '^', '@')")
  (infixop4 "infix operator (level 4) (start with '+', '-')")
  (infixop5 "infix operator (level 5) (start with '*', '/', '%')")
  (infixop6 "infix operator (level 6) (start with \"**\") (right assoc)")

  (prefixop "prefix operator (start with '!', '?', '~')")
  (case_quot "quotation of case (try/match/function case)")

  module_longident_dot_lparen  sequence'  fun_def  
  mbind_quot ident_quot string_list     
  method_opt_override  value_val_opt_override  unquoted_typevars  lang with_exp_lang
  with_stru_lang
  (* for the grammar module *)  



  dot_lstrings
  a_string
  a_lident
  a_uident
  luident
  uident
  vid 
  astr
  };;
  
let antiquot_exp = Fgram.eoi_entry exp 
let antiquot_pat = Fgram.eoi_entry pat
let antiquot_ident = Fgram.eoi_entry ident
let parse_exp loc str = Fgram.parse_string antiquot_exp ~loc str
let parse_pat loc str = Fgram.parse_string antiquot_pat ~loc str
let parse_ident loc str = Fgram.parse_string antiquot_ident ~loc str

let anti_filter = Ant.antiquot_expander  ~parse_exp  ~parse_pat
let exp_filter (x:ep) = (anti_filter#exp (x:>exp))
let pat_filter (x:ep) = (anti_filter#pat (x:>pat))

let anti_filter_n = AntN.antiquot_expander  ~parse_exp  ~parse_pat
let exp_filter_n (x:ep) = anti_filter_n#exp (x:>exp)
let pat_filter_n (x:ep) = anti_filter_n#pat (x:>pat)



