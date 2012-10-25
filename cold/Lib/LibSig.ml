open Format
type vrn =  TyVrn | TyVrnEq | TyVrnSup | TyVrnInf | TyVrnInfSup | TyAbstr 
type trail_info =  ( vrn * int ) 
type col = 
{ col_label: string ; col_mutable: bool ; col_ctyp: Ast.ctyp } 
type ty_info = 
{ ty_name_expr: Ast.expr ; ty_expr: Ast.expr ; ty_id_expr: Ast.expr ;
   ty_id_patt: Ast.patt ; ty_id_exprs: Ast.expr  list ;
                                                         ty_id_patts:
                                                          Ast.patt  list } 
type record_col = 
{ record_label: string ; record_mutable: bool ; record_info: ty_info } 
type record_info =   record_col  list  
type basic_id_transform = 
[ `Pre of  string |`Post of  string |`Fun of ( string  ->  string ) ] 
type rhs_basic_id_transform = 
[  basic_id_transform |`Exp of ( string  ->  Ast.expr ) ] 
type full_id_transform = 
[  basic_id_transform
  |`Idents of ( Ast.ident  list  ->  Ast.ident ) |`Ident of
                                                    ( Ast.ident  -> 
                                                      Ast.ident ) |`Last
                                                                    of
                                                                    (
                                                                    string 
                                                                    -> 
                                                                    Ast.ident
                                                                    ) |
  `Obj of ( string  ->  string ) ] 
type named_type =  ( string * Ast.ctyp ) and and_types = 
 named_type  list  and types = 
  Mutual of  and_types  | Single of  named_type  and module_types = 
 types  list  
type obj_dest =  Obj of  k  | Str_item and k =  Fold | Iter | Map 
let  preserve = ["self";"self_type";"unit";"result"]
module type Config =
  sig 
   val mk_variant :
     ( string  -> ( ty_info  list  ->  Ast.expr ) ) 
  val mk_tuple : ( ty_info  list  ->  Ast.expr ) 
  val mk_record : ( record_info  ->  Ast.expr ) 
  val arity :  int  val left_type_variable :  basic_id_transform 
  val right_type_variable :  rhs_basic_id_transform 
  val right_type_id :  full_id_transform 
  val left_type_id :  basic_id_transform 
  val trail : ( trail_info  ->  Ast.match_case ) 
  val names :  string  list  end
type warning_type = 
  Abstract of  string  | Qualified of  string  
let  string_of_warning_type =
  (sprintf "Warning: %a\n" (
    (fun (_) ->
      
      function
      | Abstract(s) -> ("Abstract: " ^ s)
      | Qualified(s) -> ("Qualified: " ^ s)) ))
module type Grammar =
  sig  type 'a t   type loc   val eoi_entry : ('a  t  -> 'a  t ) 
  val parse_quot_string_with_filter :
    ('a  t  ->
      (('a  -> 'b )  ->
        ( loc  -> ( string  option  -> ( string  -> 'b ) ) ) ) ) 
  val parse_quot_string :
    ('a  t  ->
      ( loc  -> ( string  option  -> ( string  -> 'a ) ) ) ) 
  val add_quotation :
    (?antiquot_expander:<
                          expr :( Ast.expr  ->  Ast.expr )  ;patt
                                                               :(
                                                                  Ast.patt 
                                                                  -> 
                                                                  Ast.patt
                                                                  )  ;..
                          > ->
      ( string  ->
        (entry:'a  t  ->
          (mexpr:( FanLoc.t  -> ('a  ->  Ast.expr ) )  ->
            (mpatt:( FanLoc.t  -> ('a  ->  Ast.patt ) )  ->  unit
              ) ) ) ) ) 
  val add_quotation_of_str_item :
    (name: string  -> (entry: Ast.str_item  t  ->  unit ) ) 
  val add_quotation_of_str_item_with_filter :
    (name: string  ->
      (entry:'a  t  ->
        (filter:('a  ->  Ast.str_item )  ->  unit ) ) ) 
  val add_quotation_of_expr :
    (name: string  -> (entry: Ast.expr  t  ->  unit ) ) 
  val add_quotation_of_patt :
    (name: string  -> (entry: Ast.patt  t  ->  unit ) ) 
  val add_quotation_of_class_str_item :
    (name: string  -> (entry: Ast.class_str_item  t  ->  unit ) ) 
  val add_quotation_of_match_case :
    (name: string  -> (entry: Ast.match_case  t  ->  unit ) ) 
  end