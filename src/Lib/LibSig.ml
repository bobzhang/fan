open Format;
type vrn =
    [TyVrn
    | TyVrnEq
    | TyVrnSup
    | TyVrnInf
    | TyVrnInfSup
    | TyAbstr ];
type trail_info = (vrn*int);

type col = {
    col_label:string;
    col_mutable:bool;
    col_ctyp:Ast.ctyp
  };

type ty_info = {
    ty_name_expr: Ast.expr;
    (* int -> meta_int *)
    ty_expr: Ast.expr;
    (* int -> meta_int fmt x0 *)
    ty_id_expr: Ast.expr ;
    (* (ai,bi) *)
    ty_id_patt: Ast.patt;
    (* (ai,bi) *)
    ty_id_exprs: list Ast.expr;
    (* [ai;bi;ci] *)
    ty_id_patts: list Ast.patt;
    (* [ai;bi;ci]*)
  }
;

(* Feed to user to compose an expression node *)
type record_col = {
    record_label: string ;
    record_mutable: bool ;
    record_info: ty_info;
  };
type record_info = list record_col ;

(* types below are used to tell fan how to produce
   function of type [Ast.ident -> Ast.ident]
 *)
type basic_id_transform =
    [ = `Pre of string
    | `Post of string
    | `Fun of string -> string ];

type rhs_basic_id_transform =
    [ = basic_id_transform
    | `Exp of string -> Ast.expr ];

type full_id_transform =
    [ =  basic_id_transform
    | `Idents of list Ast.ident  -> Ast.ident
    (* decompose to a list of ident and compose as an ident *)          
    | `Ident of Ast.ident -> Ast.ident
    (* just pass the ident to user do ident transform *)
    | `Last of string -> Ast.ident
    (* pass the string, and {| .$old$. .$return$. |}  *)      
    | `Obj of string -> string ];

type named_type = (string*Ast.ctyp)
and and_types = list named_type
and types =
    [ Mutual of and_types
    | Single of named_type ]
and module_types = list types;

type obj_dest =
  [Obj of k
  |Str_item]
and k =
  [ Fold
  | Iter
  | Map];

(* preserved keywords for the generator *)
let preserve =
  ["self"; "self_type"; "unit"; "result"];
module type Config = sig
  val mk_variant:(string -> list ty_info  -> Ast.expr);
  val mk_tuple: (list ty_info -> Ast.expr );    
  val mk_record: (record_info -> Ast.expr);
  val arity: int;

  val left_type_variable: basic_id_transform;
  (* left type variable transformation
     ['a -> mf_a]
   *)  

  val right_type_variable: rhs_basic_id_transform;
  (* right type variable transformatoin.
     in most cases, it's similiar to var *)
    
  val right_type_id: full_id_transform;
  (* [t -> pp_print_t]  *)  

  val left_type_id: basic_id_transform;
  (* For structure_item generation, similar to
     right.
     Sometimes, it's not the same object, for example
     [t -> t]
   *)

  val trail: trail_info -> Ast.match_case;
  val names: list string;
end; 


type warning_type =
  [ Abstract of string 
  | Qualified of string ];

let string_of_warning_type =
  sprintf "Warning: %a\n" (fun _ ->
    fun
      [Abstract s -> "Abstract: " ^ s
      |Qualified s -> "Qualified: " ^ s]
                          );
  

module type Grammar = sig
  type t 'a;
  type loc ;
  val eoi_entry : t 'a -> t 'a;
  val parse_quot_string_with_filter :
      t 'a ->
        ('a -> 'b) -> loc -> option string -> string -> 'b;
  val parse_quot_string :
     t 'a -> loc -> option string -> string -> 'a;
  val add_quotation :
      ?antiquot_expander:
      < expr : Ast.expr -> Ast.expr; patt : Ast.patt -> Ast.patt; .. > ->
        string -> 
        ~entry: (t 'a) ->
        ~mexpr: (FanLoc.t -> 'a -> Ast.expr) ->
        ~mpatt: (FanLoc.t -> 'a -> Ast.patt) ->
        unit ;
  val add_quotation_of_str_item :
      ~name: string -> ~entry: (t Ast.str_item) -> unit;
  val add_quotation_of_str_item_with_filter :
     ~name: string ->
     ~entry: (t 'a) -> ~filter: ('a -> Ast.str_item) -> unit;
  val add_quotation_of_expr :
    ~name: string -> ~entry: t Ast.expr -> unit;
  val add_quotation_of_patt :
    ~name: string -> ~entry: t Ast.patt -> unit;
  val add_quotation_of_class_str_item :
    ~name: string -> ~entry: t Ast.class_str_item -> unit;
  val add_quotation_of_match_case :
    ~name: string -> ~entry: t Ast.match_case -> unit;
end;
