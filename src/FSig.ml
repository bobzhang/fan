(** Main signatures for Fan *)
open Format;
open Ast;
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
    col_ctyp:ctyp
  };

type ty_info = {
    ty_name_expr: expr;
    (* int -> meta_int *)
    ty_expr: expr;
    (* int -> meta_int fmt x0 *)
    ty_id_expr: expr ;
    (* (ai,bi) *)
    ty_id_patt: patt;
    (* (ai,bi) *)
    ty_id_exprs: list expr;
    (* [ai;bi;ci] *)
    ty_id_patts: list patt;
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
   function of type [ident -> ident]
 *)
type basic_id_transform =
    [ = `Pre of string
    | `Post of string
    | `Fun of string -> string ];

type rhs_basic_id_transform =
    [ = basic_id_transform
    | `Exp of string -> expr ];

type full_id_transform =
    [ =  basic_id_transform
    | `Idents of list ident  -> ident
    (* decompose to a list of ident and compose as an ident *)          
    | `Ident of ident -> ident
    (* just pass the ident to user do ident transform *)
    | `Last of string -> ident
    (* pass the string, and << .$old$. .$return$. >>  *)      
    | `Obj of string -> string ];

type named_type =
    (string*ctyp)
and and_types =
    list named_type
and types =
    [= `Mutual of and_types
    | `Single of named_type ]
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
  val mk_variant:(string -> list ty_info  -> expr);
  val mk_tuple: (list ty_info -> expr );    
  val mk_record: (record_info -> expr);
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

  val trail: trail_info -> match_case;
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
  

