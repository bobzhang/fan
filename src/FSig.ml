(** Main signatures for Fan *)

open LibUtil;
open FanAst;

type vrn =
    [ Sum 
    | TyVrnEq
    | TyVrnSup
    | TyVrnInf
    | TyVrnInfSup
    | TyAbstr ];
type trail_info = (vrn*int);

(* [collumn] meta data for the record *)
type col = {
    label:string;
    is_mutable:bool;
    ctyp:ctyp
  };

(* a series of [ast] nodes generated by the type *)
type ty_info = {
    name_expr: expr;
    (* [int] -> [meta_int] *)

    expr: expr;
    (* int -> [meta_int fmt x0] *)

    exp0: expr;
    (* ai *)
    pat0: patt;
    (* ai *)
    id_expr: expr ;
    (* (ai,bi) *)

    id_patt: patt;
    (* (ai,bi) *)

    id_exprs: list expr;
    (* [ai;bi;ci] *)

    id_patts: list patt;
    (* [ai;bi;ci]*)

    ty: ctyp;
    (* int *) 
  }
;

type vbranch =
   [= `variant of (string* list ctyp)
   | `abbrev of ident (* ctyp *) ];
type branch =
   [= `branch of (string * list ctyp) ];
(* Feed to user to compose an expression node *)
type record_col = {
    label: string ;
    is_mutable: bool ;
    info: ty_info;
  };
type record_info = list record_col ;

(* types below are used to tell fan how to produce
   function of type [ident -> ident]
 *)
type basic_id_transform =
    [ = `Pre of string
    | `Post of string
    | `Fun of id string ];

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
    | `Obj of id string ];

open StdLib;

{:fans|keep on; derive (Print); |};

{:ocaml|
type named_type = (string*ctyp)
and and_types =
    list named_type
and types =
    [= `Mutual of and_types
    | `Single of named_type ]
and module_types = list types;

type destination =
  [Obj of kind
  |Str_item]
and kind =
  [ Fold
  | Iter (* Iter style *) 
  | Map (* Map style *)];

type warning_type =
  [ Abstract of string 
  | Qualified of string ];
 
  |};

  
let str_item_of_module_types ~f:(aux:named_type -> ctyp) (x:module_types) : str_item =
  (* let aux (_,ty)= ty in *)
  let _loc = FanLoc.ghost in
  sem_of_list
    (List.map
       (fun
         [`Mutual tys -> {:str_item| type $(and_of_list (List.map aux tys)) |}
         |`Single ty ->
             {:str_item| type $(aux ty)|}] ) x );



(*
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

  (* transforming the constructor name(it only has effect in the pattern part) *)  
  val cons_transform: option (string->string);  
end; 
*)


(* let string_of_warning_type = *)
(*   sprintf "Warning: %a\n" (fun _ -> *)
(*     fun *)
(*       [Abstract s -> "Abstract: " ^ s *)
(*       |Qualified s -> "Qualified: " ^ s] *)
(*                           ); *)
  

