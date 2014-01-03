
(** Utilities for Fan's deriving mechanism *)
open Astfn
type vrn =
  | Sum 
  | TyVrnEq
  | TyVrnSup
  | TyVrnInf
  | TyVrnInfSup
  | TyAbstr


type col = {
    label : string;
    is_mutable : bool;
    ty : ctyp
  }
type ty_info = {
    name_exp: exp; (*  [meta_int] *)
    info_exp: exp; (* [meta_int fmt test _a3] *)
    ep0: ep; (* _a3*)
    id_ep: ep; (* (_a3,_b3) *)
    id_eps: ep list ; (* [_a3;_b3] *)
    ty: ctyp; (* int *) 
  }

type vbranch =
   [ `variant of (string* ctyp list )
   | `abbrev of ident ]
type branch =
   [ `branch of (string * ctyp list) ]


type destination =
  |Obj of kind
  |Str_item
and kind =
  | Fold
  | Iter (* Iter style *) 
  | Map (* Map style *)
  | Concrete of ctyp


type warning_type =
  | Abstract of string 
  | Qualified of string 


(* Feed to user to compose an expession node *)
type record_col = {
    label: string ;
    is_mutable: bool ;
    info: ty_info;
  }
type record_info =  record_col list

(* types below are used to tell fan how to produce
   function of type [ident -> ident]
 *)
type basic_id_transform =
    [ `Pre of string
    | `Post of string
    | `Fun of (string->string) ]

type rhs_basic_id_transform =
    [ basic_id_transform
    | `Exp of string -> exp ]

type full_id_transform =
    [  basic_id_transform
    | `Idents of  vid list  -> vid 
    (* decompose to a list of ident and compose as an ident *)          
    | `Id of vid -> vid
    (* just pass the ident to user do ident transform *)
    | `Last of string -> vid
    (* pass the string, and << .$old$. .$return$. >>  *)      
    | `Obj of  (string -> string) ]
        

val mapi_exp :
    ?arity:int ->
      ?names:string list -> f:(ctyp -> exp) -> int -> ctyp -> ty_info      

val tuple_exp_of_ctyp :
    ?arity:int ->
      ?names:string list ->
        mk_tuple:(ty_info list -> exp) ->
          f:(ctyp -> exp ) -> ctyp -> exp

val mk_record: ?arity:int -> col list -> ep

val list_of_record : name_ctyp -> col list



(**
     [result] is a keyword
   {[
   let (name,len) =
   (%stru{ type list 'a  'b = [A of int | B of 'a] }
     |> function %stru{type $x } -> name_length_of_tydcl x)
   let f = mk_method_type ~number:2 ~prefix:["fmt"]
   (%ident{ $lid:name },len);

   open Fan_sig
   
   f (Obj Map)|> eprint;
   ! 'all_a0 'all_a1 'all_b0 'all_b1.
  ('self_type -> 'fmt -> 'all_a0 -> 'all_a0 -> 'all_b0) ->
  ('self_type -> 'fmt -> 'all_a1 -> 'all_a1 -> 'all_b1) ->
  'fmt ->
  list 'all_a0 'all_a1 -> list 'all_a0 'all_a1 -> list 'all_b0 'all_b1

  f (Obj Iter)|> eprint;
  ! 'all_a0 'all_a1.
  ('self_type -> 'fmt -> 'all_a0 -> 'all_a0 -> 'result) ->
  ('self_type -> 'fmt -> 'all_a1 -> 'all_a1 -> 'result) ->
  'fmt -> list 'all_a0 'all_a1 -> list 'all_a0 'all_a1 -> 'result
  
  f (Obj Fold) |> eprint;
  ! 'all_a0 'all_a1.
  ('self_type -> 'fmt -> 'all_a0 -> 'all_a0 -> 'self_type) ->
  ('self_type -> 'fmt -> 'all_a1 -> 'all_a1 -> 'self_type) ->
  'fmt -> list 'all_a0 'all_a1 -> list 'all_a0 'all_a1 -> 'self_type
  
  f Str_item |> eprint;
  ! 'all_a0 'all_a1.
  ('fmt -> 'all_a0 -> 'all_a0 -> 'result) ->
  ('fmt -> 'all_a1 -> 'all_a1 -> 'result) ->
  'fmt -> list 'all_a0 'all_a1 -> list 'all_a0 'all_a1 -> 'result]} *)
val mk_method_type :
    number:int ->
      id:ident -> 
        prefix:int ->
      int -> destination -> (ctyp*ctyp)


      

        
val mk_obj : string -> string -> clfield -> stru
val is_recursive : decl -> bool
val is_abstract : decl -> bool

val abstract_list : decl -> int option
    
val qualified_app_list : ctyp -> (ident * ctyp list) option



val reduce_data_ctors:
    or_ctyp ->
      'a -> compose:('e -> 'a  -> 'a) -> (string -> ctyp list -> 'e) -> 'a    

(* @raise Invalid_argument *)        
(* val of_stru: stru -> decl *)

val view_sum: or_ctyp -> branch list
val view_variant: row_field -> vbranch list    




val transform : full_id_transform -> vid -> exp
val basic_transform :
  [< `Fun of string -> string | `Post of string | `Pre of string ] ->
  string -> string
val right_transform :
  [< `Exp of string -> exp
   | `Fun of string -> string
   | `Post of string
   | `Pre of string ] ->
  string -> exp
    
val gen_tuple_abbrev : arity:int ->
  annot:ctyp ->
  destination:destination -> ident -> exp -> case

val pp_print_warning_type: Format.formatter -> warning_type -> unit


