open FAstN
type vrn =
  | Sum 
  | TyVrnEq
  | TyVrnSup
  | TyVrnInf
  | TyVrnInfSup
  | TyAbstr


type col = {
    col_label:string;
    col_mutable:bool;
    col_ctyp:ctyp
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
    re_label: string ;
    re_mutable: bool ;
    re_info: ty_info;
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
        
val arrow_of_list : ctyp list -> ctyp
val app_arrow : ctyp list -> ctyp -> ctyp
val ( <+ ) : string list -> ctyp -> ctyp
(* val ( +> ) : ctyp list -> ctyp -> ctyp *)

(** {[
    match {:stru< type 'a list  = [A of int | B of 'a] |} with
    {:stru| type $x |} -> name_length_of_tydcl x 
    ("list",1)  ]} *)
val name_length_of_tydcl : typedecl -> string * int



val gen_ty_of_tydcl : off:int -> typedecl -> ctyp

(**
  {[
  of_id_len ~off:2 (<:ident< Loc.t >> , 3 ) |> eprint;
  ('all_c0, 'all_c1, 'all_c2) Loc.t]} *)     
val of_id_len : off:int -> ident * int -> ctyp



(**
  {[
  ( {:stru-| type 'a list  = [A of int | B of 'a] |} |>
  function |  {:stru-| type $x |} -> name_length_of_tydcl x
  |> of_name_len ~off:1  );
  list 'all_b0

  ( <:stru< type list   = [A of int | B] >> |>
  fun [ <:stru<type .$x$. >> -> name_length_of_tydcl x
  |> of_name_len ~off:1 |> eprint ] );
  ]}
 *)    
val of_name_len : off:int -> string * int -> ctyp


val list_of_record : name_ctyp -> col list
val gen_tuple_n : ctyp -> int -> ctyp
val repeat_arrow_n : ctyp -> int -> ctyp

(**
     [result] is a keyword
   {[
   let (name,len) =
   ({:stru| type list 'a  'b = [A of int | B of 'a] |}
     |> function {:stru|type $x |} -> name_length_of_tydcl x)
   let f = mk_method_type ~number:2 ~prefix:["fmt"]
   ({:ident| $lid:name |},len);

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
  'fmt -> list 'all_a0 'all_a1 -> list 'all_a0 'all_a1 -> 'result

 *)
val mk_method_type :
  number:int ->
  prefix:string list -> ident * int -> destination -> (ctyp*ctyp)


(**
   
 *)
val mk_method_type_of_name :
  number:int ->
  prefix:string list -> string * int -> destination -> (ctyp*ctyp)
      
(* val mk_dest_type: destination:destination -> ident * int -> ctyp  *)
        
val mk_obj : string -> string -> clfield -> stru
val is_recursive : typedecl -> bool
val is_abstract : typedecl -> bool

val abstract_list : typedecl -> int option
    
val qualified_app_list : ctyp -> (ident * ctyp list) option


(* val eq : ctyp -> ctyp -> bool *)
(* val eq_list : ctyp list -> ctyp list -> bool *)
(* val mk_transform_type_eq : *)
(*   unit -> FanAst.map *)
  
(* val transform_mtyps : mtyps -> *)
(*   (string * ident * int) list * mtyps *)

val reduce_data_ctors:
    or_ctyp ->
      'a -> compose:('e -> 'a  -> 'a) -> (string -> ctyp list -> 'e) -> 'a    
(* @raise Invalid_argument *)        
(* val of_stru: stru -> typedecl *)

val view_sum: or_ctyp -> branch list
val view_variant: row_field -> vbranch list    

(* val ty_name_of_tydcl : typedecl -> ctyp *)    
(* val gen_quantifiers : arity:int -> int -> ctyp *)


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


(* val stru_from_mtyps : *)
(*     f:(named_type -> FAstN.typedecl) -> mtyps -> FAstN.stru option     *)
(* val stru_from_ty : *)
(*     f:(string -> FAstN.stru) -> mtyps -> FAstN.stru *)
