open Ast
open FSig


(**
   Some common arguments
   [mk_tuple: ty_info list -> exp]
   [mk_variant: string -> ty_info list -> exp]
   [mk_record: record_col list -> exp ]

   [names:string list]
     A prefix argument list, for example if you want to write
     Format library, you may need to pass ["fmt"]
   [arity:int]

   [f:ctyp->exp]

   [left_type_id:basic_id_transform]
     for objects, the default case is [`Pre ""], which means the
     the type is self type which appears in the context
   
   [right_type_id:full_id_transform]

   [left_type_variable:basic_id_transform]

 *)  


(* we preserve some keywords to avoid variable capture *)
val check : string list -> unit





(** collect the [partial evaluated Ast node] and meta data
   The input [y] is handled by [simple_exp_of_ctyp], generally it will
   be  exlcuding adt or variant type

    {[
    Frame.mapi_exp ~arity:2 ~names:["fmt";"test"]
    ~f:(fun {:ctyp|$lid:x|} -> {:exp|$(lid:("meta_"^x))|})   3  {:ctyp|int |};;
    ]}

    {name_exp = `Lid (, "meta_int");
    info_exp =
    `App
    (,
     `App
       (,
        `App
          (, `App (, `Lid (, "meta_int"), `Lid (, "fmt")), `Lid (, "test")),
        `Lid (, "_a3")),
     `Lid (, "_b3"));
    ep0 = `Lid (, "_a3");
    id_ep = `Par (, `Com (, `Lid (, "_a3"), `Lid (, "_b3")));
    id_eps = [`Lid (, "_a3"); `Lid (, "_b3")]; ty = `Lid (, "int")}
 *)      
val mapi_exp :
  ?arity:int ->
  ?names:string list ->
  f:(ctyp -> exp) -> int -> ctyp -> ty_info

(** @raise Invalid_argument when type can not be handled
    [mk_tuple]
    spits out an expression node when the input is a tuple type *)        
val tuple_exp_of_ctyp :
    ?arity:int ->
      ?names:string list ->
        mk_tuple:(ty_info list -> exp) ->
          f:(ctyp -> exp ) -> ctyp -> exp


(**
   @supported types type application: list int
   basic type: int
   product type: (int * float * int)
   [m_list
   (fun _loc fmt ((a0, a1, a2), (b0, b1, b2)) ->
   ((m_int _loc fmt (a0, b0)), (m_float _loc fmt (a0, b0)),
   (m_float _loc fmt (a0, b0))))]
   return type is result
   Plz supply current type [type 'a list] =>  [list] *)    
val normal_simple_exp_of_ctyp :
  ?arity:int ->
  ?names:string list ->
  mk_tuple:(ty_info list -> exp) ->
  right_type_id:full_id_transform ->
  left_type_id:basic_id_transform ->
  right_type_variable:rhs_basic_id_transform ->
  (string, 'a) Hashtbl.t -> ctyp -> exp


(**
  list int ==>  self#list (fun self -> self#int)
  'a list  ==>  self#list mf_a 
  'a  ==> (mf_a self)
  list ('a list) ==>  self#list (fun self -> (self#list mf_a))
  m_list (tree 'a) ==>  self#m_list (fun self -> self#tree mf_a) *)         
val obj_simple_exp_of_ctyp :
  right_type_id:full_id_transform ->
  left_type_variable:basic_id_transform ->
  right_type_variable:rhs_basic_id_transform ->
  ?names:string list ->
  ?arity:int ->
  mk_tuple:(ty_info list -> exp) ->
  ctyp -> exp



(**
  accept [simple_exp_of_ctyp]
  call [reduce_data_ctors]  for [sum types]
  assume input is  [sum type]
  accept input type to generate  a function expession *)  
val exp_of_ctyp :
  ?cons_transform:(string -> string) ->
  ?arity:int ->
  ?names:string list ->
  default:(vrn * int -> case option) ->
  mk_variant:(string -> ty_info list -> exp) ->
  (ctyp -> exp) -> or_ctyp -> exp

      

      
(** return a [exp] node accept [variant types] *)  
val exp_of_variant:
    ?cons_transform:(string -> string) ->
      ?arity:int ->
        ?names:string list ->
          default:(vrn * int -> case option) ->
            mk_variant:(string -> ty_info list -> exp) ->
              destination:destination -> (ctyp -> exp) -> ctyp -> row_field ->  exp
                  






(* add extra arguments to the generated expession node *)  
val mk_prefix:
  opt_decl_params ->
  exp ->
  ?names:string list ->
  left_type_variable:basic_id_transform ->
  exp

      
val fun_of_tydcl :
    ?names:string list ->
    ?arity:int ->
    left_type_variable:basic_id_transform ->
    mk_record:(record_col list -> exp) ->
    destination:destination ->
      result_type:ctyp -> 
      (ctyp -> exp ) ->
        (or_ctyp -> exp ) ->
          (ctyp -> row_field -> exp) ->  (* labeld as variant *)
            typedecl -> exp

                
val bind_of_tydcl :
  ?cons_transform:(string -> string) ->
  (ctyp -> exp ) ->
  typedecl ->
  ?arity:int ->
  ?names:string list ->
  default:(vrn * int -> case option) ->
  mk_variant:(string -> ty_info list -> exp) ->
  left_type_id:basic_id_transform ->
  left_type_variable:basic_id_transform ->
  mk_record:(record_col list -> exp) ->
  (* destination:destination -> *)
    bind
      
val stru_of_mtyps :
  ?module_name:string ->
  ?cons_transform:(string -> string) ->
  ?arity:int ->
  ?names:string list ->
  default:(vrn * int -> case option) ->
  mk_variant:(string -> ty_info list -> exp) ->
  left_type_id:basic_id_transform ->
  left_type_variable:basic_id_transform ->
  mk_record:(record_col list -> exp) ->
  (* destination:destination ->  *)
  ((string, unit) Hashtbl.t -> ctyp -> exp ) ->
  mtyps -> stru
      
val obj_of_mtyps :
  ?cons_transform:(string -> string) ->
    ?module_name:string ->
  ?arity:int ->
  ?names:string list ->
  default:(vrn * int -> case option) ->
  left_type_variable:basic_id_transform ->
  mk_record:(record_col list -> exp) ->
  mk_variant:(string -> ty_info list -> exp) ->
  (* destination:destination  -> *)
     string ->  string ->  (ctyp -> exp ) -> 
  kind -> mtyps -> stru
  

val gen_stru :
  ?module_name:string ->
  ?arity:int ->
  ?default:exp ->
  ?cons_transform:(string -> string) ->
  id:basic_id_transform ->
  ?names:string list ->
  mk_tuple:(ty_info list -> exp) ->
  mk_record:(record_col list -> exp) ->
  mk_variant:(string -> ty_info list -> exp) -> unit -> 
  mtyps -> stru
val gen_object :
  ?module_name:string ->
  ?arity:int ->
  ?default:exp ->
  ?cons_transform:(string -> string) ->
  kind:kind ->
  base:string ->
  class_name:string ->
  ?names:string list ->
  mk_tuple:(ty_info list -> exp) ->
  mk_record:(record_col list -> exp) ->
  mk_variant:(string -> ty_info list -> exp) -> unit -> 
  mtyps -> stru
      
