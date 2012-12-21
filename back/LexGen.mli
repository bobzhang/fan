
open Ast
  
(* val tables : (int array, string) Hashtbl.t *)

(* val tables_counter : int ref *)
val get_tables: tables:('a, 'b) Hashtbl.t -> unit -> ('b * 'a) list
(* val get_tables : unit -> (string * int array) list *)


val table_name:
    tables:(int array,int) Hashtbl.t -> counter:int ref -> int array -> string
    
val output_byte : Buffer.t -> int -> unit
val output_byte_array : int array -> expr
val table : string * int array -> str_item
val partition_name : int -> string

val partition:
    counter:int ref ->
      tables:(int array, int) Hashtbl.t ->
        int * (int * int * int) list -> str_item


val best_final : bool array -> int option

(* val call_state : ('a * 'b array * bool array) array -> int -> expr *)
(* val gen_state : *)
(*   ('a * 'b array * bool array) array -> *)
(*   loc -> int -> int * int array * bool array -> binding *)
val gen_definition :
  loc -> (Ulex.regexp * expr) list -> expr


