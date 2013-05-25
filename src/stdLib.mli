val eq_int : int -> int -> bool
val eq_int32 : int32 -> int32 -> bool
val eq_int64 : int64 -> int64 -> bool
val eq_nativeint : nativeint -> nativeint -> bool
val eq_float : float -> float -> bool
val eq_string : string -> string -> bool
val eq_bool : bool -> bool -> bool
val eq_char : char -> char -> bool
val eq_unit : unit -> unit -> bool
val pp_print_int : Format.formatter -> int -> unit
val pp_print_int32 : Format.formatter -> int32 -> unit
val pp_print_int64 : Format.formatter -> int64 -> unit
val pp_print_nativeint : Format.formatter -> nativeint -> unit
val pp_print_float : Format.formatter -> float -> unit
val pp_print_string : Format.formatter -> string -> unit
val pp_print_bool : Format.formatter -> bool -> unit
val pp_print_char : Format.formatter -> char -> unit
val pp_print_unit : Format.formatter -> unit -> unit
val eq_option : ('a -> 'b -> bool) -> 'a option -> 'b option -> bool
val eq_ref : ('a -> 'b -> 'c) -> 'a ref -> 'b ref -> 'c
val pp_print_option :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
val pp_print_ref :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a ref -> unit
val pp_print_list :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val pp_print_exn : Format.formatter -> exn -> unit
val eq_list : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val eq_array : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
val pp_print_array :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a array -> unit
val eq_arrow : 'a -> 'b -> 'c -> 'd -> bool
val pp_print_arrow : 'a -> 'b -> Format.formatter -> 'c -> unit
class printbase :
  object ('c)
    method array :
      ('c -> Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a array -> unit
    method arrow :
      ('c -> Format.formatter -> 'a -> unit) ->
      ('c -> Format.formatter -> 'b -> unit) ->
      Format.formatter -> ('a -> 'b) -> unit
    method bool : Format.formatter -> bool -> unit
    method char : Format.formatter -> char -> unit
    method float : Format.formatter -> float -> unit
    method int : Format.formatter -> int -> unit
    method int32 : Format.formatter -> int32 -> unit
    method int64 : Format.formatter -> int64 -> unit
    method list :
      ('c -> Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a list -> unit
    method nativeint : Format.formatter -> nativeint -> unit
    method option :
      ('c -> Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a option -> unit
    method ref :
      ('c -> Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a ref -> unit
    method string : Format.formatter -> string -> unit
    method unit : Format.formatter -> unit -> unit
    method unknown : Format.formatter -> 'a -> unit
  end
class mapbase :
  object ('c)
    method array : ('c -> 'a0 -> 'b0) -> 'a0 array -> 'b0 array
    method arrow :
      ('c -> 'a0 -> 'b0) -> ('c -> 'a1 -> 'b1) -> ('a0 -> 'a1) -> 'b0 -> 'b1
    method bool : bool -> bool
    method char : char -> char
    method float : float -> float
    method int : int -> int
    method int32 : int32 -> int32
    method int64 : int64 -> int64
    method list : ('c -> 'a0 -> 'b0) -> 'a0 list -> 'b0 list
    method nativeint : nativeint -> nativeint
    method option : ('c -> 'a -> 'b) -> 'a option -> 'b option
    method ref : ('c -> 'a -> 'b) -> 'a ref -> 'b ref
    method string : string -> string
    method unit : unit -> unit
    method unknown : 'a -> 'a
  end
class iterbase :
  object ('b)
    method array : ('b -> 'a0 -> unit) -> 'a0 array -> unit
    method arrow :
      ('b -> 'a0 -> unit) ->
      ('b -> 'a1 -> unit) -> ('a0 -> 'a1) -> 'b0 -> 'b1
    method bool : bool -> unit
    method char : char -> unit
    method float : float -> unit
    method int : int -> unit
    method int32 : int32 -> unit
    method int64 : int64 -> unit
    method list : ('b -> 'a0 -> unit) -> 'a0 list -> unit
    method nativeint : nativeint -> unit
    method option : ('b -> 'a -> unit) -> 'a option -> unit
    method ref : ('b -> 'a -> unit) -> 'a ref -> unit
    method string : string -> unit
    method unit : unit -> unit
    method unknown : 'a -> unit
  end
class eqbase :
  object ('b)
    method array :
      ('b -> 'a0 -> 'a0 -> bool) -> 'a0 array -> 'a0 array -> bool
    method arrow :
      ('b -> 'a0 -> bool) ->
      ('b -> 'a1 -> bool) -> ('a0 -> 'a1) -> 'b0 -> 'b1
    method bool : bool -> bool -> bool
    method char : char -> char -> bool
    method float : float -> float -> bool
    method int : int -> int -> bool
    method int32 : int32 -> int32 -> bool
    method int64 : int64 -> int64 -> bool
    method list : ('b -> 'a0 -> 'a0 -> bool) -> 'a0 list -> 'a0 list -> bool
    method nativeint : nativeint -> nativeint -> bool
    method option :
      ('b -> 'a -> 'a -> bool) -> 'a option -> 'a option -> bool
    method ref : ('b -> 'a -> 'a -> bool) -> 'a ref -> 'a ref -> bool
    method string : string -> string -> bool
    method unit : unit -> unit -> bool
    method unknown : 'a -> 'a -> bool
  end
class mapbase2 :
  object ('b)
    method array :
      ('b -> 'a0 -> 'a0 -> 'b0) -> 'a0 array -> 'a0 array -> 'b0 array
    method arrow :
      ('b -> 'a0 -> 'a0 -> 'b0) ->
      ('b -> 'a1 -> 'a1 -> 'b1) -> ('a0 -> 'a1) -> ('a0 -> 'a1) -> 'b0 -> 'b1
    method bool : bool -> bool -> bool
    method char : char -> char -> char
    method float : float -> float -> float
    method int : int -> int -> int
    method int32 : int32 -> int32 -> int32
    method int64 : int64 -> int64 -> int64
    method list :
      ('b -> 'a0 -> 'a0 -> 'b0) -> 'a0 list -> 'a0 list -> 'b0 list
    method nativeint : nativeint -> nativeint -> nativeint
    method option :
      ('b -> 'a0 -> 'a0 -> 'b0) -> 'a0 option -> 'a0 option -> 'b0 option
    method ref : ('b -> 'a0 -> 'a0 -> 'b0) -> 'a0 ref -> 'a0 ref -> 'b0 ref
    method string : string -> string -> string
    method unit : unit -> unit -> unit
    method unknown : 'a -> 'a -> 'a
  end
class monadbase : mapbase
class monadbase2 : mapbase2
class foldbase :
  object ('b)
    method array : ('b -> 'a0 -> 'b) -> 'a0 array -> 'b
    method arrow :
      ('b -> 'a0 -> 'b) -> ('b -> 'a1 -> 'b) -> ('a0 -> 'a1) -> 'b
    method bool : bool -> 'b
    method char : char -> 'b
    method float : float -> 'b
    method int : int -> 'b
    method int32 : int32 -> 'b
    method int64 : int64 -> 'b
    method list : ('b -> 'a0 -> 'b) -> 'a0 list -> 'b
    method nativeint : nativeint -> 'b
    method option : ('b -> 'a0 -> 'b) -> 'a0 option -> 'b
    method ref : ('b -> 'a0 -> 'b) -> 'a0 ref -> 'b
    method string : string -> 'b
    method unit : unit -> 'b
    method unknown : 'a -> 'b
  end
class foldbase2 :
  object ('b)
    method array : ('b -> 'a0 -> 'a0 -> 'b) -> 'a0 array -> 'a0 array -> 'b
    method arrow :
      ('b -> 'a0 -> 'a0 -> 'b) ->
      ('b -> 'a1 -> 'a1 -> 'b) -> ('a0 -> 'a1) -> ('a0 -> 'a1) -> 'b
    method bool : bool -> bool -> 'b
    method char : char -> char -> 'b
    method float : float -> float -> 'b
    method int : int -> int -> 'b
    method int32 : int32 -> int32 -> 'b
    method int64 : int64 -> int64 -> 'b
    method list : ('b -> 'a0 -> 'a0 -> 'b) -> 'a0 list -> 'a0 list -> 'b
    method nativeint : nativeint -> nativeint -> 'b
    method option :
      ('b -> 'a0 -> 'a0 -> 'b) -> 'a0 option -> 'a0 option -> 'b
    method ref : ('b -> 'a0 -> 'a0 -> 'b) -> 'a0 ref -> 'a0 ref -> 'b
    method string : string -> string -> 'b
    method unit : unit -> unit -> 'b
    method unknown : 'a -> 'a -> 'b
  end



(* class primitive : *)
(*   object *)
(*     method ant : FAst.loc -> FAst.ant -> FAst.ep *)
(*     method bool : FAst.loc -> bool -> FAst.ep *)
(*     method char : FAst.loc -> char -> FAst.ep *)
(*     method float : FAst.loc -> float -> FAst.ep *)
(*     method int : FAst.loc -> int -> FAst.ep *)
(*     method int32 : FAst.loc -> int32 -> FAst.ep *)
(*     method int64 : FAst.loc -> int64 -> FAst.ep *)
(*     method loc : FAst.loc -> FAst.loc -> FAst.ep *)
(*     method nativeint : FAst.loc -> nativeint -> FAst.ep *)
(*     method string : FAst.loc -> string -> FAst.ep *)
(*     method unit : FAst.loc -> unit -> FAst.ep *)
(*   end *)
    
