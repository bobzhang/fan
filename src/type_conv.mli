



val add_generator : ?is_exn : bool -> string -> (bool -> ctyp -> str_item) -> unit
(** [add_generator ?is_exn name gen] adds the code generator [gen],
    which maps type or exception declarations to structure items, where
    [is_exn] specifies whether the declaration is an exception.  Note that
    the original type/exception declarations get added automatically in
    any case.

    @param is_exn = [false]
*)
  

val add_generator_with_arg :
  ?is_exn : bool -> string -> 'a Camlp4.PreCast.Gram.Entry.t ->
  ('a option -> bool -> ctyp -> str_item) -> unit
(** [add_generator_with_arg ?is_exn name entry generator] same as
    [add_generator], but the generator may accept an argument, which is
    parsed with [entry]. *)


val rm_generator : ?is_exn : bool -> string -> unit
(** [rm_generator ?is_exn name] removes the code generator named [name]
    for types if [is_exn] is [false], or exceptions otherwise.

    @param is_exn = [false]
*)

    
val add_sig_generator :
  ?delayed : bool -> ?is_exn : bool ->
  string -> (bool -> ctyp -> sig_item) -> unit
(** [add_sig_generator ?delayed ?is_exn name gen] adds the code generator [gen],
    which maps type or exception declarations to signature items, where
    [is_exn] specifies whether the declaration is an exception.  Note that the
    original type/exception declarations get added automatically in any case. If
    [delayed] is set to true, the output of this generator is appended to the
    signature in which it's defined

    @param delayed = [false]
    @param is_exn = [false]
*)


val add_sig_generator_with_arg :
  ?delayed : bool -> ?is_exn : bool -> string ->
  'a Camlp4.PreCast.Gram.Entry.t ->
  ('a option -> bool -> ctyp -> sig_item) -> unit
(** [add_sig_generator_with_arg ?delayed ?is_exn name entry generator] same as
    [add_sig_generator], but the generator may accept an argument,
    which is parsed with [entry]. *)

val rm_sig_generator : ?is_exn : bool -> string -> unit
(** [rm_sig_generator ?is_exn name] removes the signature code generator named
    [name] for types if [is_exn] is [false], or exceptions otherwise.

    @param is_exn = [false]
*)

(** Type of record field code generators *)
type record_field_generator = ctyp -> unit

val add_record_field_generator : string -> record_field_generator -> unit
(** [add_record_field_generator gen_name gen] adds the record field code
    generator [gen] with name [gen_name], which acts on the location
    identifiying the record field. *)

val add_record_field_generator_with_arg :
  string -> 'a Camlp4.PreCast.Gram.Entry.t ->
  ('a option -> record_field_generator) -> unit
(** [add_record_field_generator_with_arg name entry generator] same as
    [add_record_field_generator], but the [generator] takes an argument,
    which is parsed with [entry].  If [None] is passed to the generator,
    parsing of the argument failed, otherwise [Some arg] will be passed,
    where [arg] is the successfully parsed argument. *)

val rm_record_field_generator : string -> unit
(** [rm_record_field_generator name] removes the record field code generator
    named [name]. *)

(** {6 Generator sets registration} *)

val add_sig_set : ?is_exn: bool -> string -> set: string list -> unit
(** [add_sig_set ?is_exn id ~set] adds the generator [id] to the list
    of generators for signatures.
    This generator will behave as if is all the generators from [set]
    had been given instead. Any duplicate arising from repeatedly
    expanding such generators are removed.
    If [is_exn], then it is a generator for exception declaration, or
    else it is a generator for type declaration.
*)

val add_str_set : ?is_exn: bool -> string -> set: string list -> unit
(** [add_str_set ?is_exn id ~set] behaves exactly like
    [add_sig_set ?is_exn id ~set] but for structure items instead of
    signatures items.
*)

val add_set :
  kind:[`Str | `Sig | `Both] ->
  is_exn:[`Yes | `No | `Both] ->
  string ->
  set:string list ->
  unit
(** [add_set ~kind ~is_exn id ~set] is a shorthand for doing multiple
    calls to [add_str_set] and [add_sig_set]
*)


(** {6 Utility functions to rewrite type definitions} *)

module Rewrite_tds : sig
  val sig_ : Loc.t -> bool -> ctyp -> sig_item
  (** [sig_ loc rec_ typedefs] rewrites the given type definition to make it either
      recursive or non recursive.
      For instance, the parser calls [sig_ loc false (TyDcl (_, t, [], t, []))] when it
      encouters [type t = t] and calls [sig_ loc true (TyDcl (_, t, [], t, []))] when it
      encouters [type nonrec t = t] in signatures. *)

  val str_ : Loc.t -> bool -> ctyp -> str_item
  (** [str_ loc rec_ typedefs] does the same thing as [sig_ loc rec_ typedefs], except
      that it returns a structure item instead of a signature item. *)
end
      
