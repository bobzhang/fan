
(** An environment mapping variables to identifiers, and capable of generating
	fresh identifiers. *)

(** The type of environments. *)
type t

(** [empty f] returns an empty environment with naming function [f]. *)
val empty : (int -> string) -> t

(** [lookup env var] returns the identifier for [var] in [env], or raises
	[Not_found] if unbound. *)
val lookup : t -> string -> string

(** [bind env var id] returns a new environment where [var] is bound to [id].
	*)
val bind : t -> string -> string -> t

(** [bound env var] checks whether [var] is bound in [env]. *)
val bound : t -> string -> bool

(** [fresh_id env] allocates a fresh identifier in [env]. *)
val fresh_id : t -> t * string

(** [dispatch env v f1 f2] will return [f1 id] when [v] is bound to [id] in
	[env]; it will return [f2 id env] otherwise, where [id] is fresh and [env]
	is extended with the binding of [v] to [id]. *)
val dispatch : t -> string -> (string -> 'a) -> (t -> string -> 'a) -> 'a

(** [bind_or_test env tst var id] binds [var] to [id] in enviroment, or extends
	[tst] with the pair [id,id'] if [var] is already bound to [id']. *)
val bind_or_test : t -> (string * string) list -> string -> string -> t * (string * string) list

(** [gen_bind_or_test env tst var] returns a fresh identifier, and binds [var]
	to this identifier in [env] or extends [tst] with the pair [id,id'] when
	[var] is already bound to [id']. *)
val gen_bind_or_test : t -> (string * string) list -> string -> t * (string * string) list * string

(** [unify env tst var1 var2] unifies [var1] and [var2]: if one of both is
	bound, the other is bound as well; if both are bound, [tst] is extended
	with a pair of their identifiers; if both are unbound, [Not_found] is
	raised. *)
val unify : t -> (string * string) list -> string -> string -> t * (string * string) list
