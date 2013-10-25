
type anti_cxt = Tokenf.ant = {
    loc : Locf.t;
    cxt : string option;
    kind :  string; (* keep it simple first*)
    txt :string;
    shift : int;
    retract : int;
  }
      


val mk_anti :
    ?c:string -> Tokenf.ant -> [> `Ant of Locf.t * anti_cxt ]

val expand : (Location.t -> string -> 'a) -> anti_cxt -> 'a

(* val add_context : anti_cxt -> string -> anti_cxt *)
    
val pp_print_anti_cxt : Format.formatter -> anti_cxt -> unit
  

