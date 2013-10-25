
type anti_cxt = {
    cxt:string;
    mutable kind :  string; (* keep it simple first*)
    txt :string;
  }
      
val mk_anti:
    ?c:string -> Tokenf.ant -> [> `Ant of Locf.t * anti_cxt ]


val add_context : anti_cxt -> string -> anti_cxt
    
val pp_print_anti_cxt : Format.formatter -> anti_cxt -> unit
  

