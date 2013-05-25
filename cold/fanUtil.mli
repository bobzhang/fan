
type anti_cxt = {
    cxt:string;
    sep:  string option;
    mutable decorations:  string; (* keep it simple first*)
    content:string;
  }
      
val mk_anti:
    ?c:string ->
      ?sep:string ->
        'a ->
          string ->
            string ->
              [> `Ant of 'a * anti_cxt ]


val add_context : anti_cxt -> string -> anti_cxt
    
val pp_print_anti_cxt : Format.formatter -> anti_cxt -> unit
  
