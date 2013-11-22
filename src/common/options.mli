
type spec_list = (string * Arg.spec * string) list 

val init : spec_list -> unit

val add : (string * Arg.spec * string) -> unit

val adds : (string * Arg.spec * string) list  -> unit

val init_spec_list : spec_list ref 

