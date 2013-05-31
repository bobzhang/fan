
type spec_list = (string * FArg.spec * string) list 

val init : spec_list -> unit

val add : (string * FArg.spec * string) -> unit

val adds : (string * FArg.spec * string) list  -> unit

val init_spec_list : spec_list ref 

