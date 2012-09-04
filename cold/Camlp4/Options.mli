type spec_list = (string * Arg.spec * string) list

val init :
                                                     (spec_list -> unit)


val add : (string -> (Arg.spec -> (string -> unit)))

val print_usage_list :
                                                       (spec_list -> unit)


val ext_spec_list : (unit -> spec_list)

val parse :
                                          ((string -> unit) ->
                                           (string array -> string list))
