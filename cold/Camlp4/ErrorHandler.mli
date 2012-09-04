val print : (Format.formatter -> (exn -> unit))

val try_print :
                                                  (Format.formatter ->
                                                   (exn -> unit))

val to_string :
                                                                    (exn ->
                                                                    string)


val try_to_string : (exn -> string)

val register :
                                      ((Format.formatter -> (exn -> unit)) ->
                                       unit)

module Register :
                                               functor (Error : Sig.Error) ->
                                                sig end

module ObjTools :
                                                          sig
                                                           val print :
                                                            (Format.formatter
                                                             ->
                                                             (Obj.t -> unit))

                                                           val print_desc :
                                                            (Format.formatter
                                                             ->
                                                             (Obj.t -> unit))

                                                           val to_string :
                                                            (Obj.t -> string)

                                                           val desc :
                                                            (Obj.t -> string)

                                                          end
