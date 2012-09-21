module Make :
 functor (Structure : Structure.S) ->
  sig
   val flatten_tree : (Structure.tree -> Structure.symbol list list)

   val print_symbol : (Format.formatter -> (Structure.symbol -> unit))

   val print_meta :
    (Format.formatter -> (string -> (Structure.symbol list -> unit)))

   val print_symbol1 : (Format.formatter -> (Structure.symbol -> unit))

   val print_rule : (Format.formatter -> (Structure.symbol list -> unit))

   val print_level :
    (Format.formatter ->
     ((Format.formatter -> (unit -> unit)) ->
      (Structure.symbol list list -> unit)))

   val levels : (Format.formatter -> (Structure.level list -> unit))

   val entry : (Format.formatter -> (Structure.internal_entry -> unit))

  end

module MakeDump :
        functor (Structure : Structure.S) ->
         sig
          val print_symbol : (Format.formatter -> (Structure.symbol -> unit))

          val print_meta :
           (Format.formatter -> (string -> (Structure.symbol list -> unit)))

          val print_symbol1 :
           (Format.formatter -> (Structure.symbol -> unit))

          val print_rule :
           (Format.formatter -> (Structure.symbol list -> unit))

          val print_level :
           (Format.formatter ->
            ((Format.formatter -> (unit -> unit)) ->
             (Structure.symbol list list -> unit)))

          val levels : (Format.formatter -> (Structure.level list -> unit))

          val entry :
           (Format.formatter -> (Structure.internal_entry -> unit))

         end
