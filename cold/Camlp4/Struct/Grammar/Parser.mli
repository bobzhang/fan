module Make :
 functor (Structure : Structure.S) ->
  sig
   open Structure

   val add_loc :
    (Loc.t -> ((token_stream -> 'b) -> (token_stream -> ('b * Loc.t))))

   val level_number : (internal_entry -> (string -> int))

   val strict_parsing : bool ref

   val strict_parsing_warning : bool ref

   val top_symb : (internal_entry -> (symbol -> symbol))

   val top_tree : (internal_entry -> (tree -> tree))

   val entry_of_symb : (internal_entry -> (symbol -> internal_entry))

   val continue :
    (internal_entry ->
     (Loc.t -> (Action.t -> (symbol -> (tree -> (efun -> efun))))))

   val do_recover :
    ((internal_entry -> ('a -> ('b -> (tree -> efun)))) ->
     (internal_entry ->
      ('a -> ('b -> (Loc.t -> (Action.t -> (symbol -> (tree -> efun))))))))

   val recover :
    ((internal_entry -> ('a -> ('b -> (tree -> efun)))) ->
     (internal_entry ->
      ('a -> ('b -> (Loc.t -> (Action.t -> (symbol -> (tree -> efun))))))))

   val parser_of_tree : (internal_entry -> (int -> (int -> (tree -> efun))))

   val parser_cont :
    (efun ->
     (internal_entry ->
      (int -> (int -> (symbol -> (tree -> (Loc.t -> (Action.t -> efun))))))))

   val parser_of_token_list :
    ((Loc.t -> (Action.t -> efun)) -> (symbol list -> efun))

   val parser_of_symbol : (internal_entry -> (int -> (symbol -> efun)))

   val parse_top_symb : (internal_entry -> (symbol -> efun))

   val start_parser_of_levels :
    (internal_entry -> (int -> (level list -> (int -> efun))))

   val start_parser_of_entry : (internal_entry -> (int -> efun))

   val continue_parser_of_levels :
    (internal_entry ->
     (int -> (level list -> (int -> (Loc.t -> ('a -> efun))))))

   val continue_parser_of_entry :
    (internal_entry -> (int -> (Loc.t -> (Action.t -> efun))))

  end
