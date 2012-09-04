module Make =
 functor (Ast : Sig.Ast) ->
  struct
   let print_interf =
    fun ?input_file:_ ->
     fun ?output_file:_ -> fun _ -> (failwith "No interface printer")

   let print_implem =
    fun ?input_file:_ ->
     fun ?output_file:_ -> fun _ -> (failwith "No implementation printer")

  end
