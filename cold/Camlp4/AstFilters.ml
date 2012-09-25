module type S =
 sig
  type 'a filter = ('a -> 'a)

  val register_sig_item_filter : (Ast.sig_item filter -> unit)

  val register_str_item_filter : (Ast.str_item filter -> unit)

  val register_topphrase_filter : (Ast.str_item filter -> unit)

  val fold_interf_filters :
   (('a -> (Ast.sig_item filter -> 'a)) -> ('a -> 'a))

  val fold_implem_filters :
   (('a -> (Ast.str_item filter -> 'a)) -> ('a -> 'a))

  val fold_topphrase_filters :
   (('a -> (Ast.str_item filter -> 'a)) -> ('a -> 'a))

 end

module Make =
       functor (U : sig end) ->
        (struct
          type 'a filter = ('a -> 'a)

          let interf_filters =
           ((Queue.create () ) : Ast.sig_item filter Queue.t)

          let fold_interf_filters =
           fun f -> fun i -> (Queue.fold f i interf_filters)

          let implem_filters =
           ((Queue.create () ) : Ast.str_item filter Queue.t)

          let fold_implem_filters =
           fun f -> fun i -> (Queue.fold f i implem_filters)

          let topphrase_filters =
           ((Queue.create () ) : Ast.str_item filter Queue.t)

          let fold_topphrase_filters =
           fun f -> fun i -> (Queue.fold f i topphrase_filters)

          let register_sig_item_filter =
           fun f -> (Queue.add f interf_filters)

          let register_str_item_filter =
           fun f -> (Queue.add f implem_filters)

          let register_topphrase_filter =
           fun f -> (Queue.add f topphrase_filters)

         end : S)
