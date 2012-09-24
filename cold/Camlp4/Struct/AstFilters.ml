module Make =
 functor (Ast : (Sig.Camlp4Ast with module Loc = FanLoc)) ->
  (struct
    module Ast = Ast

    type 'a filter = ('a -> 'a)

    let interf_filters = (Queue.create () )

    let fold_interf_filters =
     fun f -> fun i -> (Queue.fold f i interf_filters)

    let implem_filters = (Queue.create () )

    let fold_implem_filters =
     fun f -> fun i -> (Queue.fold f i implem_filters)

    let topphrase_filters = (Queue.create () )

    let fold_topphrase_filters =
     fun f -> fun i -> (Queue.fold f i topphrase_filters)

    let register_sig_item_filter = fun f -> (Queue.add f interf_filters)

    let register_str_item_filter = fun f -> (Queue.add f implem_filters)

    let register_topphrase_filter = fun f -> (Queue.add f topphrase_filters)

   end : (Sig.AstFilters with module Ast = Ast))
