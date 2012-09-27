
  (******************************)
  (* Intialize a filter plugin  *)
  (******************************)
module type S = sig
  type filter 'a = 'a -> 'a;
  val register_sig_item_filter : (filter Ast.sig_item) -> unit;
  val register_str_item_filter : (filter Ast.str_item) -> unit;
  val register_topphrase_filter : (filter Ast.str_item) -> unit;
  val fold_interf_filters : ('a -> filter Ast.sig_item -> 'a) -> 'a -> 'a;
  val fold_implem_filters : ('a -> filter Ast.str_item -> 'a) -> 'a -> 'a;
  val fold_topphrase_filters : ('a -> filter Ast.str_item -> 'a) -> 'a -> 'a;
end;
    
module Make (U:sig end) : S = struct
  type filter 'a = 'a -> 'a;
  let interf_filters: Queue.t (filter Ast.sig_item) = Queue.create ();
  let fold_interf_filters f i = Queue.fold f i interf_filters;
  let implem_filters: Queue.t (filter Ast.str_item) = Queue.create ();
  let fold_implem_filters f i = Queue.fold f i implem_filters;
  let topphrase_filters: Queue.t (filter Ast.str_item) = Queue.create ();
  let fold_topphrase_filters f i = Queue.fold f i topphrase_filters;

  let register_sig_item_filter f = Queue.add f interf_filters;
  let register_str_item_filter f = Queue.add f implem_filters;
  let register_topphrase_filter f = Queue.add f topphrase_filters;
end;
