module type S = sig
  type filter 'a = 'a -> 'a;
  value register_sig_item_filter : (filter Ast.sig_item) -> unit;
  value register_str_item_filter : (filter Ast.str_item) -> unit;
  value register_topphrase_filter : (filter Ast.str_item) -> unit;
  value fold_interf_filters : ('a -> filter Ast.sig_item -> 'a) -> 'a -> 'a;
  value fold_implem_filters : ('a -> filter Ast.str_item -> 'a) -> 'a -> 'a;
  value fold_topphrase_filters : ('a -> filter Ast.str_item -> 'a) -> 'a -> 'a;
end;
    
module Make (U:sig end) : S = struct
  type filter 'a = 'a -> 'a;
  value interf_filters: Queue.t (filter Ast.sig_item) = Queue.create ();
  value fold_interf_filters f i = Queue.fold f i interf_filters;
  value implem_filters: Queue.t (filter Ast.str_item) = Queue.create ();
  value fold_implem_filters f i = Queue.fold f i implem_filters;
  value topphrase_filters: Queue.t (filter Ast.str_item) = Queue.create ();
  value fold_topphrase_filters f i = Queue.fold f i topphrase_filters;

  value register_sig_item_filter f = Queue.add f interf_filters;
  value register_str_item_filter f = Queue.add f implem_filters;
  value register_topphrase_filter f = Queue.add f topphrase_filters;
end;
