
open LibUtil;

type key = string;
type filter 'a = 'a -> 'a;
  
let interf_filters: Hashtbl.t key (filter Ast.sig_item) = Hashtbl.create 40;
let implem_filters: Hashtbl.t key (filter Ast.str_item) = Hashtbl.create 40;
let topphrase_filters: Hashtbl.t key (filter Ast.str_item) = Hashtbl.create 40;

let applied_interf_filters:  Queue.t (string * filter Ast.sig_item) = Queue.create ();
let applied_implem_filters:  Queue.t (string *filter Ast.str_item) = Queue.create ();
let applied_topphrase_filters:  Queue.t (string * filter Ast.str_item) = Queue.create ();
  
let apply_interf_filters  i = Queue.fold (fun ast (_name,f) -> f ast) i applied_interf_filters;
let apply_implem_filters  i = Queue.fold (fun ast (_name,f) -> f ast) i applied_implem_filters;
let apply_topphrase_filters  i = Queue.fold (fun ast (_name,f) -> f ast) i applied_topphrase_filters;

let use_interf_filter s =
  let u =
    try Hashtbl.find interf_filters s
    with Not_found -> failwithf "filter %s is not registered" s in
  Queue.add (s,u) applied_interf_filters ;
  
let use_implem_filter s =
  let u =
    try Hashtbl.find implem_filters s
    with Not_found -> failwithf "filter %s is not registered" s in
  Queue.add (s,u) applied_implem_filters ;
let use_topphrase_filter s =
  let u =
    try Hashtbl.find topphrase_filters s
    with Not_found -> failwithf "filter %s is not registered" s in
  Queue.add (s,u) applied_topphrase_filters;

let register_sig_item_filter (k,f) =
  Hashtbl.replace interf_filters k f ;
let register_str_item_filter (k,f) =
  Hashtbl.replace implem_filters k f ;
let register_topphrase_filter (k,f) =
  Hashtbl.replace topphrase_filters k f;

