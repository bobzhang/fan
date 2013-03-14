(* open FanUtil; *)
open LibUtil;
class c_fold_pattern_vars ['accu] f init =  object
  inherit FanAst.fold as super;
  val acc = init;
  method acc : 'accu = acc;
  method! patt = fun
  [ {:patt| $lid:s |} | {:patt| ~ $s |} | {:patt| ? $s |}
    -> {< acc = f s acc >}
  | p -> super#patt p ];
end;

let fold_pattern_vars f p init = ((new c_fold_pattern_vars f init)#patt p)#acc;

let rec fold_binding_vars f bi acc = match bi with
  [ {:binding| $bi1 and $bi2 |} ->
    fold_binding_vars f bi1 (fold_binding_vars f bi2 acc)
  | {:binding| $p = $_ |} -> fold_pattern_vars f p acc
  | {:binding||} -> acc
  | {:binding| $anti:_ |} -> assert false ];

class fold_free_vars ['accu] (f : string -> 'accu -> 'accu) ?(env_init = SSet.empty) free_init =  object (o)
  inherit FanAst.fold as super;
  val free : 'accu = free_init;
  val env : SSet.t = env_init;
    
  method free = free;
  method set_env env = {< env = env >};
  method add_atom s = {< env = SSet.add s env >};
  method add_patt p = {< env = fold_pattern_vars SSet.add p env >};
  method add_binding bi = {< env = fold_binding_vars SSet.add bi env >};

  method! expr = fun
  [ {:expr| $lid:s |} | {:expr| ~ $s |} | {:expr| ? $s |} ->
    if SSet.mem s env then o else {< free = f s free >}
      
  | {:expr| let $bi in $e |} ->
      (((o#add_binding bi)#expr e)#set_env env)#binding bi
        
  | {:expr| let rec $bi in $e |} ->
      (((o#add_binding bi)#expr e)#binding bi)#set_env env
        
  | {:expr| for $s = $e1 $to:_ $e2 do  $e3 done |} ->
      ((((o#expr e1)#expr e2)#add_atom s)#expr e3)#set_env env
        
  | {:expr| $id:_ |} | {:expr| new $_ |} -> o
        
  | {:expr| object ($p) $cst end |} ->
      ((o#add_patt p)#class_str_item cst)#set_env env
        
  | e -> super#expr e ];

  method! case = fun
  [ {:case| $pat:p when $e1 -> $e2 |} ->
    (((o#add_patt p)#expr e1)#expr e2)#set_env env
  | m -> super#case m ];

  method! str_item = fun
  [ {:str_item| external $s : $t = $_ |} ->
    (o#ctyp t)#add_atom s
  | {:str_item| let $bi |} ->
      (o#binding bi)#add_binding bi
  | {:str_item| let rec $bi |} ->
      (o#add_binding bi)#binding bi
  | st -> super#str_item st ];

  method! class_expr = fun
  [ {:class_expr| fun $p -> $ce |} ->
    ((o#add_patt p)#class_expr ce)#set_env env
  | {:class_expr| let $bi in $ce |} ->
      (((o#binding bi)#add_binding bi)#class_expr ce)#set_env env
  | {:class_expr| let rec $bi in $ce |} ->
      (((o#add_binding bi)#binding bi)#class_expr ce)#set_env env
  | {:class_expr| object ($p) $cst end |} ->
      ((o#add_patt p)#class_str_item cst)#set_env env
  | ce -> super#class_expr ce ];

  method! class_str_item = fun
  [ {:class_str_item| inherit $override:_ $_ |} as cst -> super#class_str_item cst
  | {:class_str_item| inherit $override:_ $ce as $s |} ->
      (o#class_expr ce)#add_atom s
  | {:class_str_item| val $override:_ $mutable:_ $s = $e |} ->
      (o#expr e)#add_atom s
  | {:class_str_item| val virtual $mutable:_ $s : $t |} ->
      (o#ctyp t)#add_atom s
  | cst -> super#class_str_item cst ];

  method! module_expr = fun
  [ {:module_expr| struct $st end |} ->
    (o#str_item st)#set_env env
  | me -> super#module_expr me ];
end;

let free_vars env_init e =
  let fold = new fold_free_vars SSet.add ~env_init SSet.empty in (fold#expr e)#free;

