

(* open FanUtil; *)

open LibUtil;;

(* syntax error class declaration TODO FIXME *)
class c_fold_pattern_vars ['accu] f init =  object
  inherit FanAst.fold as super;
  val acc = init;
  method acc : 'accu = acc;
  method! pat = fun
  [ {:pat| $lid:s |} | {:pat| ~ $s |} | {:pat| ? $s |}
    -> {< acc = f s acc >}
  | p -> super#pat p ];
end;

let fold_pattern_vars f p init = ((new c_fold_pattern_vars f init)#pat p)#acc;

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
  method add_pat p = {< env = fold_pattern_vars SSet.add p env >};
  method add_binding bi = {< env = fold_binding_vars SSet.add bi env >};

  method! exp = fun
  [ {:exp| $lid:s |} | {:exp| ~ $s |} | {:exp| ? $s |} ->
    if SSet.mem s env then o else {< free = f s free >}
      
  | {:exp| let $bi in $e |} ->
      (((o#add_binding bi)#exp e)#set_env env)#binding bi
        
  | {:exp| let rec $bi in $e |} ->
      (((o#add_binding bi)#exp e)#binding bi)#set_env env
        
  | {:exp| for $s = $e1 $to:_ $e2 do  $e3 done |} ->
      ((((o#exp e1)#exp e2)#add_atom s)#exp e3)#set_env env
        
  | {:exp| $id:_ |} | {:exp| new $_ |} -> o
        
  | {:exp| object ($p) $cst end |} ->
      ((o#add_pat p)#cstru cst)#set_env env
        
  | e -> super#exp e ];

  method! case = fun
  [ {:case| $pat:p when $e1 -> $e2 |} ->
    (((o#add_pat p)#exp e1)#exp e2)#set_env env
  | m -> super#case m ];

  method! stru = fun
  [ {:stru| external $s : $t = $_ |} ->
    (o#ctyp t)#add_atom s
  | {:stru| let $bi |} ->
      (o#binding bi)#add_binding bi
  | {:stru| let rec $bi |} ->
      (o#add_binding bi)#binding bi
  | st -> super#stru st ];

  method! clexp = fun
  [ {:clexp| fun $p -> $ce |} ->
    ((o#add_pat p)#clexp ce)#set_env env
  | {:clexp| let $bi in $ce |} ->
      (((o#binding bi)#add_binding bi)#clexp ce)#set_env env
  | {:clexp| let rec $bi in $ce |} ->
      (((o#add_binding bi)#binding bi)#clexp ce)#set_env env
  | {:clexp| object ($p) $cst end |} ->
      ((o#add_pat p)#cstru cst)#set_env env
  | ce -> super#clexp ce ];

  method! cstru = fun
  [ {:cstru| inherit $override:_ $_ |} as cst -> super#cstru cst
  | {:cstru| inherit $override:_ $ce as $s |} ->
      (o#clexp ce)#add_atom s
  | {:cstru| val $override:_ $mutable:_ $s = $e |} ->
      (o#exp e)#add_atom s
  | {:cstru| val virtual $mutable:_ $s : $t |} ->
      (o#ctyp t)#add_atom s
  | cst -> super#cstru cst ];

  method! mexp = fun
  [ {:mexp| struct $st end |} ->
    (o#stru st)#set_env env
  | me -> super#mexp me ];
end;

let free_vars env_init e =
  let fold = new fold_free_vars SSet.add ~env_init SSet.empty in (fold#exp e)#free;

