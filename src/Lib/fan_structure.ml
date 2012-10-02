value app a b = << .$a$. .$b$. >>
;

  
value comma a b = << .$a$., .$b$. >>
;  

(*
   Left associativity
 *)  
value ( <$ ) = app
;


(*
  left associative 
   {[
   apply << f >> [ << a >> ; << b >> ] |> eprint;
   f a b
   ]}
 *)      
value rec apply acc = fun
  [ [] -> acc
  | [x::xs] ->  apply (app acc x) xs]
;

value sem a b = << .$a$.; .$b$.  >> ;

  
(*
   @raise Invalid_argument 

  decompose a left associative  application to  an identifier  and
   a list of small ast nodes.

  Unlike com [a,(b,c),d], the Ast node will introduce [ExTup],
  for [f (g a) b], there is no intermediate node.
  
  {[
  list_of_app << (f.g#u) a b >> |> app_of_list |> eprint;

  f.g#u a b

  list_of_app << f (g a) b >>  |> app_of_list |> eprint;
  f (g a) b
  ]}
 *)          

value  list_of_app  ty  =
  let rec  loop t acc =  match t with  
    [ << .$t1$. .$t2$. >> -> loop t1 [t2 ::acc]
    | << >> -> acc (* remove nil case *)
    | i ->  [i::acc] ] in 
  loop ty []
;    

  
(* right associative comma
   {[

   list_of_com << 1,2,3>> |> com_of_list |> eprint;
   1, 2, 3

   list_of_com << 1,(2,3),4>> |> com_of_list |> eprint;
   1, (2, 3), 4
   ]}
 *)  
value list_of_com ty = 
  let rec loop t acc = match t with
    [ << .$t1$. , .$t2$. >> -> (* loop t2 [t1::acc]*)
       [t1 :: (loop t2 acc) ]  
    | << >> -> acc 
    | i -> [i::acc] ] in
  loop ty [];

(*
  right associative sem
  [ << 1;(2;3);4 >> ] will introduce an intermedate [ExSeq] here.
  {[
   list_of_com << 1;(2;3);4>> |> com_of_list |> eprint;
  1; (2; 3); 4
  ]}
 *)
value list_of_sem ty = 
  let rec loop t acc = match t with
    [ << .$t1$. ; .$t2$. >> ->  [t1 :: (loop t2 acc)]  
    | << >> -> acc 
    | i -> [i::acc] ] in
  loop ty [];

(*
  {[
  ]}
 *)
value app_of_list = fun 
  [ [] -> << >>
  | l -> reduce_left app l ] ;  
value com_of_list  = fun
  [ [] -> << >>
  | l -> reduce_right comma l ];
value sem_of_list  = fun
  [ [] -> << >>
  | l -> reduce_right sem l ]; 
  
(*
   {[
   tuple_of_list [ << a >>; << b >>; << c >> ] |> eprint
   (a, b, c)
   ]}
 *)
value tuple_of_list = fun
  [ [] -> invalid_arg "tuple_of_list while list is empty"
  | [x] -> x
  | xs -> << .$tup:com_of_list xs$. >> ]
;













