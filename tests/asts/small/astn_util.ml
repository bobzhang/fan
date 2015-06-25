open Astfn
let sem = function | a -> (function | b -> `Sem (a, b))
let com = function | a -> (function | b -> `Com (a, b))
let app = function | a -> (function | b -> `App (a, b))
let apply = function | a -> (function | b -> `Apply (a, b))
let sta = function | a -> (function | b -> `Sta (a, b))
let bar = function | a -> (function | b -> `Bar (a, b))
let anda = function | a -> (function | b -> `And (a, b))
let dot = function | a -> (function | b -> `Dot (a, b))
let par = function | x -> `Par x
let seq = function | a -> `Seq a
let arrow = function | a -> (function | b -> `Arrow (a, b))
let typing = function | a -> (function | b -> `Constraint (a, b))
let bar_of_list = function | xs -> Ast_basic.of_listr bar xs
let and_of_list = function | xs -> Ast_basic.of_listr anda xs
let sem_of_list = function | xs -> Ast_basic.of_listr sem xs
let com_of_list = function | xs -> Ast_basic.of_listr com xs
let sta_of_list = function | xs -> Ast_basic.of_listr sta xs
let dot_of_list = function | xs -> Ast_basic.of_listr dot xs
let appl_of_list = function | xs -> Ast_basic.of_listl app xs
let seq_sem = function | ls -> seq (sem_of_list ls)
let binds =
  function
  | bs ->
      (function
       | (e : exp) ->
           (match bs with
            | [] -> e
            | _ ->
                let binds = and_of_list bs in
                (`LetIn (`Negative, (binds :> Astfn.bind), (e :> Astfn.exp)) :> 
                  Astfn.exp)))
let lid = function | (n : string) -> `Lid n
let uid = function | (n : string) -> `Uid n
let unit: ep = `Unit
let tuple_com_unit =
  function | [] -> unit | p::[] -> p | y -> `Par (com_of_list y)
let tuple_com =
  function
  | y ->
      (match y with
       | [] -> invalid_arg ("Astn_util" ^ ("." ^ "tuple_com"))
       | x::[] -> x
       | _ -> `Par (com_of_list y))
let tuple_sta =
  function
  | y ->
      (match y with
       | [] -> invalid_arg ("Astn_util" ^ ("." ^ "tuple_sta"))
       | x::[] -> x
       | _ -> `Par (sta_of_list y))
let (+>) =
  function
  | f ->
      (function
       | (names : string list) -> appl_of_list (f :: (List.map lid names)))
