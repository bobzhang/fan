open Ast

open AstLib

type key = string 

type expander = exp -> exp 

let macro_expanders: (key,expander) Hashtbl.t = Hashtbl.create 40

let register_macro (k,f) = Hashtbl.replace macro_expanders k f

let rec fib =
  function
  | 0|1 -> 1
  | n when n > 0 -> (fib (n - 1)) + (fib (n - 2))
  | _ -> invalid_arg "fib"

let fibm y =
  match y with
  | (`Int (_loc,x) : Ast.exp) ->
      (`Int (_loc, (string_of_int (fib (int_of_string x)))) : Ast.exp )
  | x ->
      let _loc = loc_of x in
      (`App (_loc, (`Lid (_loc, "fib")), x) : Ast.exp )

let _ = register_macro ("FIB", fibm)

open LibUtil

let macro_expander =
  object (self)
    inherit  Objs.map as super
    method! exp =
      function
      | (`App (_loc,`Uid (_,a),y) : Ast.exp) ->
          ((try
              let f = Hashtbl.find macro_expanders a in
              fun ()  -> self#exp (f y)
            with
            | Not_found  ->
                (fun ()  ->
                   (`App (_loc, (`Uid (_loc, a)), (self#exp y)) : Ast.exp ))))
            ()
      | e -> super#exp e
  end