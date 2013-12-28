open Astf
open Ast_gen
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
  | (`Int (_loc,x) : Astf.exp) ->
      (`Int (_loc, (string_of_int (fib @@ (int_of_string x)))) :>Astf.exp)
  | x ->
      let _loc = loc_of x in
      (`App (_loc, (`Lid (_loc, "fib")), (x :>Astf.exp)) :>Astf.exp)
let _ = register_macro ("FIB", fibm)
let macro_expander =
  object (self)
    inherit  Objs.map as super
    method! exp =
      function
      | (`App (_loc,`Uid (_,a),y) : Astf.exp) ->
          ((try
              let f = Hashtbl.find macro_expanders a in
              fun ()  -> self#exp (f y)
            with
            | Not_found  ->
                (fun ()  ->
                   (`App (_loc, (`Uid (_loc, a)), (self#exp y :>Astf.exp)) :>
                   Astf.exp)))) ()
      | e -> super#exp e
  end
