let pp_print_string = StdFan.pp_print_string
let pp_print_vid' = Objs.pp_print_vid'
let pp_print_vid = Objs.pp_print_vid
let pp_print_alident = Objs.pp_print_alident
let pp_print_ant = Objs.pp_print_ant
let fprintf = Format.fprintf
open Astf
let pp_print_loc = function | _f -> (function | _loc -> ())
class mapbase =
  object
    method loc = function | (x : loc) -> x
    method string = function | (x : string) -> x
    method ant = function | (x : ant) -> x
  end
type lident = [ `Lid of (loc* string) ]
and t =
  [ `Vrn of (loc* string)  | `App of (loc* t* t)  | `Lid of (loc* string) 
  | ant | `Com of (loc* t* t)  | `Alias of (loc* t* lident) 
  | `Str of (loc* string)  | `Any of loc ]
class map =
  object (self : 'self_type)
    inherit  mapbase
    method lident : lident -> lident=
      function
      | `Lid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Lid (_a0, _a1)
    method t : t -> t=
      function
      | `Vrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Vrn (_a0, _a1)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#t _a1 in
          let _a2 = self#t _a2 in `App (_a0, _a1, _a2)
      | `Lid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Lid (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :> t)
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#t _a1 in
          let _a2 = self#t _a2 in `Com (_a0, _a1, _a2)
      | `Alias (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#t _a1 in
          let _a2 = self#lident _a2 in `Alias (_a0, _a1, _a2)
      | `Str (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Str (_a0, _a1)
      | `Any _a0 -> let _a0 = self#loc _a0 in `Any _a0
  end
let rec pp_print_lident: Format.formatter -> lident -> unit =
  function
  | fmt ->
      (function
       | `Lid (_a0,_a1) ->
           Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0
             pp_print_string _a1)
and pp_print_t: Format.formatter -> t -> unit =
  function
  | fmt ->
      (function
       | `Vrn (_a0,_a1) ->
           Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" pp_print_loc _a0
             pp_print_string _a1
       | `App (_a0,_a1,_a2) ->
           Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
             pp_print_t _a1 pp_print_t _a2
       | `Lid (_a0,_a1) ->
           Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0
             pp_print_string _a1
       | #ant as _a0 -> (pp_print_ant fmt _a0 :> unit)
       | `Com (_a0,_a1,_a2) ->
           Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
             pp_print_t _a1 pp_print_t _a2
       | `Alias (_a0,_a1,_a2) ->
           Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" pp_print_loc _a0
             pp_print_t _a1 pp_print_lident _a2
       | `Str (_a0,_a1) ->
           Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" pp_print_loc _a0
             pp_print_string _a1
       | `Any _a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc _a0)
let wildcarder =
  object (self)
    inherit  map as super
    method! t =
      function
      | `Lid (_loc,_) -> `Any _loc
      | `Alias (_loc,p,_) -> self#t p
      | p -> super#t p
  end
let p = fprintf
let rec unparse =
  function
  | f ->
      (function
       | (x : t) ->
           (match x with
            | `Vrn (_,s) -> p f "`%s" s
            | `App _ ->
                let l = Ast_basic.list_of_app x [] in
                (match l with
                 | (`Vrn _ as x)::[] -> unparse f x
                 | (`Vrn _ as x)::v::[] -> p f "%a %a" unparse x unparse v
                 | (`Vrn _ as x)::rest ->
                     p f "%a (%a)" unparse x
                       (Formatf.pp_list unparse ~sep:",") rest
                 | _ ->
                     (p Format.err_formatter "impossible pattern %a@."
                        pp_print_t x;
                      invalid_arg "unparse"))
            | `Com (_,a,b) -> p f "%a, %a" unparse a unparse b
            | `Alias (_,p,_) -> unparse f p
            | `Str (_,s) -> p f "%S" s
            | `Any _ -> p f "_"
            | `Lid (_,s) -> p f "%s" s
            | `Ant (_,x) -> p f "$%s" x.txt))
let to_string = Formatf.to_string unparse
