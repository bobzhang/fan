

(** This module ipmlements the antiquot filter for quasiquotation with locations
 *)


open FAst


(**
   An object exported for antiquot filters.
   First the quotation is traversed by [Objs.map], the antiquotation in
   [pat] and [exp] is specially treated, they will be first parsed by
   [parse_pat] or [parse_exp](typically [Syntax.pat] and [Syntax.exp]) and decorated
   with some code fragments, for example
   {[
      $uid:x -> `Uid(_loc,x)
      $lid:x -> `Lid(_loc,x)
   ]}
   The decoration rule is simple, please read the code for the decoration rules
 *)  
val antiquot_expander : parse_pat:(loc -> string -> pat) ->
  parse_exp:(loc -> string -> exp) ->  Objs.map
