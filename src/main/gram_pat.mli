type lident = [ `Lid of Location.t * string ]
and simple_pat =
    [ `Alias of Location.t * simple_pat * lident
    | `Ant of Location.t * FanUtil.anti_cxt
    | `Any of Location.t
    | `App of Location.t * simple_pat * simple_pat
    | `Bar of Location.t * simple_pat * simple_pat
    | `Com of Location.t * simple_pat * simple_pat
    | `Lid of Location.t * string
    | `Str of Location.t * string
    | `Vrn of Location.t * string ]
class map :
  object
    method ant : FAst.ant -> FAst.ant
    method lident : lident -> lident
    method loc : Location.t -> Location.t
    method simple_pat : simple_pat -> simple_pat
    method string : string -> string
  end

val wildcarder : map

val simple_pat : simple_pat Gentry.t

val string_of_simple_pat : simple_pat -> string
