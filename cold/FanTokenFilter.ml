open FanToken
type t =  {
  is_kwd: string -> bool;
  mutable filter: filter} 
open LibUtil
let mk ~is_kwd  = { is_kwd; filter = ignore_layout }
let filter x =
  let f (tok,loc) =
    let tok = keyword_conversion tok x.is_kwd in
    check_keyword_as_label tok loc x.is_kwd; (tok, loc) in
  fun strm  -> x.filter (XStream.map f strm)
let define_filter x f = x.filter <- f x.filter
let keyword_added _ _ _ = ()
let keyword_removed _ _ = ()