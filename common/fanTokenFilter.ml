
type t = {
    is_kwd : string -> bool;
      mutable filter : FToken.filter;
  }

let filter x =
  let f (tok, loc) = 
    let tok = FToken.keyword_conversion tok x.is_kwd in begin 
      FToken.check_keyword_as_label tok loc x.is_kwd ;
      (* if !error_on_unknown_keywords  then *)
      (*   check_unknown_keywords tok loc *)
      (* else (); *)
      (tok, loc)
    end in

  fun strm -> x.filter (XStream.map f strm)

let set_filter x f = x.filter <- f x.filter



