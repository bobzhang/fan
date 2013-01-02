



let meta_int _loc i =  {|$`int:i|};

let meta_int32 _loc i =  {|$`int32:i|};

let meta_int64 _loc i =  {|$`int64:i|};
  
let meta_nativeint _loc i =  {|$`nativeint:i|};
  
let meta_float _loc i = {|$`flo:i|};
  
let meta_string _loc i = {|$`str:i|};
  
let meta_char _loc i = {|$`chr:i|};
let meta_unit _loc _ =  {|()|};
let meta_bool _loc =
  fun [true -> {|true|} | false -> {|false|} ];

(* FIXME [=$] was a token .. *)    
let meta_ref mf_a _loc i =
  {| {contents= $(mf_a _loc !i) } |};
  (* << {val = .$ mf_a _loc i.val $. } >> ;  *)
  
let meta_list mf_a _loc  ls =
  MKLIST _loc (List.map (fun x -> mf_a _loc x ) ls ) ;
let meta_array mf_a _loc ls =
  MKARRAY _loc (Array.map (fun x -> mf_a _loc x) ls)  ;
  
let meta_option mf_a _loc  = fun
  [ None -> {|None|}
  | Some x -> {|Some $(mf_a _loc x)|} ];

let meta_arrow (type t)
    (_mf_a: FanLoc.t -> 'a -> t)
    (_mf_b: FanLoc.t -> 'b ->t)
    (_loc: FanLoc.t)  (_x:'a -> 'b) = invalid_arg "meta_arrow not implemented";



















