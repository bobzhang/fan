
open LibUtil;
open FanToken;


type assoc =
    [= `NA|`RA|`LA];
type position =
    [= `First | `Last | `Before of string | `After of string | `Level of string];


module Action  = struct
  type  t     = Obj.t   ;
  let mk :'a -> t   = Obj.repr;
  let get: t -> 'a  = Obj.obj ;
  let getf: t-> 'a -> 'b  = Obj.obj ;
  let getf2: t -> 'a -> 'b -> 'c = Obj.obj ;
end;

(* {:fans|derive (OIter ); |}; *)

{:ocaml|

(* the [location] and the parsed value *)
type 'a cont_parse  = FanLoc.t -> Action.t -> parse 'a;
    
type description =
    [= `Normal
    | `Antiquot];

type descr = (description * string) ;  
type token_pattern = ((FanToken.t -> bool) * descr);

type terminal =
    [= `Skeyword of string
    | `Stoken of token_pattern ];
  
type gram = {
    annot : string;
    gfilter         : FanTokenFilter.t;
    gkeywords :  ref SSet.t;
      (* Hashtbl.t string (ref int); *)
    (* glexer          : FanLoc.t -> XStream.t char -> stream; *)
};

type label = option string;
  
type entry = {
    egram     : gram;
    ename     : string;
    estart    : mutable int -> parse Action.t;
    econtinue : mutable int -> cont_parse Action.t;
    edesc     : mutable desc;
    freezed : mutable bool;}
and desc =
    [ Dlevels of list level
    | Dparser of stream -> Action.t ]
and level = {
    assoc   : assoc         ;
    lname   : label;
    productions : list production;
    (* the raw productions stored in the level*)
    lsuffix : tree          ;
    lprefix : tree          }
and symbol =
    [=
     `Smeta of (list string * list symbol * Action.t)
    | `Snterm of entry
    | `Snterml of (entry * string) (* the second argument is the level name *)
    | `Slist0 of symbol
    | `Slist0sep of (symbol * symbol)
    | `Slist1 of symbol
    | `Slist1sep of (symbol * symbol)
    | `Sopt of symbol
    | `Stry of symbol
    | `Speek of symbol
    | `Sself
    | `Snext
    | `Stree of tree
    | terminal ]
and tree = (* internal struccture *)
    [ Node of node
    | LocAct of (* (int*Action.t) *)anno_action and list anno_action (* (int * Action.t) *)
    (* | EarlyAction of Action.t and node (\* This action was only used to produce side effect *\) *)
    (* | ReplaceAction of Action.t and node  *)
    | DeadEnd ]
and node = {
    node    : symbol ;
    son     : tree   ;
    brother : tree   }
and production= (list symbol *  (* Action.t *) (string * Action.t))
and anno_action = (int  * list symbol * string  * Action.t)
      ;
  
  |};


(* FIXME duplciate with Gram.mli*)
type olevel = (label * option assoc * list production);
type extend_statment = (option position * list olevel);
type single_extend_statement =  (option position * olevel);      
type delete_statment = list symbol;

type ('a,'b,'c) fold  =
    entry -> list symbol ->
      (XStream.t 'a -> 'b) -> XStream.t 'a -> 'c;

type  ('a, 'b, 'c) foldsep =
    entry -> list symbol ->
      (XStream.t 'a -> 'b) -> (XStream.t 'a -> unit) -> XStream.t 'a -> 'c;

(* let get_filter g = g.gfilter; *)

let gram_of_entry {egram;_} = egram;
  
(* let using ({ annot ; gkeywords = table(\* ; gfilter = filter; _ *\) } as gram) kwd = *)
(*   let new_keywords = ref [] in *)
(*   if not (SSet.mem kwd !table) then *)
(*     Ref.modify new_keywords (cons kwd); *)
  
    (* failwithf "%s is not in the keywords table %s" kwd  annot; *)
  (* let (tbl,flag) =  SMap.add_with ~f:(+) kwd 1 table in begin *)
  (*   match flag with *)
  (*   [`NotExist -> *)
  (*     FanTokenFilter.keyword_added filter kwd true *)
  (*   | `Exist ->  () ]; *)
  (*   gram.gkeywords <- tbl  *)
  (* end; *)

  
let mk_action=Action.mk;
let string_of_token=FanToken.extract_string  ;

    (* try *)
    (*   let v = SMap.find kwd table in *)
    (*   if v = 1 then begin  *)
    (*     FanTokenFilter.keyword_removed filter kwd ; *)
    (*     gram.gkeywords <- SMap.remove kwd table *)
    (*   end *)
    (*   else *)
    (*     gram.gkeywords <- (fst (SMap.add_with ~f:(-) kwd 1 table )) *)
    (* with *)
    (*   [Not_found -> ()]; *)
  (* in *)

  (* let () = decr r in *)
  (*   if !r = 0 then begin *)
  (*     FanTokenFilter.keyword_removed filter kwd; *)
  (*     Hashtbl.remove table kwd *)
  (*   end else (); *)


(* tree processing *)  
let rec flatten_tree = fun
  [ DeadEnd -> []
  | LocAct (_, _) -> [[]]
  | Node {node = n; brother = b; son = s} ->
      List.map (fun l -> [n::l]) (flatten_tree s) @ flatten_tree b ];

type brothers = [ Bro of symbol and list brothers | End];

type space_formatter =  format unit Format.formatter unit;

let get_brothers x =
  let rec aux acc =  fun
  [ DeadEnd -> List.rev acc 
  | LocAct _ -> List.rev [End:: acc]
  | Node {node = n; brother = b; son = s} ->
          aux [ Bro n (aux [] s) :: acc] b ] in aux [] x ;
let get_children x = 
  let rec aux acc =  fun
  [ [] -> List.rev acc
  | [Bro (n, x)] -> aux [n::acc] x
  | _ -> raise Exit ] in aux [] x ;

(* level -> lprefix -> *)  
let get_first =
  let rec aux acc = fun
     [Node {node;brother;_}
      ->
       aux [node::acc] brother
     |LocAct (_,_) | DeadEnd -> acc ] in
  aux [];

let get_first_from levels set =
  List.iter
    (fun level -> level.lprefix |> get_first |> Hashset.add_list set)
    levels;

  
