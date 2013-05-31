open FAst
open LibUtil
open FToken
open Format



type quotation_error_message =
  | Finding
  | Expanding
  | ParsingResult of FLoc.t * string
  | NoName with ("Print")

(* the first argument is quotation name
   the second argument is the position tag  *)
type quotation_error = (name * string * quotation_error_message * exn);; 

exception QuotationError of quotation_error

type 'a expand_fun  = FLoc.t ->  string option -> string -> 'a
  
module ExpKey = FDyn.Pack(struct  type 'a t  = unit end)

module ExpFun = FDyn.Pack(struct  type 'a t  = 'a expand_fun  end)

let current_loc_name = ref None  

let stack = Stack.create ()
  
let current_quot () =
  try Stack.pop stack
  with Stack.Empty -> failwith "it's not in a quotation context"
    
let dump_file = ref None


(* This table is used to resolve the full qualified name first,
   After the full qualified name, is resolved, it is pared with
   tag, to finally resolve expander. So there are two kinds of errors,
   the first one is the full qualified name can not be found, the second
   is the even a fully qualified name can be found, it does not match
   the postition tag.

   Here key should be [absolute path] instead
 *)


  
type key = (name * ExpKey.pack)

module QMap =MapMake (struct type t =key  let compare = compare end)

(**

  [map] is used


  [map]  and [default] is used to help resolve default case {[ {||} ]}

  for example, you can register [Fan.Meta.exp] with [exp] and [stru] positions,
  but when you call {[ with {exp:pat} ]} here, first the name of pat will be resolved
  to be [Fan.Meta.pat], then when you parse {[ {| |} ]} in a position, because its
  name is "", so it will first turn to help from [map], then to default
  
 *)

let map = ref SMap.empty

  
let update (pos,(str:name)) =
  map := SMap.add pos str !map

(* create a table mapping from  (string_of_tag tag) to default
   quotation expander intentionaly make its value a string to
   be more flexibile to incorporating more tags in the future
 *)  
let fan_default = (`Absolute ["Fan"],"")
  
let default: name ref = ref fan_default

let set_default s =  default := s
  
let clear_map () =  map := SMap.empty

let clear_default () = default:= fan_default
  
(* If the quotation has a name, it has a higher precedence,
   otherwise the [position table] has a precedence, otherwise
   the default is used
   the quotation name returned by the parser can only be

   Absolute, Sub, and (Sub [],"")

   The output should be an [absolute name]
 *)
let expander_name ~pos:(pos:string) (name:name) =
  match name with
  | (`Sub [],"") ->
     (* resolve default case *)
     SMap.find_default ~default:(!default) pos !map
  |(`Sub _ ,_) ->
    FToken.resolve_name name
  | _ -> name  
  
let default_at_pos pos str =  update (pos,str)

let expanders_table =ref QMap.empty


let add ((domain,n) as name) (tag : 'a FDyn.tag ) (f:  'a expand_fun) =
  let (k,v) = ((name, ExpKey.pack tag ()), ExpFun.pack tag f) in
  let s  =
    try  Hashtbl.find names_tbl domain with
    | Not_found -> SSet.empty in
  (Hashtbl.replace names_tbl domain (SSet.add  n s);
   expanders_table := QMap.add k v !expanders_table)
        



(* called by [expand] *)
let expand_quotation loc ~expander pos_tag (q_name,q_loc,q_contents) =
  let loc_name_opt = if q_loc = "" then None else Some q_loc in
  try expander loc loc_name_opt q_contents with
  | FLoc.Exc_located (_, (QuotationError _)) as exc ->
     raise exc
  | FLoc.Exc_located (iloc, exc) ->
     let exc1 = QuotationError ( q_name, pos_tag, Expanding, exc) in
     raise (FLoc.Exc_located iloc exc1)
  | exc ->
     let exc1 = QuotationError ( q_name, pos_tag, Expanding, exc) in
     raise (FLoc.Exc_located loc exc1) ;;

  
(* The table is indexed by [quotation name] and [tag] *)
let find loc name tag =
  let key = (expander_name ~pos:(FDyn.string_of_tag tag) name, ExpKey.pack tag ()) in
  let try pack = QMap.find key !expanders_table in
  ExpFun.unpack tag pack
  with
  |Not_found ->
    let pos_tag = FDyn.string_of_tag tag in
    (match name with
    |(`Sub [],"" )->
      (FLoc.raise loc (QuotationError ( name,pos_tag,NoName,Not_found)))
    | _ -> raise Not_found)
  | e -> raise e  ;;

(*
  [tag] is used to help find the expander,
  is passed by the parser function at parsing time
 *)
let expand loc (q_name,q_loc,q_shift,q_contents)    (tag:'a FDyn.tag) : 'a =
  let pos_tag = FDyn.string_of_tag tag in
  let name = q_name in
  let try expander = find loc name tag
  and loc = FLoc.join (FLoc.move `start q_shift loc) in
  begin
    Stack.push  q_name stack;
    finally ~action:(fun _ -> Stack.pop stack)
      (fun _ -> expand_quotation ~expander loc pos_tag (q_name,q_loc,q_contents))
      ()
  end
  with
  | FLoc.Exc_located (_, (QuotationError _)) as exc -> raise exc
  | FLoc.Exc_located (qloc, exc) ->
     raise (FLoc.Exc_located
              (qloc,
              (QuotationError
                 ( name, pos_tag, Finding, exc))))
  | exc ->
     raise (FLoc.Exc_located
              (loc,
              (QuotationError
                 ( name, pos_tag, Finding, exc))))

let quotation_error_to_string (name, position, ctx, exn) =
  let ppf = Buffer.create 30 in
  let name = expander_name ~pos:position name in
  let pp x = bprintf ppf "@?@[<2>While %s %S in a position of %S:" x
                     (string_of_name name) position in
  let () =
    match ctx with
    | Finding ->
       begin
         pp "finding quotation";
         bprintf ppf "@ @[<hv2>Available quotation expanders are:@\n";
         QMap.iter  (fun (s,t) _ ->
                     bprintf ppf "@[<2>%s@ (in@ a@ position@ of %a)@]@ "
                             (string_of_name s) ExpKey.print_tag t)
                    !expanders_table;
         bprintf ppf "@]";
       end
    | Expanding ->  pp "expanding quotation"
    | ParsingResult (loc, str) ->
       begin
         pp "parsing result of quotation" ;
         match !dump_file with
         | Some dump_file ->
            let () = bprintf ppf " dumping result...\n" in
            (try
              let oc = open_out_bin dump_file in
              begin
                output_string oc str;
                output_string oc "\n";
                flush oc;
                close_out oc;
                bprintf ppf "%a:" FLoc.print (FLoc.set_file_name dump_file loc);
              end
            with _ ->
                 bprintf ppf
                         "Error while dumping result in file %S; dump aborted"
                         dump_file)
         | None ->
            bprintf ppf
                    "\n(consider setting variable AstQuotation.dump_file, or using the -QD option)"
       end
    | NoName -> pp "No default quotation name"  in
  let () = bprintf ppf "@\n%s@]@." (Printexc.to_string exn)in Buffer.contents ppf;;

Printexc.register_printer (function
  | QuotationError x -> Some (quotation_error_to_string x )
  | _ -> None);;

let parse_quotation_result parse loc (q_name,_q_loc,_q_shift,q_contents)
    pos_tag str =
  try parse loc str with
  | FLoc.Exc_located (iloc, (QuotationError (n, pos_tag, Expanding, exc))) ->
      let ctx = ParsingResult iloc q_contents in
      let exc1 = QuotationError (n, pos_tag, ctx, exc) in
      FLoc.raise iloc exc1
  | FLoc.Exc_located (iloc, (QuotationError _ as exc)) ->
      FLoc.raise iloc exc
  | FLoc.Exc_located (iloc, exc) ->
      let ctx = ParsingResult iloc q_contents in
      let exc1 = QuotationError (q_name, pos_tag, ctx, exc) in
      FLoc.raise iloc exc1 

    
(* [exp_filter] needs an coercion , we can not finish in one step
   by mexp, since 1. the type has to be relaxed not only to ep, since
   [parse_pat] or [parse_exp] could introduce any type.
   2. the context is a bit missing when expand the antiquotation..
   it expands differently when in exp or pat... 
 *)
let add_quotation ~exp_filter ~pat_filter  ~mexp ~mpat name entry  =
  let entry_eoi = Fgram.eoi_entry entry in
  let expand_exp loc loc_name_opt s =
    Ref.protect2 (FConfig.antiquotations,true) (current_loc_name, loc_name_opt)
      (fun _ ->
        Fgram.parse_string entry_eoi ~loc s |> mexp loc |> exp_filter) in
  let expand_stru loc loc_name_opt s =
    let exp_ast = expand_exp loc loc_name_opt s in
    `StExp(loc,exp_ast) in
  let expand_pat _loc loc_name_opt s =
    Ref.protect FConfig.antiquotations true begin fun _ ->
      let ast = Fgram.parse_string entry_eoi ~loc:_loc s in
      let meta_ast = mpat _loc ast in
      let exp_ast = pat_filter meta_ast in
      (* BOOTSTRAPPING *)
      let rec subst_first_loc name : pat -> pat =  with pat function
        | `App(loc, `Vrn (_,u), (`Par (_, `Com (_,_,rest)))) ->
            `App(loc, `Vrn(loc,u),(`Par (loc,`Com(loc,`Lid (_loc,name),rest))))
        | `App(_loc,`Vrn(_,u),`Any _) ->
            `App(_loc, `Vrn(_loc,u), `Lid(_loc,name))
        | `App(_loc,a,b) -> `App (_loc, subst_first_loc name a , b)
        | `Constraint(_loc,a,ty) -> `Constraint(_loc,subst_first_loc name a,ty)      
              (* | {| $a $b |} -> {| $(subst_first_loc name a) $b |} *)
        |p -> p  in

      (* fun [{:pat| `a ($loc,b,c)|} -> b] *)
      
      match loc_name_opt with
      | None -> subst_first_loc (!FLoc.name) exp_ast
      | Some "_" -> exp_ast
      | Some name -> subst_first_loc name exp_ast 
    end in begin
      add name FDyn.exp_tag expand_exp;
      add name FDyn.pat_tag expand_pat;
      add name FDyn.stru_tag expand_stru;
    end

    
let make_parser entry =
  fun loc loc_name_opt s  ->
    Ref.protect2
      (FConfig.antiquotations, true)
      (current_loc_name,loc_name_opt)
      (fun _ -> Fgram.parse_string (Fgram.eoi_entry entry) ~loc  s);;

DEFINE REGISTER(tag) = fun  ~name ~entry -> add name tag (make_parser entry);;

DEFINE REGISTER_FILTER(tag) = fun ~name ~entry ~filter ->
  add name tag (fun loc loc_name_opt s -> filter (make_parser entry loc loc_name_opt s));;

  
(* let of_stru = REGISTER(FDyn.stru_tag); *)
(* let of_stru_with_filter = REGISTER_FILTER(FDyn.stru_tag); *)
(* let of_pat  = REGISTER(FDyn.pat_tag); *)
(* let of_pat_with_filter  = REGISTER_FILTER(FDyn.pat_tag); *)
(* let of_clfield  = REGISTER(FDyn.clfield_tag); *)
(* let of_clfield_with_filter  = REGISTER_FILTER(FDyn.clfield_tag); *)
(* let of_case = REGISTER(FDyn.case_tag); *)
(* let of_case_with_filter = REGISTER_FILTER(FDyn.case_tag); *)
  

(* (\* both [exp] and [stru] positions are registered *\) *)
(* let of_exp ~name ~entry = *)
(*   let expand_fun =  make_parser entry in *)
(*   let mk_fun loc loc_name_opt s = *)
(*     {:stru'@loc| $(exp:expand_fun loc loc_name_opt s) |} in begin *)
(*       add name FDyn.exp_tag expand_fun ; *)
(*       add name FDyn.stru_tag mk_fun ; *)
(*     end ; *)
  
(* let of_exp_with_filter ~name ~entry ~filter = *)
(*   let expand_fun = *)
(*     fun loc loc_name_opt s -> filter ( make_parser entry loc loc_name_opt s) in *)
(*   let mk_fun loc loc_name_opt s = *)
(*     {:stru'@loc| $(exp:expand_fun loc loc_name_opt s) |} in begin *)
(*       add name FDyn.exp_tag expand_fun ; *)
(*       add name FDyn.stru_tag mk_fun ; *)
(*     end ; *)
  



  
let of_stru ~name  ~entry  = add name FDyn.stru_tag (make_parser entry)

let of_stru_with_filter ~name  ~entry  ~filter  =
  add name FDyn.stru_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser entry loc loc_name_opt s))

let of_pat ~name  ~entry  = add name FDyn.pat_tag (make_parser entry)

let of_pat_with_filter ~name  ~entry  ~filter  =
  add name FDyn.pat_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser entry loc loc_name_opt s))

let of_clfield ~name  ~entry  = add name FDyn.clfield_tag (make_parser entry)

let of_clfield_with_filter ~name  ~entry  ~filter  =
  add name FDyn.clfield_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser entry loc loc_name_opt s))

let of_case ~name  ~entry  = add name FDyn.case_tag (make_parser entry)

let of_case_with_filter ~name  ~entry  ~filter  =
  add name FDyn.case_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser entry loc loc_name_opt s))

let of_exp ~name  ~entry  =
  let expand_fun = make_parser entry in
  let mk_fun loc loc_name_opt s =
    (`StExp (loc, (expand_fun loc loc_name_opt s)) : FAst.stru ) in
  (add name FDyn.exp_tag expand_fun; add name FDyn.stru_tag mk_fun)

let of_exp_with_filter ~name  ~entry  ~filter  =
  let expand_fun loc loc_name_opt s =
    filter (make_parser entry loc loc_name_opt s) in
  let mk_fun loc loc_name_opt s =
    (`StExp (loc, (expand_fun loc loc_name_opt s)) : FAst.stru ) in
  (add name FDyn.exp_tag expand_fun; add name FDyn.stru_tag mk_fun)
