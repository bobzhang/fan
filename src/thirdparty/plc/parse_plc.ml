
let list_of_list_option = fun
  | Some l -> l
  | None -> []

let term_list _loc ts e =
  List.fold_right
    (fun t c ->
      (Comp (Names.cons,[t;c],_loc) : Ast_plc.term)) ts
    (match e with
    | Some t -> t
    | None -> Comp (Names.nil,[],_loc))

let group_rs = ref Compile_plc.group_rs;;

let unwrap_rule_or_masks rd =
    List.fold_left (fun m -> fun
      | `Rule (p,ts,body,_loc) ->
        let (l1,l2) = try Ast_plc.PredMap.find p m with Not_found -> ([],[]) in
          Ast_plc.PredMap.add p ((ts,body,_loc)::l1,l2) m
      | `Mask (p,args,_loc) ->
         let (l1,l2) = try Ast_plc.PredMap.find p m with Not_found -> ([],[]) in
           Ast_plc.PredMap.add p (l1,(args,_loc)::l2) m)
      Ast_plc.PredMap.empty rd
    ;;

Foptions.add ("-nogroup",
              (Unit (fun () -> group_rs := Compile_plc.nogroup_rs)),
              "Don't try to optimally group predicate rules" )


let g = Gramf.create_lexer ~annot:"prolog"
    ~keywords:[".";":";"-";",";"(";")";"=";
               "\\=";"is";"=:=";"=\\=";"<";
               "=<";">";">=";"+"; "_";"!";
               "[";"]";"|";"%:";"?";":-"]
    ();;

%create{(g:Gramf.t) prog rule_or_mask rule body args term bar_term mask  arg_mask};;

%extend{
prog:
  [ L0 rule_or_mask as rd %{
    let res = unwrap_rule_or_masks rd in
    %stru{ ${Ast_gen.sem_of_list (Compile_plc.prog_statics _loc res)} ;;
        ${Ast_gen.sem_of_list (Compile_plc.prog_rules _loc !group_rs res)} }
    }]
rule_or_mask: [ rule as x %{`Rule x} | mask as x %{`Mask x} ]
rule:
  [ Lid x; ? args as t;  ? body as b;  "." %{
    let t = list_of_list_option t and b = list_of_list_option b in
    ((x,List.length t),t,b,_loc) }]
body: [ ":-"; L1 term SEP "," as r %{r} ]
args: [ "(";  L1 term SEP "," as r; ")" %{r} ]
term:
  { "relop" NA
      [ S as x; ("="|"\\="|"is"|"=:="|"=\\="|"<"|"=<"|">"|">=" as  op );
        S as y %{Comp (Names.transform op,[x;y],_loc)}]
      "add" LA
      [ S as x; ("+"|"-" as op ); S as y %{Comp (Names.transform op,[x;y],_loc)}]
      "unary minus" NA
      [ "-"; Int s %{Integer (-(int_of_string s), _loc)}  (* FIXME-NRG *)
      | "-"; S as x %{Comp (Names.neg,[x],_loc)} ]
      "simple" NA
      [ Lid x; ? args as t %{
        (match (x,t) with
        | (x,None) -> Comp (x,[],_loc)
        | (x,Some t) -> Comp (x,t,_loc))}
      | "_" %{Anon _loc}
      | "!" %{Comp (Names.cut,[],_loc)}
      | Uid x %{Var (x,_loc)}
      | Int s %{Integer (int_of_string s, _loc)}
      | "("; S as t; ")" %{t}
      | "["; L0 S SEP "," as t; ? bar_term as e; "]" %{ term_list _loc t e}
      ]}
bar_term: [ "|"; term as t %{t} ]
mask: [ "%:"; Lid x; "(";  L1 arg_mask SEP "," as t; ")" %{((x, List.length t),t,_loc)} ]
arg_mask:
   [ "+"; ? Uid %{ArgClosed _loc}
   | "-"; ? Uid  %{ArgOpen _loc}
   | "?";  ? Uid %{ArgAny _loc}]};;

Ast_quotation.of_stru ~lexer:Lex_plc.from_stream ~name:(Ns.lang,"plc")  ~entry:prog ()

