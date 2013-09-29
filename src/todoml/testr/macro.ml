module MakeMacroParser (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;
  module Ast = Camlp4Ast;
  type item_or_def 'a =
    [ SdStr of 'a
    | SdDef of string and option (list string * Ast.expr)
    | SdUnd of string
    | SdITE of bool and list (item_or_def 'a) and list (item_or_def 'a)
    | SdLazy of Lazy.t 'a ];
  let defined = ref [];
  let is_defined i = List.mem_assoc i !defined;
  let incorrect_number loc l1 l2 =
    FanLoc.raise loc
      (Failure
        (Printf.sprintf "expected %d parameters; found %d"
            (List.length l2) (List.length l1)));
  let define eo x = begin 
      match eo with
      [ Some ([], e) ->
        {:extend|Gram
        expr: Level "simple"
          [ `UID $x -> (new Ast.reloc _loc)#expr e ]
        patt: Level "simple"
          [ `UID $x ->
            let p = Expr.substp _loc [] e
            in (new Ast.reloc _loc)#patt p ] |}
      | Some (sl, e) ->
          {:extend| Gram
            expr: Level "apply"
            [ `UID $x; S{param} ->
              let el =  match param with
              [ {:expr| ($tup:e) |} -> Ast.list_of_expr e []
              | e -> [e] ]  in
              if List.length el = List.length sl then
                let env = List.combine sl el in
                (new Expr.subst _loc env)#expr e
              else
                incorrect_number _loc el sl ]
          patt: Level "simple"
            [ `UID $x; S{param} ->
              let pl = match param with
              [ {:patt| ($tup:p) |} -> Ast.list_of_patt p []
              | p -> [p] ] in
              if List.length pl = List.length sl then
                let env = List.combine sl pl in
                let p = Expr.substp _loc env e in
                (new Ast.reloc _loc)#patt p
              else
                incorrect_number _loc pl sl ] |}
      | None -> () ];
      defined := [(x, eo) :: !defined]
    end;

  let undef x =
    try
      begin
        let eo = List.assoc x !defined in
        match eo with
        [ Some ([], _) -> {:delete| Gram expr: [`UID $x ]  patt: [`UID $x ] |}
        | Some (_, _) ->  {:delete| Gram expr: [`UID $x; S ] patt: [`UID $x; S] |}
        | None -> () ];
        defined := list_remove x !defined;
      end
    with
    [ Not_found -> () ];

  let parse_def s =
    match Gram.parse_string expr (FanLoc.mk "<command line>") s with
    [ {:expr| $uid:n |} -> define None n
    | {:expr| $uid:n = $e |} -> define (Some ([],e)) n
    | _ -> invalid_arg s ];

  (* This is a list of directories to search for INCLUDE statements. *)
  let include_dirs = ref [];

  (* Add something to the above, make sure it ends with a slash. *)
  let add_include_dir str =
    if str <> "" then
      let str =
        if String.get str ((String.length str)-1) = '/'
        then str else str ^ "/"
      in include_dirs := !include_dirs @ [str]
    else ();

  let parse_include_file entry =
    let dir_ok file dir = Sys.file_exists (dir ^ file) in
    fun file ->
      let file =
        try (List.find (dir_ok file) (!include_dirs @ ["./"])) ^ file
        with [ Not_found -> file ]
      in
      let ch = open_in file in
      let st = Stream.of_channel ch in
        Gram.parse entry (FanLoc.mk file) st;

  let rec execute_macro nil cons = fun
    [ SdStr i -> i
    | SdDef (x, eo) -> begin  define eo x; nil  end
    | SdUnd x -> begin  undef x; nil  end
    | SdITE (b, l1, l2) -> execute_macro_list nil cons (if b then l1 else l2)
    | SdLazy l -> Lazy.force l ]

  and execute_macro_list nil cons = fun
  [ [] -> nil
  | [hd::tl] -> (* The evaluation order is important here *)
    let il1 = execute_macro nil cons hd in
    let il2 = execute_macro_list nil cons tl in
    cons il1 il2 ] ;

  (* Stack of conditionals. *)
  let stack = Stack.create () ;

  (* Make an SdITE let by extracting the result of the test from the stack. *)
  let make_SdITE_result st1 st2 =
   let test = Stack.pop stack in
   SdITE test st1 st2 ;

  type branch = [ Then | Else ];

  (* Execute macro only if it belongs to the currently active branch. *)
  let execute_macro_if_active_branch _loc nil cons branch macro_def =
   let test = Stack.top stack in
   let item =
     if (test && branch=Then) || ((not test) && branch=Else) then
      execute_macro nil cons macro_def
     else (* ignore the macro *)
      nil
   in SdStr(item)
   ;

   {:extend|Gram
     local: macro_def macro_def_sig uident_eval_ifdef uident_eval_ifndef
     else_macro_def else_macro_def_sig else_expr smlist_then smlist_else sglist_then
     sglist_else endif opt_macro_value uident ;
    str_item: First
      [ macro_def{x} ->
            execute_macro {:str_item||} (fun a b -> {:str_item| $a; $b |}) x ]
    sig_item: First
      [ macro_def_sig{x} ->
            execute_macro {:sig_item||} (fun a b -> {:sig_item| $a; $b |}) x ]
    macro_def:
      [ "DEFINE"; uident{i}; opt_macro_value{def} -> SdDef i def
      | "UNDEF";  uident{i} -> SdUnd i
      | "IFDEF";  uident_eval_ifdef;  "THEN"; smlist_then{st1}; else_macro_def{st2} ->
          make_SdITE_result st1 st2
      | "IFNDEF"; uident_eval_ifndef; "THEN"; smlist_then{st1}; else_macro_def{st2} ->
          make_SdITE_result st1 st2
      | "INCLUDE"; `STR (_, fname) ->
            SdLazy (lazy (parse_include_file str_items fname)) ]
    macro_def_sig:
      [ "DEFINE"; uident{i} -> SdDef i None
      | "UNDEF";  uident{i} -> SdUnd i
      | "IFDEF";  uident_eval_ifdef;  "THEN"; sglist_then{sg1}; else_macro_def_sig{sg2}
        ->
          make_SdITE_result sg1 sg2
      | "IFNDEF"; uident_eval_ifndef; "THEN"; sglist_then{sg1}; else_macro_def_sig{sg2} ->
            make_SdITE_result sg1 sg2
      | "INCLUDE"; `STR (_, fname) ->
            SdLazy (lazy (parse_include_file sig_items fname)) ]
    uident_eval_ifdef:
      [ uident{i} -> Stack.push (is_defined i) stack ]
    uident_eval_ifndef:
      [ uident{i} -> Stack.push (not (is_defined i)) stack ]
    else_macro_def:
      [ "ELSE"; smlist_else{st}; endif -> st
      | endif -> [] ]
    else_macro_def_sig:
      [ "ELSE"; sglist_else{st}; endif -> st
      | endif -> [] ]
    else_expr:
      [ "ELSE"; expr{e}; endif -> e
      | endif -> {:expr| () |} ]
    smlist_then:
      [ L1
          [ macro_def{d}; semi ->
            execute_macro_if_active_branch _loc {:str_item||} (fun a b -> {:str_item| $a; $b |}) Then d
          | str_item{si}; semi -> SdStr si ]{sml} ->
          sml ]
    smlist_else:
      [ L1 [ macro_def{d}; semi ->
           execute_macro_if_active_branch _loc {:str_item||} (fun a b -> {:str_item| $a; $b |}) Else d
           | str_item{si}; semi -> SdStr si ]{sml} -> sml ]
    sglist_then:
      [ L1 [ macro_def_sig{d}; semi ->
             execute_macro_if_active_branch _loc {:sig_item||} (fun a b -> {:sig_item| $a; $b |}) Then d
           | sig_item{si}; semi -> SdStr si ]{sgl} -> sgl ]   
    sglist_else:
      [ L1 [ macro_def_sig{d}; semi ->
             execute_macro_if_active_branch _loc {:sig_item||} (fun a b -> {:sig_item| $a; $b |}) Else d
           | sig_item{si}; semi -> SdStr si ]{sgl} -> sgl ]  
    endif:
      [ "END" -> ()
      | "ENDIF" -> () ]
    opt_macro_value:
      [ "("; L1 [ `LID x -> x ] SEP ","{pl}; ")"; "="; expr{e} -> Some (pl, e)
      | "="; expr{e} -> Some ([], e)
      | -> None ]
    expr: Level "top"
      [ "IFDEF"; uident{i}; "THEN"; expr{e1}; else_expr{e2} ->
        if is_defined i then e1 else e2
      | "IFNDEF"; uident{i}; "THEN"; expr{e1}; else_expr{e2} ->
          if is_defined i then e2 else e1
      | "DEFINE"; `LID i; "="; expr{def}; "IN"; expr{body} ->
          (new Expr.subst _loc [(i, def)])#expr body ] 
    patt:
      [ "IFDEF"; uident{i}; "THEN"; patt{p1};  "ELSE"; patt{p2}; endif ->
        if is_defined i then p1 else p2
      | "IFNDEF"; uident{i}; "THEN"; patt{p1}; "ELSE"; patt{p2}; endif ->
          if is_defined i then p2 else p1 ]
    uident:
      [ `UID i -> i ]
    (* dirty hack to allow polymorphic variants using the introduced keywords.FIXME *)
    expr: Before "simple"
      [ "`";  [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF"
                | "DEFINE" | "IN" ]{kwd} -> {:expr| `$uid:kwd |}
      | "`"; a_ident{s} -> {:expr| ` $s |} ]
    patt: Before "simple"
      [ "`"; [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF" ]{kwd} ->
            {:patt| `$uid:kwd |}
      | "`"; a_ident{s} -> {:patt| ` $s |} ] |};


  Options.add ("-D", (FanArg.String parse_def),"<string> Define for IFDEF instruction.");
  Options.add ("-U", (FanArg.String undef), "<string> Undefine for IFDEF instruction.");
  Options.add ("-I", (FanArg.String add_include_dir), "<string> Add a directory to INCLUDE search path.");
end;
module MakeNothing (Syn : Sig.Camlp4Syntax) = struct
 module Ast = Camlp4Ast ;
 (* Remove NOTHING and expanse __FILE__ and __LOCATION__ *)
 Syn.AstFilters.register_str_item_filter (Ast.map_expr Expr.map_expr)#str_item;
end;




















