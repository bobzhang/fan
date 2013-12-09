
open Astf
open Ast_gen
  
(** FIXME *)
let _loc = Locf.ghost

(** spliced in the inital lexer with [lexbuf] captured*)    
let auto_binds = 
  [ %bind{
  __ocaml_lex_init_lexbuf  mem_size =
    let pos = lexbuf.lex_curr_pos in
    (lexbuf.lex_mem <- Array.create mem_size (-1) ;
     lexbuf.lex_start_pos <- pos ;
     lexbuf.lex_last_pos <- pos ;
     lexbuf.lex_last_action <- (-1))
  };
   %bind{
   __ocaml_lex_next_char () =
    if lexbuf.lex_curr_pos >= lexbuf.lex_buffer_len then
      begin
        if lexbuf.lex_eof_reached then
          256
        else begin
          lexbuf.refill_buff lexbuf ;
          __ocaml_lex_next_char ()
        end
      end else begin
        let i = lexbuf.lex_curr_pos in
        (lexbuf.lex_curr_pos <- i+1 ;
        Char.code lexbuf.lex_buffer.[i])
      end}]


let output_mem_access (i:int) = %exp{lexbuf.lex_mem.($int':i)}

let (

     (* last_action, *)
     start_pos
    )  =
  ( 

    (* %exp{lexbuf.lex_last_action }, *)
    %exp{ lexbuf.lex_start_pos }
   )
    
    
let output_memory_actions (mvs:Lexgen.memory_action list) : exp list =
  List.map
    (fun x ->
      match x with
      | Lexgen.Copy(tgt,src) ->
          (* let u = output_mem_access tgt in *)
          (* let v = output_mem_access src in  *)
          %exp{ lexbuf.lex_mem.($int':tgt) <- lexbuf.lex_mem.($int':src) }
      | Set tgt ->
          (* let u = output_mem_access tgt in *)
          %exp{ lexbuf.lex_mem.($int':tgt) <- lexbuf.lex_curr_pos }) mvs
    
let lex_state i =
   "__ocaml_lex_state"^string_of_int i

    
(* length at least  one*)    
let output_action (mems:Lexgen.memory_action list) (r:Lexgen.automata_move) : exp list  =
   output_memory_actions mems @
  (match r with
  | Backtrack ->
      [ %exp{lexbuf.lex_curr_pos <- lexbuf.lex_last_pos };
        %exp{lexbuf.lex_last_action}]
  | Goto n -> [%exp{$lid{lex_state n} ()}])

let output_clause
    (pats:int list)
    (mems:Lexgen.memory_action list)
    (r:Lexgen.automata_move) =
  let pat = bar_of_list (List.map (fun x -> %pat{$int':x}) pats) in
  let action = seq_sem (output_action mems r) in 
  %case{ $pat -> $action }

let output_default_clause mems r =
  let action = seq_sem (output_action mems r) in
  %case{ _ -> $action }

(* output at least one case *)    
let output_moves (moves: (Lexgen.automata_move * Lexgen.memory_action list) array) : case list =
  let t = Hashtbl.create 17 in
  let add_move i (m,mems) =
    let (mems,r) = try Hashtbl.find t m with Not_found -> (mems,[]) in
    Hashtbl.replace t m (mems,(i::r)) in
  begin
    for i = 0 to 256 do
      add_move i moves.(i)
    done ;
    let most_frequent = ref Lexgen.Backtrack
    and most_mems = ref []
    and size = ref 0 in
    (Hashtbl.iter
       (fun m (mems,pats) ->
         let size_m = List.length pats in
         if size_m > !size then begin
           most_frequent := m ;
           most_mems := mems;
           size := size_m
         end
       )
       t;
     (Hashtbl.fold 
        (fun m (mems,pats) acc ->
          if m <> !most_frequent then
            output_clause (List.rev pats) mems m  :: acc
          else acc
        ) t []) @ [output_default_clause !most_mems !most_frequent])
  end
    

(* Generate transition code, i.e state transition  *)    
let output_trans (i:int) (trans:Lexgen.automata)=
  let output_tag_actions (mvs:Lexgen.tag_action list) : exp list =
    List.map
      (function
        | Lexgen.SetTag(t,m) ->
            let u = output_mem_access t in
            let v = output_mem_access m in 
            %exp{ $u <- $v }
        | EraseTag(t) ->
            let u = output_mem_access t in 
            %exp{ $u <- -1 }) mvs in
  let e =
    match trans with
    | Perform(n,mvs) ->
        let es = output_tag_actions mvs in 
        seq_sem (es @ [ %exp{ $int':n}]) 
    | Shift(trans,move) ->
        let moves = bar_of_list (output_moves move) in
        seq_sem
          (match trans with
          | Remember(n,mvs) ->
              let es = output_tag_actions mvs in
              (es@
               [ %exp{ lexbuf.lex_last_pos  <- lexbuf.lex_curr_pos };
                 %exp{ lexbuf.lex_last_action <- $int':n };
                 %exp{ match __ocaml_lex_next_char () with | $moves }])
          | No_remember ->
              [ %exp{ match __ocaml_lex_next_char () with | $moves }]) in
  %bind{ $lid{lex_state i} () = $e  }
      
let output_args (args:string list) e =
  List.fold_right (fun a b -> %exp{ fun $lid:a -> $b }) args e
    
let output_automata (transitions:Lexgen.automata array) : bind list =
  transitions
  |> Array.mapi (fun i auto -> output_trans i auto)
  |> Array.to_list


  
    
let output_env (env: Automata_def.t_env) : bind list =
  let env =
    List.sort
      (function x y ->
        match (x,y) with
        | (((p1,_),_), ((p2,_),_)) ->
            if Locf.strictly_before p1 p2 then -1 else 1) env in
  let output_tag_access (x : (Automata_def.tag_base * int ))=
    match x with 
    | (Automata_def.Mem i,d) ->
        %exp{ (${output_mem_access i} + $int':d) }
    | (Start,d) ->
        %exp{ ($start_pos+ $int':d) }
    | (End,d) ->
        %exp{ (lexbuf.lex_curr_pos + $int':d) } in
  List.map
    (fun (((loc,_) as id),v) ->
      let (id : pat) = `Lid id  in
      match (v:Automata_def.ident_info) with
      | Ident_string (o,nstart,nend) ->
          let sub =
            if o then %exp{Lexing.sub_lexeme_opt}
            else %exp{Lexing.sub_lexeme} in
          let nstart = output_tag_access nstart in
          let nend = output_tag_access nend in
          %bind{ $id = $sub lexbuf  $nstart $nend}
      | Ident_char (o,nstart) ->
          (* break the invariant ....
             "Lexing.sub_lexeme_char_opt" can not be an lident...
             it can only be an ident
           *)
          let sub =
            if o then %exp{ Lexing.sub_lexeme_char_opt}
            else  %exp{Lexing.sub_lexeme_char} in
          let nstart = output_tag_access nstart in
          %bind@loc{ $id = $sub lexbuf $nstart }
    ) env
     
let output_entry _loc
    ({Lexgen.auto_mem_size;
    auto_initial_state=(init_num,init_moves);
    auto_actions; },
     (transitions:Lexgen.automata array)) : Astf.exp  =
  let binds =
    (* init_lexbuf,
       next_char,
       lex_state[i] *)
    and_of_list (auto_binds @ output_automata transitions) in 
  let actions = seq_sem
      (%exp{ __ocaml_lex_init_lexbuf  $int':auto_mem_size } ::
       output_memory_actions init_moves) in  
    %exp{
    function (lexbuf:Lexing.lexbuf) ->
      let rec $binds in
      begin
      $actions; (* lex_init_buf .... *)
      let __ocaml_lex_result = $lid{lex_state init_num}  () in
      begin
        lexbuf.lex_start_p <- lexbuf.lex_curr_p ;
        lexbuf.lex_curr_p <-
          { (lexbuf.lex_curr_p) with
            pos_cnum = lexbuf.lex_abs_pos + lexbuf.lex_curr_pos };
        match __ocaml_lex_result with
        | ${auto_actions
            |> List.map
                (function (num,env,act) ->
                  let n = string_of_int num in
                  match output_env env with
                  | [] -> %case{ $int:n -> $act }
                  | xs ->
                      let bind = and_of_list xs in
                      %case{ $int:n -> let $bind in $act }) |> bar_of_list}
        | _ -> failwith   (__MODULE__ ^ "." ^ __BIND__ ^ " lexing: empty token" )
      end
    end
  }


    

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/compile_lex.cmo" *)
(* end: *)
