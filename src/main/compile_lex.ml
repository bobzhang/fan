
open Astf
open Ast_gen
  
(** FIXME *)
let _loc = Locf.ghost

(** spliced in the inital lexer with [lexbuf] captured*)    



let output_memory_actions (mvs:Lexgen.memory_action list) : exp list =
  List.map
    (fun (x:Lexgen.memory_action) ->
      match x with
      | Copy(tgt,src) ->
          %exp{ lexbuf.lex_mem.($int':tgt) <- lexbuf.lex_mem.($int':src) }
      | Set tgt ->
          %exp{ lexbuf.lex_mem.($int':tgt) <- lexbuf.lex_curr_pos }) mvs
    
let lex_state i =
   "__ocaml_lex_state"^string_of_int i

    


(** output at least one case
   Called when the automata is shift
 *)    
let output_moves (moves: (Lexgen.automata_move * Lexgen.memory_action list) array) : case list =
  (* length at least  one*)    
  let output_action (mems:Lexgen.memory_action list) (r:Lexgen.automata_move) : exp list  =
    output_memory_actions mems @
    (match r with
    | Backtrack ->
        [ %exp{lexbuf.lex_curr_pos <- lexbuf.lex_last_pos };
          %exp{lexbuf.lex_last_action}]
    | Goto n -> [%exp{$lid{lex_state n} ()}]) in
  let output_clause ?pats
      (mems:Lexgen.memory_action list)
      (r:Lexgen.automata_move) =
    let pat =
      match pats with
      | Some pats ->
          bar_of_list (List.map (fun x -> %pat{$int':x}) pats)
      | None -> %pat{_} in
    let action = seq_sem (output_action mems r) in 
    %case{ $pat -> $action } in
  let (t:(Lexgen.automata_move, (Lexgen.memory_action list * int list)) Hashtbl.t) =
    Hashtbl.create 17 in
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
            output_clause ~pats:(List.rev pats) mems m  :: acc
          else acc
        ) t []) @ [output_clause  !most_mems !most_frequent])
  end
    

(* Generate transition code, i.e state transition  *)    
let output_trans (i:int) (trans:Lexgen.automata)=
  let output_tag_actions (mvs:Lexgen.tag_action list) : exp list =
    List.map
      (function
        | Lexgen.SetTag(t,m) ->
            %exp{ lexbuf.lex_mem.($int':t) <- lexbuf.lex_mem.($int':m) }
        | EraseTag(t) ->
            %exp{ lexbuf.lex_mem.($int':t) <- -1 }) mvs in
  let e =
    match trans with
    | Perform(n,mvs) ->
        seq_sem (output_tag_actions mvs @ [ %exp{ $int':n}]) 
    | Shift(trans,move) ->
        let moves = bar_of_list (output_moves move) in
        seq_sem
          (match trans with
          | Remember(n,mvs) ->
              output_tag_actions mvs @
              [ %exp{ lexbuf.lex_last_pos  <- lexbuf.lex_curr_pos };
                %exp{ lexbuf.lex_last_action <- $int':n };
                %exp{ match __ocaml_lex_next_char () with | $moves }]
          | No_remember ->
              [ %exp{ match __ocaml_lex_next_char () with | $moves }]) in
  %bind{ $lid{lex_state i} () = $e  }
      
let output_args (args:string list) e =
  List.fold_right (fun a b -> %exp{ fun $lid:a -> $b }) args e
    
let output_automata (transitions:Lexgen.automata array) : bind list =
  transitions
  |> Array.mapi (fun i auto -> output_trans i auto)
  |> Array.to_list


  
let offset e i =
  if i = 0 then
    e
  else %exp{ $e + $int':i}
      
let output_env (env: Automata_def.t_env) : bind list =
  let output_tag_access ((x : Automata_def.tag_base),d )=
    offset
      (match x with 
      | Mem i ->  %exp{lexbuf.lex_mem.($int':i)}
      | Start ->  %exp{lexbuf.lex_start_pos}
      | End   ->  %exp{lexbuf.lex_curr_pos}) d in
  env
  |>
    List.sort
      (function x y ->
        match (x,y) with
        | (((p1,_),_), ((p2,_),_)) ->
            if Locf.strictly_before p1 p2 then -1 else 1)
  |>
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
            %bind@loc{ $id = $sub lexbuf $nstart })
      
let output_entry _loc
    ({Lexgen.auto_mem_size;
      auto_initial_state=(init_num,init_moves);
      auto_actions; },
     (transitions:Lexgen.automata array)) : Astf.exp  =
  let auto_binds = 
    [ %bind{
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
        end}] in
  let binds =
    (* init_lexbuf,
       next_char,
       lex_state[i] *)
    and_of_list (auto_binds @ output_automata transitions) in 
  let actions = seq_sem
      (%exp{
       let pos = lexbuf.lex_curr_pos in
       (lexbuf.lex_start_pos <- pos;
        lexbuf.lex_last_pos <- pos;
        lexbuf.lex_last_action <- (-1))} :: 
       (if auto_mem_size > 0 then
         %exp{lexbuf.lex_mem <- Array.create $int':auto_mem_size (-1)} ::
         output_memory_actions init_moves
        else  output_memory_actions init_moves)) in  
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
                  let e = Ast_gen.binds (output_env env) act in
                  %case{$int':num -> $e })
            |> bar_of_list}
        | _ -> failwith   (__MODULE__ ^ "." ^ __BIND__ ^ " lexing: empty token" )
      end
    end
}


    

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/compile_lex.cmo" *)
(* end: *)
