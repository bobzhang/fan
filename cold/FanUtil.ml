open Format
open LibUtil
let normal_handler =
 function
 | Out_of_memory -> (Some ("Out of memory"))
 | Assert_failure (file , line , char) ->
    (Some
      (sprintf "Assertion failed, file %S, line %d, char %d" file line char))
 | Match_failure (file , line , char) ->
    (Some
      (sprintf "Pattern matching failed, file %S, line %d, char %d" file line
        char))
 | Failure (str) -> (Some (sprintf "Failure: %S" str))
 | Invalid_argument (str) -> (Some (sprintf "Invalid argument: %S" str))
 | Sys_error (str) -> (Some (sprintf "I/O error: %S" str))
 | Stream.Failure -> (Some (sprintf "Parse failure"))
 | Stream.Error (str) -> (Some (sprintf "Parse error: %s" str))
 | _ -> (None)
let _ = (Printexc.register_printer normal_handler)
let valid_float_lexeme =
 fun s ->
  let l = (String.length s) in
  let rec loop =
   fun i ->
    if (i >= l) then ( (s ^ ".") )
    else
     (match (String.get s i) with
      | (('0'
          | ('1'
             | ('2' | ('3' | ('4' | ('5' | ('6' | ('7' | ('8' | '9')))))))))
         | '-') ->
         (loop ( (i + 1) ))
      | _ -> s) in
  (loop 0)
let float_repres =
 fun f ->
  (match (classify_float f) with
   | FP_nan -> "nan"
   | FP_infinite -> if (f < 0.0) then "neg_infinity" else "infinity"
   | _ ->
      let float_val =
       let s1 = (Printf.sprintf "%.12g" f) in
       if (f = ( (float_of_string s1) )) then s1
       else
        let s2 = (Printf.sprintf "%.15g" f) in
        if (f = ( (float_of_string s2) )) then s2
        else (Printf.sprintf "%.18g" f) in
      (valid_float_lexeme float_val))
let cvt_int_literal = fun s -> ((~-) ( (int_of_string ( ("-" ^ s) )) ))
let cvt_int32_literal =
 fun s -> (Int32.neg ( (Int32.of_string ( ("-" ^ s) )) ))
let cvt_int64_literal =
 fun s -> (Int64.neg ( (Int64.of_string ( ("-" ^ s) )) ))
let cvt_nativeint_literal =
 fun s -> (Nativeint.neg ( (Nativeint.of_string ( ("-" ^ s) )) ))
let mk_anti =
 fun ?(c = "") ->
  fun n -> fun s -> ("\\$" ^ ( (n ^ ( (c ^ ( (":" ^ s) )) )) ))
let append_eLem = fun el -> fun e -> (el @ ( [e] ))
let is_antiquot =
 fun s ->
  let len = (String.length s) in
  (( (len > 2) ) && (
    (( (( (String.get s 0) ) = '\\') ) && ( (( (String.get s 1) ) = '$') ))
    ))
let view_antiquot =
 fun s ->
  let len = (String.length s) in
  if (( (len > 2) ) && (
       (( (( (String.get s 0) ) = '\\') ) && ( (( (String.get s 1) ) = '$')
         )) )) then
   (
   (try
     let pos = (String.index s ':') in
     let name = (String.sub s 2 ( (pos - 2) )) in
     let code =
      (String.sub s ( (pos + 1) ) ( (( (( (String.length s) ) - pos) ) - 1)
        )) in
     (Some (name , code))
    with
    Not_found -> (None))
   )
  else (None)
let handle_antiquot_in_string =
 fun ~s ->
  fun ~default ->
   fun ~parse ->
    fun ~loc ->
     fun ~decorate ->
      if (is_antiquot s) then
       (
       let pos = (String.index s ':') in
       let name = (String.sub s 2 ( (pos - 2) ))
       and code =
        (String.sub s ( (pos + 1) ) ( (( (( (String.length s) ) - pos) ) - 1)
          )) in
       (decorate name ( (parse loc code) ))
       )
      else default
let neg_string =
 fun n ->
  let len = (String.length n) in
  if (( (len > 0) ) && ( (( (String.get n 0) ) = '-') )) then
   (
   (String.sub n 1 ( (len - 1) ))
   )
  else ("-" ^ n)
let stream_peek_nth =
 fun n ->
  fun strm ->
   let rec loop =
    fun i ->
     function
     | (x :: xs) -> if (i = 1) then ( (Some (x)) ) else (loop ( (i - 1) ) xs)
     | [] -> (None) in
   (loop n ( (Stream.npeek n strm) ))
let njunk = fun n -> fun strm -> for _i = 1 to n do (Stream.junk strm) done
let rec list_remove =
 fun x ->
  function
  | ((y , _) :: l) when (y = x) -> l
  | (d :: l) -> ( d ) :: (list_remove x l) 
  | [] -> ([])
let symbolchar =
 let list =
  ['$'; '!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?'; '@';
   '^'; '|'; '~'; '\\'] in
 let rec loop =
  fun s ->
   fun i ->
    if (i == ( (String.length s) )) then true 
    else if (List.mem ( (String.get s i) ) list) then
          (
          (loop s ( (i + 1) ))
          )
    else (false) in
 loop
let stopped_at = fun _loc -> (Some (FanLoc.move_line 1 _loc))
let with_open_out_file =
 fun x ->
  fun f ->
   (match x with
    | Some (file) ->
       let oc = (open_out_bin file) in
       (
       (f oc)
       );
       (
       (flush oc)
       );
       (close_out oc)
    | None ->
       ( (set_binary_mode_out stdout true ) ); ( (f stdout) ); (flush stdout))
let dump_ast =
 fun magic ->
  fun ast -> fun oc -> ( (output_string oc magic) ); (output_value oc ast)
let dump_pt =
 fun magic ->
  fun fname ->
   fun pt ->
    fun oc ->
     (
     (output_string oc magic)
     );
     (
     (output_value oc ( if (fname = "-") then "" else fname ))
     );
     (output_value oc pt)
let char_of_char_token =
 fun loc ->
  fun s ->
   (try (TokenEval.char s) with
    (Failure (_) as exn) -> (FanLoc.raise loc exn))
let string_of_string_token =
 fun loc ->
  fun s ->
   (try (TokenEval.string s) with
    (Failure (_) as exn) -> (FanLoc.raise loc exn))
let remove_underscores =
 fun s ->
  let l = (String.length s) in
  let buf = (Buffer.create l) in
  let () =
   (String.iter (
     fun ch ->
      if (ch <> '_') then ( (ignore ( (Buffer.add_char buf ch) )) ) else () )
     s) in
  (Buffer.contents buf)
module Options :
 sig
  type spec_list = (string * Arg.spec * string) list
 val init : (spec_list -> unit)
 val add : (string -> (Arg.spec -> (string -> unit)))
 val print_usage_list : (spec_list -> unit)
 val ext_spec_list : (unit -> spec_list)
 val parse : ((string -> unit) -> (string array -> string list))
 end =
 struct
  type spec_list = (string * Arg.spec * string) list
 open Format
 let rec action_arg =
  fun s ->
   fun sl ->
    function
    | Arg.Unit (f) ->
       if (s = "") then  begin ( (f () ) ); (Some (sl)) end else (None)
    | Arg.Bool (f) ->
       if (s = "") then
        (
        (match sl with
         | (s :: sl) ->
            (try ( (f ( (bool_of_string s) )) ); (Some (sl)) with
             Invalid_argument ("bool_of_string") -> (None))
         | [] -> (None))
        )
       else
        (try ( (f ( (bool_of_string s) )) ); (Some (sl)) with
         Invalid_argument ("bool_of_string") -> (None))
    | Arg.Set (r) ->
       if (s = "") then  begin ( (r := true ) ); (Some (sl)) end else (None)
    | Arg.Clear (r) ->
       if (s = "") then  begin ( (r := false ) ); (Some (sl)) end else (None)
    | Arg.Rest (f) -> ( (List.iter f ( ( s ) :: sl  )) ); (Some (([])))
    | Arg.String (f) ->
       if (s = "") then
        (
        (match sl with | (s :: sl) -> ( (f s) ); (Some (sl)) | [] -> (None))
        )
       else begin
        ( (f s) ); (Some (sl))
       end
    | Arg.Set_string (r) ->
       if (s = "") then
        (
        (match sl with
         | (s :: sl) -> ( (r := s) ); (Some (sl))
         | [] -> (None))
        )
       else begin
        ( (r := s) ); (Some (sl))
       end
    | Arg.Int (f) ->
       if (s = "") then
        (
        (match sl with
         | (s :: sl) ->
            (try ( (f ( (int_of_string s) )) ); (Some (sl)) with
             Failure ("int_of_string") -> (None))
         | [] -> (None))
        )
       else
        (try ( (f ( (int_of_string s) )) ); (Some (sl)) with
         Failure ("int_of_string") -> (None))
    | Arg.Set_int (r) ->
       if (s = "") then
        (
        (match sl with
         | (s :: sl) ->
            (try ( (r := ( (int_of_string s) )) ); (Some (sl)) with
             Failure ("int_of_string") -> (None))
         | [] -> (None))
        )
       else
        (try ( (r := ( (int_of_string s) )) ); (Some (sl)) with
         Failure ("int_of_string") -> (None))
    | Arg.Float (f) ->
       if (s = "") then
        (
        (match sl with
         | (s :: sl) -> ( (f ( (float_of_string s) )) ); (Some (sl))
         | [] -> (None))
        )
       else begin
        ( (f ( (float_of_string s) )) ); (Some (sl))
       end
    | Arg.Set_float (r) ->
       if (s = "") then
        (
        (match sl with
         | (s :: sl) -> ( (r := ( (float_of_string s) )) ); (Some (sl))
         | [] -> (None))
        )
       else begin
        ( (r := ( (float_of_string s) )) ); (Some (sl))
       end
    | Arg.Tuple (specs) ->
       let rec action_args =
        fun s ->
         fun sl ->
          function
          | [] -> (Some (sl))
          | (spec :: spec_list) ->
             (match (action_arg s sl spec) with
              | None -> (action_args "" []  spec_list)
              | Some ((s :: sl)) -> (action_args s sl spec_list)
              | Some (sl) -> (action_args "" sl spec_list)) in
       (action_args s sl specs)
    | Arg.Symbol (syms , f) ->
       (match if (s = "") then sl else ( s ) :: sl  with
        | (s :: sl) when (List.mem s syms) -> ( (f s) ); (Some (sl))
        | _ -> (None))
 let common_start =
  fun s1 ->
   fun s2 ->
    let rec loop =
     fun i ->
      if (( (i == ( (String.length s1) )) ) || (
           (i == ( (String.length s2) )) )) then
       i
      else if (( (String.get s1 i) ) == ( (String.get s2 i) )) then
            (
            (loop ( (i + 1) ))
            )
      else i in
    (loop 0)
 let parse_arg =
  fun fold ->
   fun s ->
    fun sl ->
     (fold (
       fun (name , action , _) ->
        fun acu ->
         let i = (common_start s name) in
         if (i == ( (String.length name) )) then
          (
          (try
            (action_arg ( (String.sub s i ( (( (String.length s) ) - i) )) )
              sl action)
           with
           Arg.Bad (_) -> acu)
          )
         else acu ) None )
 let rec parse_aux =
  fun fold ->
   fun anon_fun ->
    function
    | [] -> ([])
    | (s :: sl) ->
       if (( (( (String.length s) ) > 1) ) && ( (( (String.get s 0) ) = '-')
            )) then
        (
        (match (parse_arg fold s sl) with
         | Some (sl) -> (parse_aux fold anon_fun sl)
         | None -> ( s ) :: (parse_aux fold anon_fun sl) )
        )
       else begin
        ( ((anon_fun s) : unit) ); (parse_aux fold anon_fun sl)
       end
 let align_doc =
  fun key ->
   fun s ->
    let s =
     let rec loop =
      fun i ->
       if (i = ( (String.length s) )) then ""
       else if (( (String.get s i) ) = ' ') then ( (loop ( (i + 1) )) )
       else (String.sub s i ( (( (String.length s) ) - i) )) in
     (loop 0) in
    let (p , s) =
     if (( (String.length s) ) > 0) then
      (
      if (( (String.get s 0) ) = '<') then
       (
       let rec loop =
        fun i ->
         if (i = ( (String.length s) )) then ("" , s)
         else if (( (String.get s i) ) <> '>') then ( (loop ( (i + 1) )) )
         else
          let p = (String.sub s 0 ( (i + 1) )) in
          let rec loop =
           fun i ->
            if (i >= ( (String.length s) )) then (p , "")
            else if (( (String.get s i) ) = ' ') then ( (loop ( (i + 1) )) )
            else (p , ( (String.sub s i ( (( (String.length s) ) - i) )) )) in
          (loop ( (i + 1) )) in
       (loop 0)
       )
      else ("" , s)
      )
     else ("" , "") in
    let tab =
     (String.make (
       (max 1 ( (( (16 - ( (String.length key) )) ) - ( (String.length p) ))
         )) ) ' ') in
    (p ^ ( (tab ^ s) ))
 let make_symlist =
  fun l ->
   (match l with
    | [] -> "<none>"
    | (h :: t) ->
       ((
         (List.fold_left ( fun x -> fun y -> (x ^ ( ("|" ^ y) )) ) (
           ("{" ^ h) ) t) ) ^ "}"))
 let print_usage_list =
  fun l ->
   (List.iter (
     fun (key , spec , doc) ->
      (match spec with
       | Arg.Symbol (symbs , _) ->
          let s = (make_symlist symbs) in
          let synt = (key ^ ( (" " ^ s) )) in
          (eprintf "  %s %s\n" synt ( (align_doc synt doc) ))
       | _ -> (eprintf "  %s %s\n" key ( (align_doc key doc) ))) ) l)
 let remaining_args =
  fun argv ->
   let rec loop =
    fun l ->
     fun i ->
      if (i == ( (Array.length argv) )) then l
      else (loop ( ( ( argv.(i) ) ) :: l  ) ( (i + 1) )) in
   (List.rev ( (loop []  ( (( Arg.current.contents ) + 1) )) ))
 let init_spec_list = (ref [] )
 let ext_spec_list = (ref [] )
 let init = fun spec_list -> (init_spec_list := spec_list)
 let add =
  fun name ->
   fun spec ->
    fun descr ->
     (ext_spec_list := ( ( (name , spec , descr) ) :: ext_spec_list.contents 
       ))
 let fold =
  fun f ->
   fun init ->
    let spec_list =
     (( init_spec_list.contents ) @ ( ext_spec_list.contents )) in
    let specs =
     (Sort.list ( fun (k1 , _ , _) -> fun (k2 , _ , _) -> (k1 >= k2) )
       spec_list) in
    (List.fold_right f specs init)
 let parse =
  fun anon_fun ->
   fun argv ->
    let remaining_args = (remaining_args argv) in
    (parse_aux fold anon_fun remaining_args)
 let ext_spec_list = fun ()  -> ext_spec_list.contents
 end