  
%import{

Lexing_util:
  lexing_store
  ;

Format:
  fprintf
   std_formatter
   ; 
};;



let from_string  {Locf.loc_start;_} str =
  let lb = Lexing.from_string str in begin 
    lb.lex_abs_pos <- loc_start.pos_cnum;
    lb.lex_curr_p <- loc_start;
    Lex_fan.from_lexbuf lb
  end

let from_stream  {Locf.loc_start;_} strm =
  let lb = Lexing.from_function (lexing_store strm) in begin
    lb.lex_abs_pos <- loc_start.pos_cnum;
    lb.lex_curr_p <- loc_start;
    Lex_fan.from_lexbuf  lb
  end


(* remove trailing `EOI*)  
let rec clean : Tokenf.stream -> Tokenf.stream =  %parser{
  | (`EOI _ as x)  -> %stream{ x}
  |  x; 'xs  -> %stream{ x; 'clean xs}
  |  -> %stream{} }

let rec strict_clean : Tokenf.stream -> Tokenf.stream = %parser{
  | `EOI _  -> %stream{}
  | x; 'xs  -> %stream{ x; 'strict_clean xs }
  |  -> %stream{}} 

let debug_from_string  str =
  let loc = Locf.string_loc  in
  let stream = from_string loc str  in
  stream
  |> clean
  |> Streamf.iter
      (fun t -> fprintf std_formatter "%a@\n" Tokenf.print t )

let list_of_string ?(verbose=true) str =
  let result = ref [] in
  let loc = Locf.string_loc  in
  let stream = from_string loc str  in
  begin 
    stream
    |> clean
    |> Streamf.iter
        (fun t -> begin
          result := t :: !result ;
          if verbose then 
            fprintf std_formatter "%a@\n" Tokenf.print t 
        end) ;
   List.rev !result 
  end

let get_tokens s = list_of_string ~verbose:false s 
  
  
let debug_from_file  file =
  let loc = Locf.mk file in
  let chan = open_in file in
  let stream = Streamf.of_channel  chan in
  from_stream  loc stream |> clean |>
  Streamf.iter @@
  fun t  ->
    fprintf std_formatter "%a@\n" Tokenf.print t


(* local variables: *)
(* compile-command: "cd ../main_annot && pmake flex_lib.cmo" *)
(* end: *)
