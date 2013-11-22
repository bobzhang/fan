
(** *)    

open OUnit

open Test_util



let get_tokens s =
  Flex_lib.list_of_string ~verbose:false s 

    
let test_empty_string _ =
  if %str{""}
    |> get_tokens
    |> %p{[`Str({txt="";_}:Tokenf.txt);`EOI _]}
    |> not then
    %err{test_empty_string}

let test_escaped_string _ =
  if %str{"a\n"}
     |> get_tokens
     |> %p{[`Str ({txt = "a\n";_}:Tokenf.txt); `EOI _]}
     |> not then 
    %err{test_escaped_string}

    
(* let test_comment_string _ = *)
(*   if %str{(\*(\**\)*\)} *)
(*      |> get_tokens *)
(*      |> %p{[`Comment ({txt="(\*(\**\)*\)";_}:Tokenf.txt);`EOI _]} *)
(*      |> not then  *)
(*     %err{test_comment_sting} *)
  

let test_char _ =
  if %str{'
'}
   |> get_tokens
   |> %p{[`Chr ({txt="\n";_}:Tokenf.txt); `EOI _]}
   |> not then
    %err{test_char}


(** maybe a bug. Quotation should be loyal to its
    layout *)
let test_string _ =
if %str{"hsoghsogho\n
    haha\
    hahah"}
   |> get_tokens
   |> %p{[`Str ({txt="hsoghsogho\n\n    hahahahah";_}:Tokenf.txt); `EOI _]}
   |> not then
  %err{test_string}

  
    
(* This can not be made an unittest
   since our lexer depends on the context which is bad
*)   
let test_quotation _ =
  if
    %str{%lexer{abcdef}}
    |> get_tokens
    |> %p{ `Quot ({name=
                  (`Sub[],"lexer");
                  loc =
                  {Locf.loc_start =
                   {Locf.pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
                   loc_end =
                   {Locf.pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0;
                    pos_cnum = 14};
                   loc_ghost = false};
                  meta = None;
                  shift = 7;
                  txt = "%lexer{abcdef}";
                  retract = 1
                }:Tokenf.quot) :: _ 
         }
    |> not then
     %err{test_quotation}

(*  Flex_lib.list_of_string ~verbose:false  |> List.hd *)
 (*    === *)
 (*  (`Quot *)
 (*   {Ftoken.name = (`Sub [], "lexer"); *)
 (*    loc = *)
 (*     {Locf.loc_start = *)
 (*       {Locf.pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0}; *)
 (*      loc_end = *)
 (*       {Locf.pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0; *)
 (*        pos_cnum = 14}; *)
 (*      loc_ghost = false}; *)
 (*    meta = None; shift = 7; content = "%lexer{abcdef}"; retract = 1}, *)
 (* {Locf.loc_start = *)
 (*   {Locf.pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0}; *)
 (*  loc_end = *)
 (*   {Locf.pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0; pos_cnum = 14}; *)
 (*  loc_ghost = false}) *)



let test_ant _ =
  Ref.protect Configf.antiquotations true @@ fun _ ->
    if "$aa:a"
       |> get_tokens
       |> %p{[`Ant
                ({loc =
                 {Locf.loc_start =
                  {pos_lnum = 1; pos_bol = 0; pos_cnum = 0;_};
                  loc_end =
                  { pos_lnum = 1; pos_bol = 0; pos_cnum = 5;_};
                  loc_ghost = false};
                 cxt = None; kind = "aa"; txt = "$aa:a"; shift = 4; retract = 0} :Tokenf.ant) ;
              `EOI  _]}
       |> not then
      %err{test_ant}

let test_ant_quot _ =       
  Ref.protect Configf.antiquotations true @@ fun _ ->
    if "$(lid:{|)|})"
       |> get_tokens
       |> %p{
         [
          `Ant
            ({kind = "lid";
              txt = "({|)|})";
              loc =
              {Locf.loc_start =
               {pos_lnum = 1; pos_bol = 0; pos_cnum = 5;_};
               loc_end =
               {pos_lnum = 1; pos_bol = 0; pos_cnum = 12;_};
               loc_ghost = false};
              _} : Tokenf.ant) ;
          `EOI _]}
       |> not then
      %err{test_ant_quot}

    

let test_ant_paren _ =       
  Ref.protect Configf.antiquotations true @@ fun _ ->
    if
      "${(l:>FAst.pat)}"
      |> get_tokens
      |> %p{
        [`Ant
           ({loc =
             {loc_start =
              {pos_lnum = 1; pos_bol = 0; pos_cnum = 0;_};
              loc_end =
              {pos_lnum = 1; pos_bol = 0;
               pos_cnum = 16;_};
              loc_ghost = false};
             cxt = None; kind = "";
             txt = "${(l:>FAst.pat)}"; shift = 2; retract = 1}:Tokenf.ant) ;
         `EOI _]}
      |> not then 
      %err{test_ant_paren}
    

let test_ant_str _ =
  Ref.protect Configf.antiquotations true @@ fun _ ->
    if
      %str{${")"}}
      |> get_tokens
      |> %p{
        [`Ant ({loc =
                {loc_start =
                 { pos_lnum = 1; pos_bol = 0; pos_cnum = 0;_};
                 loc_end =
                 {pos_lnum = 1; pos_bol = 0; pos_cnum = 6;_};
                 loc_ghost = false};
                cxt = None; kind = ""; txt = "${\")\"}";
                shift = 2; retract = 1}:Tokenf.ant); `EOI _ ]}
      |> not then
      %err{test_ant_str}

    

let test_ant_chr _ = 
  Ref.protect Configf.antiquotations true @@ fun _ ->
    if
      %str{${')'}}
      |> get_tokens
      |> %p{
        [`Ant ({loc =
                {loc_start =
                 {pos_lnum = 1; pos_bol = 0; pos_cnum = 0;_};
                 loc_end =
                 {pos_lnum = 1; pos_bol = 0; pos_cnum = 6;_};
                 loc_ghost = false};
                cxt = None; kind = ""; txt = "${')'}"; shift = 2; retract = 1}:
                 Tokenf.ant);
         `EOI _  ]}
      |> not then
      %err{test_ant_chr}



let test_nested_lex _ =
  %str{%extend{%ctyp'{'$(lid:n.tvar)}}}
    ===
      "%extend{%ctyp'{'$(lid:n.tvar)}}"
    
(* let test_comment_pos _ = *)
(*   if *)
(*     "(\*    (\**\) *\)" *)
(*     |> get_tokens *)
(*     |> %p{ *)
(*       [ *)
(*        `Comment *)
(*          ({txt = "(\*    (\**\) *\)"; *)
(*            loc = *)
(*            {Locf.loc_start = *)
(*             {Locf.pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0}; *)
(*             loc_end = *)
(*             {Locf.pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0; pos_cnum = 13}; *)
(*             loc_ghost = false}}:Tokenf.txt); *)
(*        `EOI _]} *)
(*     |> not then *)
(*     %err{test_comment} *)


let test_lex_simple_quot _ =
  if
    Lex_lex.token (Lexing.from_string %str{%{ (** gshoghso *) bhgo "ghos" }})
   |> %p{
     `Quot
       ({
        name = (`Sub [], "");
        loc =
        {Locf.loc_start =
         {Locf.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
         loc_end =
         {Locf.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 32};
         loc_ghost = false};
        meta = None; shift = 2; txt = "%{ (** gshoghso *) bhgo \"ghos\" }";
        retract = 1}:Tokenf.quot)}
   |> not then
    %err{test_lex_simple_quot}
      


let test_symb _ =
  if
    "(%***{)"
    |> get_tokens
    |> %p{
      [ `Key ({txt = "(";_}:Tokenf.txt);
        `Inf ({txt = "%***";
               level = 3;_}:Tokenf.op);
        `Key ({txt= "{";_}:Tokenf.txt);
        `Key ({txt = ")";_}:Tokenf.txt);
        `EOI _ ]}
    |> not then
    %err{test_symb}
  




let test_symb_percent _ =
  if
    "->%"
   |> get_tokens
   |> %p{[`Inf ({txt= "->%"; _} :Tokenf.op); `EOI _ ]}
   |> not then
    %err{test_symb_percent}


    
let test_symb_percent1 _ =
  if
    "[%"
    |> get_tokens
    |> %p{[`Key ({txt = "["; _} : Tokenf.txt);
           `Inf ({txt = "%"; level= 3; _} : Tokenf.op);
           `EOI _  ]}
    |> not
  then 
    %err{test_symb_percent1}
  
let test_symb_percent2 _ =
  if
    "%%"
    |>  get_tokens
    |> %p{  [`Inf ({txt = "%%";level=3;_}:Tokenf.op); `EOI _ ]}
    |> not then
    %err{test_symb_percent2}


let test_symb_percent3 _ =
  if
    "|%"
    |> get_tokens
    |> %p{  [`Inf ({txt = "|%";_}:Tokenf.op); `EOI _ ]}
    |> not then
    %err{test_symb_percent3}


let test_symb_percent4 _ =
  if
    "(%)"
    |> get_tokens
    |> %p{[`Lid ({txt="%";_}:Tokenf.txt); `EOI _ ]}
    |> not then
    %err{test_symb_percent4}

  

let test_symb_percent5 _ =
  if
    "(%"
  |> get_tokens
  |> %p{[`Key ({txt = "(";_}:Tokenf.txt);
         `Inf ({txt="%";level = 3; _}:Tokenf.op); `EOI _ ]}
  |> not then
    %err{test_symb_percent5}

  


let test_single_quot _ =

  if
    "%{'$(a)}"
    |> get_tokens
    |> %p{
      [`Quot
         ({name = (`Sub [], "");
          loc =
          {Locf.loc_start =
           {Locf.pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
           loc_end =
           {Locf.pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0; pos_cnum = 8};
           loc_ghost = false};
          meta = None;
           shift = 2; txt = "%{'$(a)}"; retract = 1}:Tokenf.quot);
       `EOI _ ]}
    |> not then
    %err{test_single_quot}


    
let suite =
  "Lexing_test" >:::
  [
   "test_nested_lex"   >:: test_nested_lex;
   "test_symb_percent" >:: test_symb_percent;
   "test_symb_percent1" >:: test_symb_percent1;
   "test_symb_percent2" >:: test_symb_percent2;
   "test_symb_percent3" >:: test_symb_percent3;
   "test_symb_percent4" >:: test_symb_percent4;
   "test_symb_percent5" >:: test_symb_percent5;
   (* "test_comment_pos" >:: test_comment_pos; *)
   "test_single_quot" >:: test_single_quot;
   "test_ant_chr" >:: test_ant_chr;

   "test_empty_string" >:: test_empty_string;

   "test_escaped_string" >:: test_escaped_string;

   (* "test_comment_string" >:: test_comment_string; *)

   "test_quotation" >:: test_quotation;

   "test_string" >:: test_string;
   
   "test_char" >:: test_char;
   "test_ant" >:: test_ant;
   (* "test_ant_quot" >:: test_ant_quot; *)
   "test_ant_paren" >:: test_ant_paren;
   "test_ant_str" >:: test_ant_str;
   "test_lex_simple_quot" >:: test_lex_simple_quot;
   "test_symb" >:: test_symb
   (* "test_simple_arith" >:: test_simple_arith *)
 ]

    
(* local variables: *)
(* compile-command: "cd .. && pmake test" *)
(* end: *)
