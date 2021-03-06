


# Get started

## build scripts
   - Root.om
   - Configure.om
   - OMakeroot
   - OCaml.om
     Add a general rule to support Fan

     ```
     public.USE_FAN       = false
     public.FAN_PLUGINS   =
     public.FAN           = fan
     public.FAN_PLUGIN_OPTIONS  = $`(mapprefix -plugin, $(FAN_PLUGINS))
     ```
   - OMakefile

   - ./pmake
    a simple omake wrapper with parallel

## targets

   - fan(.run)
     cold start, build fan from preprocessed files
   - coldtop
     fan toplevel
     FIXME
   - ftop
     fan toplevel with utop support
   - fan0(.run)
   - fan1
   - fan2
   - snapshot

# Test its correctness

## when to bootstrapp?


## how to test changes is right

   - at least
     --> hotbuild -> snapshot(source 1)
     --> cold build --> hot build
     --> snapshot (source 2)

     source(1,2) should reach a fix point, it means we can keep the loop
   - unittest

# Troubleshooting
  When you add a new file , and the build system can not track the dependency correctly, fore example

``` ocaml
     Unbound module Ast_gen
```

  Try to `pmake clean` and build it again.


# contracts

## dependency

## family lang quasiquot

# Challenges


## regexp


{:?\([-a-z_\.']*\)\(@[a-z_]+\)?|\(.*?\)|}


problem
->% lexing
|%
[%
{'
=%
%exp-{ %exp'{ { $$x } } }


``` ocaml
  %lexer{
  let a = ghosgho in
  3

  }@{
     let b  = 3
   }

  match x, y with
  | %exp{ 3 + 3 }@loc, %exp{ 1 + 4 }@loc
  |

  %yy{
    %xx{
       (* gshog *)
    }@{
       (* ghosg *)
     }
  }@{
     monad law....
   }


  type t = {
    foo: int; [@default 42]
    bar: float
  } [@@sexp]


  [%lexer
    match foo with
    (Range ('a','z') | Range(1000,1500)), 65 -> Foo
    | Star (Range('a','z')) -> Bar
  ]

  let html =
    %html[
    <h1>Hello $str:world$!</h1>
  ]
```

``` ocaml
%x{ ghso

}@{
ghsogh
}
```



`Field  could be refined

| Pexp_field of expression * Longident.t loc                 |
| Pexp_setfield of expression * Longident.t loc * expression |

# Gram

simple -> Gram_gen.token_of_simple_pat

Gram_def.symbol
{
  text : text;
  styp : styp;
  (* the inferred type of the result parsed by the current symbol *)
  pattern : pat option
}

----

psymbol -> mk_rule
{prod : symbol list;
action: exp option
}

text_of_functorial_extend is the real

The meta explosion is done in gram_gen/make_exp

# monad

let! a = b in
cont


==>
M.bind b (fun a -> cont )



let! a = b
and c = d in
cont
==>


try! a with
case

==>




for!

----

while!

----

match! a with
case
==>
M.bind a (fun case)

# parser



Based on the tree structure, in the module *Gparser* there are two  functions *start_parser_of_entry* and *continue_parser_of_entry*

```ocaml
val start_parser_of_entry :  entry ->  int -> Gaction.t Tokenf.parse
val continue_parser_of_entry :  entry -> int -> Gaction.t cont_parse
```
Note that the parser building process is very fast, there is not too much work involved, for simplicity, currently,
everytime we call *extend_single* by *extend* DDSL, such building process is done immediately as follows:

```ocaml
let extend_single entry
    (lb  : Gdefs.single_extend_statement) =
  let olevel = scan_olevel entry lb in
  let elev = insert_olevel entry lb.label olevel in
  (entry.levels <-  elev;
   entry.start <-Gparser.start_parser_of_entry entry;
   entry.continue <- Gparser.continue_parser_of_entry entry)
```

The parsing behavior is driven by *action_parse* , which is essentially:

```ocaml
entry.start 0 stream
```

------

there is a trade-off here, everytime we do the insertion, and rebuild  the parser is a bit wasteful, if not working this way, the user has to finalize the parser -- which is a burdern to the user

# poc

1. indentation is  a mess :-(

2. location for each token --(todo next )

3. keywords table .... all unsafe_extend????

# TODO
```ocaml
(** It could also import regex in the future
    {:import|
    Lexing_util:
    with_curr_loc
    update_loc ;
   Location_util:
    (--)
    from_lexbuf as  (!!)
    lex_antiquot : %{ a -> b -> c}  as xx ;
   Buffer:
    add_string -> (++)
    add_char -> (+>) ;
   |}  *)
```

# Tracker




* DONE 01 lexer
  How to handle strings in quotation?

  Shall we treat them verbatim or impose some restrictions?
  the following quotations legal

  If we treat them verbatim the following is good (we may not like)
  {:lexer| "|}
  {:lexer| "agho |}

  If we treat only string specially, the following is wrong (should be
  a bug)
  {:lexer| '"' |}


  So to conclude, we should add some checks in quotation?

  how about this
  {:lexer| "|}" |}, if we treat them verbatim, this should fail.

  The problem becomes serious when handling nested quotations and
  anti-quotations:
     A sample code:

     #+BEGIN_SRC ocaml
       and quotation c = {:lexer|
         | '{' (':' quotation_name)? ('@' locname)? '|' (extra_quot as p)?
             ->
               begin
                 store c ;
                 Stack.push p opt_char; (* take care the order matters*)
                 with_curr_loc quotation c ;
                 parse quotation c
               end
         | (extra_quot as p)? "|}" ->
       ... |}
     #+END_SRC

   The current solution is ad-hoc:
     It requires string to follow the ocaml convention, and for char,
     it has a loose requirement, it tries to recognize '"' problem,
     but
     does not prevent '    a' happen.
     #+BEGIN_SRC ocaml
           | "\"" {store c;
                   begin
                     try with_curr_loc string c
                     with FanLoc.Exc_located(_,Lexing_error Unterminated_string) ->
                       err Unterminated_string_in_quotation (loc_merge c)
                   end;
                   Buffer.add_char c.buffer '"';
                   parse quotation c
                 }
           | "'" ( [^ '\\' '\010' '\013'] | '\\' (['\\' '"' 'n' 't' 'b' 'r' ' ' '\'']
           | ['0'-'9'] ['0'-'9'] ['0'-'9'] |'x' hexa_char hexa_char)  as x) "'"
                  {store_parse quotation c }
     #+END_SRC


* DONE 02 illegal begin (ghost locatio horrible error message)

  When the parser raise "illegal begin " excpetion, the location is
  ghost a horrible error message.

  FIX





* 03 paser merge

  #+BEGIN_SRC caml
      (*
      FIXME:
      the merge does not work well with

      prefix:
      |-OPT [ "!"; `Uid _]---.
      `-OPT [ "!"; `Uid _]---stream_exp_comp_list---
      It's better to bring refine STree in the future,
      get rid of Action.t in most cases
     *)

  #+END_SRC


* DONE 04 class parser error

  This is not a bug :-), it's a build script bug in Fan, fixed now.
  can not parse... the class variable
  #+BEGIN_SRC caml
    class c_fold_pattern_vars ['accu] f init =  object
      inherit FanAst.fold as super
      val acc = init
      method acc : 'accu = acc
      method! pat = fun
        [ {:pat| $lid:s |} | {:pat| ~ $s |} | {:pat| ? $s |}
        -> {< acc = f s acc >}
      | p -> super#pat p ]
    end
  #+END_SRC


* 05 generate signatures for Objs module
  This will enhance separate compilation for the project


* 06 ast design

  #+BEGIN_SRC ocaml
    {| $vrn:cons of $par:t |}

    (* make `C to Vrn *)  
    `TyVrnOf(_loc, `C(_,cons), `Par(_,t))
  #+END_SRC

* DONE 07 Stream.Failure overrides the built-in Failure
  open Stream is dangerous...
  our ocamllex engine raises Failrue...
  solution: ==> failwith instead of Failure


* 08 generate fold type and map type in ml file and write them in mli file

* 09 Specialize [eoi_entry] and provide grammar analysis


* 10 EOI problem
  The lexer feeds an EOI at the end of stream, but most entries does
  not handle EOI, if handle EOI, some problem appears

  #+BEGIN_SRC ocaml
  exp_eoi:[ exp{e}; `EOI]
  #+END_SRC
  Given [`Ant; `EOI]
  exp:[
   `Ant  ->
   `Ant "g"; `Ant"ghso"
  ]
  will not parse, since under such case, the second production has a higher priority.


* 11 inling problem again

  #+BEGIN_SRC ocaml
    pat_as_pat_opt:
      [ pat{p1}; "as"; a_lident{s} ->  `Alias (_loc, p1, s)
      | pat{p} -> p ]

    case0:
       [ `Ant (("case"|"" as n),s) -> mk_anti _loc ~c:"case" n s
       | pat_as_pat_opt{p}; "when"; exp{w};  "->"; exp{e} ->
          `CaseWhen (_loc, p, w, e)
       | pat_as_pat_opt{p}; "->";exp{e} -> `Case(_loc,p,e)]
  #+END_SRC
  can we simply inline [pat_as_pat_opt] here?  


* 12 with does not work with
  ident-
  with ident- is not parsed, the problem is the lexer.


* 13 a even light weight syntax
  #+BEGIN_SRC ocaml
  | $a . $b => ghsog
  | ($a $b) => "ghos"
  | $lid:x =>
  | $uid:x => ....
  #+END_SRC


* 14 ctyp can parse uid


  #+BEGIN_SRC caml
    {:ctyp| A |}
    `Uid "A"
  #+END_SRC
  It's not easy to fix currently due to this grammar

  #+BEGIN_SRC caml
           "." LA
            [ S{t1}; "."; S{t2} ->
                try
                  `Dot (_loc, (ident_of_ctyp t1 : ident), (ident_of_ctyp t2)) (* FIXME*)
                with Invalid_argument s -> raise (XStream.Error s) ]

  #+END_SRC


* 15 print generator
  Format.pp_print_string?


* 16 keep the code generation close with dumping
  make it easy to debug


* DONE 17 a lesson while debug
  unit = `Uid "()"
  I used it as a type, and it type check...
  so strict type annotations are helpful
   unit : ep = `Uid "()"


* 18 method syntax in-compatible
  #+BEGIN_SRC caml
    method list : 'a . ?sep:space_formatter -> ?first:space_formatter ->
  #+END_SRC

* 19 make quotation first class, dual to Ant

* 20 rewrite hb script to make it work under windows

* DONE 21 avoid dependency on dynlink for toplevel
  dynlink should appear only in Fan, dynloader, makeBin
  core has a nice trick, but it requires c stubs..



* 22 synthesize the
  meta_int,.. in the compile time? to get rid of the dependency ?


* DONE 23 relax ipat

  now fan accepts such function

  #+BEGIN_SRC ocaml
    let pp_print_ant: Format.formatter -> FAst.ant -> unit =
      function fmt  (`Ant (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" pp_print_loc _a0
          FanUtil.pp_print_anti_cxt _a1
  #+END_SRC


* 24 include fans location problem

  How to avoid the location caused by include combined with filter


* DONE 25 get rid of dependency on ocamlfind
  ocamlfind should not appear in fan, we could split it into a sub
  package fan_utop which depends on ocamlfind


* 26 standard rts ?
  =FanUtil.float_reprs= needs to put some where or
  generated each time?

* 27 fast.mli.depends
  It seems that when ocamlbuild build native code, it will try to pull
  in the module fast.mli.depends when module A depends on module fast.
  OCamlbuild bug?

* 28 how to get ocamldoc API for astLib

* DONE 29 avoid linking dependency on compiler libs
  This is helpful for creating stand-alone binaries and toplevel

* 30 adopt a new macro expansion and conditional compilation scheme, deprecated the macro parser finally

* 31 a new syntax for quotation?

  #+BEGIN_SRC ocaml
    {:exp| f x |}@loc

    vs

    {:exp@loc| f x |}

    vs

    {exp|f $x|}@loc
  #+END_SRC

* 32 gadt any type variable fails
  file:tests/gadt_32.ml


* DONE 33 illegal pattern constructed

  #+BEGIN_SRC ocaml
  {:pat|`A(_,_,_)|}
  (`App  (`App  (`Vrn  \"A\") (`Any )) (`Com  (`Any ) (`Any )))
  #+END_SRC

  This is wrong
  #+BEGIN_SRC ocaml
    with str t simple_pat {|`a(a,b,c)|};;
    - : FGramDef.simple_pat =
    `App
      (, `App (, `Vrn (, "a"), `Lid (, "a")),
       `Com (, `Lid (, "b"), `Lid (, "c")))  
  #+END_SRC

  expected:
  #+BEGIN_SRC ocaml
      {:pat|`a (a,b,c)|}  
  #+END_SRC



* 34 Ident mixed with field access.
  #+BEGIN_SRC ocaml
    # A.B.c.d;;
    Error: Unbound module A
    # A.B.C.d;;
    Ptop_def
      [
        structure_item (//toplevel//[1,0+0]..//toplevel//[1,0+7]) ghost
          Pstr_eval
          expression (//toplevel//[1,0+0]..//toplevel//[1,0+7])
            Pexp_ident "A.B.C.d"
      ]
    # A.B.C.d;;
    Error: Unbound module A
    # a.b.c.d;;
    Ptop_def
      [
        structure_item (//toplevel//[1,0+0]..//toplevel//[1,0+7]) ghost
          Pstr_eval
          expression (//toplevel//[1,0+0]..//toplevel//[1,0+7])
            Pexp_field
            expression (//toplevel//[1,0+0]..//toplevel//[1,0+5])
              Pexp_field
              expression (//toplevel//[1,0+0]..//toplevel//[1,0+3])
                Pexp_field
                expression (//toplevel//[1,0+0]..//toplevel//[1,0+1])
                  Pexp_ident "a"
                "b"
              "c"
            "d"
      ]
    # a.b.c.d;;
    Error: Unbound value a
  #+END_SRC

* 35 mask rule better error message

* 36 always lexing $
  in the future, we should keep each ast nodes with Quotation instead
  of Ant node, that said, we need not handle antiquot node any more?
  no lexing antiquot in the toplevel?


* 37 with language

  #+BEGIN_SRC ocaml
    with
      [
       {:print| |}, (* in this area resorts to [Fan.Derive.Eq]  *)
       eq,
       {:loctype|
         in-module : N;
         with : [ {:print| |} ];

       |},  
     ]  
  #+END_SRC

* 38 Fold needs super

  #+BEGIN_SRC ocaml
    inherit foldbase
    self#string

    inherit foldbase as super
    super#
  #+END_SRC


* 39 remove all XStream.Error ""

* 40 bug keyword

  #+BEGIN_SRC ocaml
    {:extend|
     let kwd:
           [`KEYWORD("DEFINE"|"UNDEF"|"IN"){x} -> x]
     |}

    Fgram.extend_single (kwd : 'kwd Fgram.t )
          (None,
            (None, None,
              [([`Stoken
                   (((function
                      | `KEYWORD ("DEFINE"|"UNDEF"|"IN") -> true
                      | _ -> false)),
                     (`App
                        ((`Vrn "KEYWORD"),
                          (`Bar
                             ((`Bar ((`Str "DEFINE"), (`Str "UNDEF"))),
                               (`Str "IN"))))),
                     "`KEYWORD \"DEFINE\"| \"UNDEF\"| \"IN\"")],
                 ("x\n",
                   (Fgram.mk_action
                      (fun (x : [> FToken.t])  (_loc : FLoc.t)  -> (x : 'kwd )))))]))  
  #+END_SRC
* COMMENT
** rules
  {-# RULES
  "map/map"    forall f g xs.  map f (map g xs) = map (f.g) xs
  "map/append" forall f xs ys. map f (xs ++ ys) = map f xs ++ map f ys
    #-}

  {:map| "row_field"
   fun [ {|$vrn:x of loc|} -> {|$vrn:x|}
   | $vrn:x of (loc * $y) ->
      match y with
      [{:ctyp| $_ * $_ |} -> {|$vrn:x of $tup:y |}
      |_ -> {|$vrn:x of $y|}]   ]

  |}



* COMMENT
  benchmark
  20s ~ 16s

** Diffierence compile time runtime and runtime runtime
  compile time runtime

** merged previous work
  Quite a lot, now could adapt the ADT type easier

** Nested Quotation examples

  #+BEGIN_SRC ocaml
    (*
      Here [s] should be a capital alphabet
      {[
      mee_of_str "A" = {| {| A |}|};
      - : bool = true
      ]}
     *)
    let mee_of_str s =
      let u = {| {:ident| $(uid:$str:s) |} |} in
      {| {| $(id:$u) |} |};

    (*
      Examples:
      {[
      meee_of_str "A" = {| {| {| A |}|}|};
      ]}
     *)
    let meee_of_str s =
      let u = {| {| {:ident| $(uid:$(str:$(str:s))) |} |} |} in
      {| {| {| $(id:$($u))|}|}|};

  #+END_SRC
** introducing a family of two diffierent languages

** {:macro.expr||}
   Diffierent styles macros for diffierent usage

   Some macros are not used as libraries (textual replacement)

   Some could

** {:inject.expr||}
** {:inject.str_item||}

** macro taxnomy

  #+BEGIN_SRC ocaml
    #define LOG(x) cout << __FILE__ << ":" << __LINE__ << " expression: " << x << endl;  
    #define SHOW_VARIABLE(X) cout << "Variable " << #X << " <" << (X) << ">\n";
  #+END_SRC

  from the point of the view of usage
** substitution macros
   #define f(x)
   {y{x} = 3}
   semantics is more clear?

   {:subs.expr|f(x)|};
   {:subs.str_item|f(x)};
   {:subs.pattern|f(x)|};

   swich to a slightly different lexer
   which could recogize #, ##
** staging macros
** change to diffieren lexers
  like the macros above it needs to switch to diffierent lexers?

** c11 constexpr
  #+BEGIN_SRC c++
  constexpr bool is_prime_recursive(size_t number, size_t c){
  return (c*c > number) ? true :
           (number % c == 0) ? false :
              is_prime_recursive(number, c+1);
}

constexpr bool is_prime_func(size_t number){
  return (number <= 1) ? false : is_prime_recursive(number, 2);
}
  #+END_SRC

  The use of constexpr on a function imposes some limitations on what
  that function can do. First, the function must have a non-void
  return type. Second, the function body cannot declare variables or
  define new types. Third, the body may contain only declarations,
  null statements and a single return statement. There must exist
  argument values such that, after argument substitution, the
  expression in the return statement produces a constant expression.




* BUGS
** PR001  ! is a keyword, behaves weird
  #+BEGIN_SRC ocaml
      let (!) = Sys.command;;
    val ( ! ) : string -> int = <fun>
      ! "pwd";;
    Characters 2-7:
      ! "pwd";;
        ^^^^^
    Error: This expression has type string but an expression was expected of type
             'a ref

  #+END_SRC
