=======================
Fan's development guide
=======================


Build system
============

Fan currently use `omake`_ , there are several relevant omakefiles:
   -  Root.om
   -  Configure.om
   -  OMakeroot
   -  OCaml.om Add a general rule to support Fan ::
      
        public.USE_FAN       = false
        public.FAN_PLUGINS   =  
        public.FAN           = fan  
        public.FAN_PLUGIN_OPTIONS  = $`(mapprefix -plugin, $(FAN_PLUGINS))

   -  OMakefile

   -  ./pmake a simple omake wrapper with parallel


Targets
-------
-  fan(.run) cold start, build fan from preprocessed files
-  coldtop fan toplevel FIXME
-  ftop fan toplevel with utop support
-  fan0(.run)
-  fan1
-  fan2
-  snapshot


Known issues
------------

When you add a new file , and the build system can not track the
dependency correctly, fore example

.. code:: ocaml

         Unbound module Ast_gen

Try to ``pmake clean`` and build it again.

Test its correctness
====================

.. todo::

   when to bootstrapp?


   how to test changes is right


   -  at least --> hotbuild -> snapshot(source 1) --> cold build --> hot
      build --> snapshot (source 2)

      source(1,2) should reach a fix point, it means we can keep the loop
   -  unittest

.. include:: ./link_names.txt


..
   contracts
   =========

   dependency
   ----------

   family lang quasiquot
   ---------------------

..
   Challenges
..
   ==========

   regexp
   ------

   {:?([-a-z\_.']*)(@[a-z\_]+)?\|(.*?)\|}

   problem ->% lexing \|% [% {' =% %exp-{ %exp'{ { $$x } } }

   .. code:: ocaml

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

   .. code:: ocaml

       %x{ ghso

       }@{
       ghsogh
       }

   \`Field could be refined

   | Pexp\_field of expression \* Longident.t loc \|
   | Pexp\_setfield of expression \* Longident.t loc \* expression \|

..
   Gram
   ====

   simple -> Gram\_gen.token\_of\_simple\_pat

   Gram\_def.symbol { text : text; styp : styp; (\* the inferred type of
   the result parsed by the current symbol \*) pattern : pat option }

   --------------

   psymbol -> mk\_rule {prod : symbol list; action: exp option }

   text\_of\_functorial\_extend is the real

   The meta explosion is done in gram\_gen/make\_exp

   monad
   =====

   let! a = b in cont

   ==> M.bind b (fun a -> cont )

   let! a = b and c = d in cont ==>

   try! a with case

   ==>

   for!

   --------------

   while!

   --------------

   match! a with case ==> M.bind a (fun case)

   parser
   ======

   Based on the tree structure, in the module *Gparser* there are two
   functions *start\_parser\_of\_entry* and *continue\_parser\_of\_entry*

   .. code:: ocaml

       val start_parser_of_entry :  entry ->  int -> Gaction.t Tokenf.parse
       val continue_parser_of_entry :  entry -> int -> Gaction.t cont_parse

   Note that the parser building process is very fast, there is not too
   much work involved, for simplicity, currently, everytime we call
   *extend\_single* by *extend* DDSL, such building process is done
   immediately as follows:

   .. code:: ocaml

       let extend_single entry
           (lb  : Gdefs.single_extend_statement) =
         let olevel = scan_olevel entry lb in
         let elev = insert_olevel entry lb.label olevel in
         (entry.levels <-  elev;
          entry.start <-Gparser.start_parser_of_entry entry;
          entry.continue <- Gparser.continue_parser_of_entry entry)

   The parsing behavior is driven by *action\_parse* , which is
   essentially:

   .. code:: ocaml

       entry.start 0 stream

   --------------

   there is a trade-off here, everytime we do the insertion, and rebuild
   the parser is a bit wasteful, if not working this way, the user has to
   finalize the parser -- which is a burdern to the user

   poc
   ===

   1. indentation is a mess :-(

   2. location for each token --(todo next )

   3. keywords table .... all unsafe\_extend????

   TODO
   ====

   .. code:: ocaml

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

   Tracker
   =======

   -  DONE 01 lexer How to handle strings in quotation?

   Shall we treat them verbatim or impose some restrictions? the following
   quotations legal

   If we treat them verbatim the following is good (we may not like)
   {:lexer\| "\|} {:lexer\| "agho \|}

   If we treat only string specially, the following is wrong (should be a
   bug) {:lexer\| '"' \|}

   So to conclude, we should add some checks in quotation?

   how about this {:lexer\| "\|}" \|}, if we treat them verbatim, this
   should fail.

   The problem becomes serious when handling nested quotations and
   anti-quotations: A sample code:

   ::

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

   The current solution is ad-hoc: It requires string to follow the ocaml
   convention, and for char, it has a loose requirement, it tries to
   recognize '"' problem, but does not prevent ' a' happen. #+BEGIN\_SRC
   ocaml \| """ {store c; begin try with\_curr\_loc string c with
   FanLoc.Exc\_located(\ *,Lexing*\ error Unterminated\_string) -> err
   Unterminated\_string\_in\_quotation (loc\_merge c) end; Buffer.add\_char
   c.buffer '"'; parse quotation c } \| "'" ( [^ '\\' '10' '13'] \| '\\'
   (['\\' '"' 'n' 't' 'b' 'r' ' ' '''] \| ['0'-'9'] ['0'-'9'] ['0'-'9']
   \|'x' hexa\_char hexa\_char) as x) "'" {store\_parse quotation c }
   #+END\_SRC

   -  DONE 02 illegal begin (ghost locatio horrible error message)

   When the parser raise "illegal begin " excpetion, the location is ghost
   a horrible error message.

   FIX

   -  03 paser merge

   #+BEGIN\_SRC caml (\* FIXME: the merge does not work well with

   ::

         prefix:
         |-OPT [ "!"; `Uid _]---.
         `-OPT [ "!"; `Uid _]---stream_exp_comp_list---
         It's better to bring refine STree in the future,
         get rid of Action.t in most cases
        *)

   #+END\_SRC

   -  DONE 04 class parser error

   This is not a bug :-), it's a build script bug in Fan, fixed now. can
   not parse... the class variable #+BEGIN\_SRC caml class
   c\_fold\_pattern\_vars ['accu] f init = object inherit FanAst.fold as
   super val acc = init method acc : 'accu = acc method! pat = fun [
   {:pat\| :math:`lid:s |} | {:pat| ~ `\ s \|} \| {:pat\| ? $s \|} -> {<
   acc = f s acc >} \| p -> super#pat p ] end #+END\_SRC

   -  05 generate signatures for Objs module This will enhance separate
      compilation for the project

   -  06 ast design

   #+BEGIN\_SRC ocaml {\| :math:`vrn:cons of `\ par:t \|}

   ::

       (* make `C to Vrn *)  
       `TyVrnOf(_loc, `C(_,cons), `Par(_,t))

   #+END\_SRC

   -  DONE 07 Stream.Failure overrides the built-in Failure open Stream is
      dangerous... our ocamllex engine raises Failrue... solution: ==>
      failwith instead of Failure

   -  08 generate fold type and map type in ml file and write them in mli
      file

   -  09 Specialize [eoi\_entry] and provide grammar analysis

   -  10 EOI problem The lexer feeds an EOI at the end of stream, but most
      entries does not handle EOI, if handle EOI, some problem appears

   #+BEGIN\_SRC ocaml exp\_eoi:[ exp{e};
   ``EOI]   #+END_SRC   Given [``\ Ant; ``EOI]   exp:[``\ Ant ->
   ``Ant "g";``\ Ant"ghso" ] will not parse, since under such case, the
   second production has a higher priority.

   -  11 inling problem again

   #+BEGIN\_SRC ocaml pat\_as\_pat\_opt: [ pat{p1}; "as"; a\_lident{s} ->
   \`Alias (\_loc, p1, s) \| pat{p} -> p ]

   ::

       case0:
          [ `Ant (("case"|"" as n),s) -> mk_anti _loc ~c:"case" n s
          | pat_as_pat_opt{p}; "when"; exp{w};  "->"; exp{e} ->
             `CaseWhen (_loc, p, w, e)
          | pat_as_pat_opt{p}; "->";exp{e} -> `Case(_loc,p,e)]

   #+END\_SRC can we simply inline [pat\_as\_pat\_opt] here?

   -  12 with does not work with ident- with ident- is not parsed, the
      problem is the lexer.

   -  13 a even light weight syntax #+BEGIN\_SRC ocaml \| :math:`a . `\ b
      => ghsog \| (:math:`a `\ b) => "ghos" \| :math:`lid:x =>   | `\ uid:x
      => .... #+END\_SRC

   -  14 ctyp can parse uid

   #+BEGIN\_SRC caml {:ctyp\| A \|} \`Uid "A" #+END\_SRC It's not easy to
   fix currently due to this grammar

   #+BEGIN\_SRC caml "." LA [ S{t1}; "."; S{t2} -> try \`Dot (*loc,
   (ident*\ of\_ctyp t1 : ident), (ident\_of\_ctyp t2)) (\* FIXME\*) with
   Invalid\_argument s -> raise (XStream.Error s) ]

   #+END\_SRC

   -  15 print generator Format.pp\_print\_string?

   -  16 keep the code generation close with dumping make it easy to debug

   -  DONE 17 a lesson while debug unit =
      ``Uid "()"   I used it as a type, and it type check...   so strict type annotations are helpful    unit : ep =``\ Uid
      "()"

   -  18 method syntax in-compatible #+BEGIN\_SRC caml method list : 'a .
      ?sep:space\_formatter -> ?first:space\_formatter -> #+END\_SRC

   -  19 make quotation first class, dual to Ant

   -  20 rewrite hb script to make it work under windows

   -  DONE 21 avoid dependency on dynlink for toplevel dynlink should
      appear only in Fan, dynloader, makeBin core has a nice trick, but it
      requires c stubs..

   -  22 synthesize the meta\_int,.. in the compile time? to get rid of the
      dependency ?

   -  DONE 23 relax ipat

   now fan accepts such function

   #+BEGIN\_SRC ocaml let pp\_print\_ant: Format.formatter -> FAst.ant ->
   unit = function fmt
   (``Ant (_a0,_a1))  ->         Format.fprintf fmt "@[<1>(``\ Ant@ %a@
   %a)@]" pp\_print\_loc *a0 FanUtil.pp*\ print\_anti\_cxt *a1 #+END*\ SRC

   -  24 include fans location problem

   How to avoid the location caused by include combined with filter

   -  DONE 25 get rid of dependency on ocamlfind ocamlfind should not
      appear in fan, we could split it into a sub package fan\_utop which
      depends on ocamlfind

   -  26 standard rts ? =FanUtil.float\_reprs= needs to put some where or
      generated each time?

   -  27 fast.mli.depends It seems that when ocamlbuild build native code,
      it will try to pull in the module fast.mli.depends when module A
      depends on module fast. OCamlbuild bug?

   -  28 how to get ocamldoc API for astLib

   -  DONE 29 avoid linking dependency on compiler libs This is helpful for
      creating stand-alone binaries and toplevel

   -  30 adopt a new macro expansion and conditional compilation scheme,
      deprecated the macro parser finally

   -  31 a new syntax for quotation?

   #+BEGIN\_SRC ocaml {:exp\| f x \|}@loc

   ::

       vs

       {:exp@loc| f x |}

       vs

       {exp|f $x|}@loc

   #+END\_SRC

   -  32 gadt any type variable fails file:tests/gadt\_32.ml

   -  DONE 33 illegal pattern constructed

   #+BEGIN\_SRC ocaml {:pat\|``A(_,_,_)|}   (``\ App (``App  (``\ Vrn "A")
   (``Any )) (``\ Com (``Any ) (``\ Any ))) #+END\_SRC

   | This is wrong #+BEGIN\_SRC ocaml with str t simple\_pat
   {\|``a(a,b,c)|};;     - : FGramDef.simple_pat =``\ App (,
   ``App (,``\ Vrn (, "a"), ``Lid (, "a")),``\ Com (, ``Lid (, "b"),``\ Lid
   (, "c")))
   |  #+END\_SRC

   | expected: #+BEGIN\_SRC ocaml {:pat\|\`a (a,b,c)\|}
   |  #+END\_SRC

   -  34 Ident mixed with field access. #+BEGIN\_SRC ocaml # A.B.c.d;;
      Error: Unbound module A # A.B.C.d;; Ptop\_def [ structure\_item
      (//toplevel//[1,0+0]..//toplevel//[1,0+7]) ghost Pstr\_eval
      expression (//toplevel//[1,0+0]..//toplevel//[1,0+7]) Pexp\_ident
      "A.B.C.d" ] # A.B.C.d;; Error: Unbound module A # a.b.c.d;; Ptop\_def
      [ structure\_item (//toplevel//[1,0+0]..//toplevel//[1,0+7]) ghost
      Pstr\_eval expression (//toplevel//[1,0+0]..//toplevel//[1,0+7])
      Pexp\_field expression (//toplevel//[1,0+0]..//toplevel//[1,0+5])
      Pexp\_field expression (//toplevel//[1,0+0]..//toplevel//[1,0+3])
      Pexp\_field expression (//toplevel//[1,0+0]..//toplevel//[1,0+1])
      Pexp\_ident "a" "b" "c" "d" ] # a.b.c.d;; Error: Unbound value a
      #+END\_SRC

   -  35 mask rule better error message

   -  36 always lexing $ in the future, we should keep each ast nodes with
      Quotation instead of Ant node, that said, we need not handle antiquot
      node any more? no lexing antiquot in the toplevel?

   -  37 with language

   #+BEGIN\_SRC ocaml with [ {:print\| \|}, (\* in this area resorts to
   [Fan.Derive.Eq] \*) eq, {:loctype\| in-module : N; with : [ {:print\|
   \|} ];

   ::

          |},  
        ]  

   #+END\_SRC

   -  38 Fold needs super

   #+BEGIN\_SRC ocaml inherit foldbase self#string

   ::

       inherit foldbase as super
       super#

   #+END\_SRC

   -  39 remove all XStream.Error ""

   -  40 bug keyword

   #+BEGIN\_SRC ocaml {:extend\| let kwd:
   [\`KEYWORD("DEFINE"\|"UNDEF"\|"IN"){x} -> x] \|}

   ::

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

   #+END\_SRC \* COMMENT \*\* rules {-# RULES "map/map" forall f g xs. map
   f (map g xs) = map (f.g) xs "map/append" forall f xs ys. map f (xs ++
   ys) = map f xs ++ map f ys #-}

   {:map\| "row\_field" fun [ {\|:math:`vrn:x of loc|} -> {|`\ vrn:x\|} \|
   :math:`vrn:x of (loc * `\ y) -> match y with [{:ctyp\| :math:`_ * `\ \_
   \|} -> {\|:math:`vrn:x of `\ tup:y \|} \|\_ ->
   {\|:math:`vrn:x of `\ y\|}] ]

   \|}

   -  COMMENT benchmark 20s ~ 16s

   \*\* Diffierence compile time runtime and runtime runtime compile time
   runtime

   \*\* merged previous work Quite a lot, now could adapt the ADT type
   easier

   \*\* Nested Quotation examples

   #+BEGIN\_SRC ocaml (* Here [s] should be a capital alphabet {[
   mee\_of\_str "A" = {\| {\| A \|}\|}; - : bool = true ]} *) let
   mee\_of\_str s = let u = {\| {:ident\| :math:`(uid:`\ str:s) \|} \|} in
   {\| {\| :math:`(id:`\ u) \|} \|};

   ::

       (*
         Examples:
         {[
         meee_of_str "A" = {| {| {| A |}|}|};
         ]}
        *)
       let meee_of_str s =
         let u = {| {| {:ident| $(uid:$(str:$(str:s))) |} |} |} in
         {| {| {| $(id:$($u))|}|}|};

   #+END\_SRC \*\* introducing a family of two diffierent languages

   \*\* {:macro.expr\|\|} Diffierent styles macros for diffierent usage

   Some macros are not used as libraries (textual replacement)

   Some could

   \*\* {:inject.expr\|\|} \*\* {:inject.str\_item\|\|}

   \*\* macro taxnomy

   | #+BEGIN\_SRC ocaml #define LOG(x) cout << **FILE** << ":" << **LINE**
   << " expression: " << x << endl;
   |  #define SHOW\_VARIABLE(X) cout << "Variable " << #X << " <" << (X) <<
   ">"; #+END\_SRC

   from the point of the view of usage \*\* substitution macros #define
   f(x) {y{x} = 3} semantics is more clear?

   {:subs.expr\|f(x)\|}; {:subs.str\_item\|f(x)}; {:subs.pattern\|f(x)\|};

   swich to a slightly different lexer which could recogize #, ## \*\*
   staging macros \*\* change to diffieren lexers like the macros above it
   needs to switch to diffierent lexers?

   \*\* c11 constexpr #+BEGIN\_SRC c++ constexpr bool
   is\_prime\_recursive(size\_t number, size\_t c){ return (c\*c > number)
   ? true : (number % c == 0) ? false : is\_prime\_recursive(number, c+1);
   }

   constexpr bool is\_prime\_func(size\_t number){ return (number <= 1) ?
   false : is\_prime\_recursive(number, 2); } #+END\_SRC

   The use of constexpr on a function imposes some limitations on what that
   function can do. First, the function must have a non-void return type.
   Second, the function body cannot declare variables or define new types.
   Third, the body may contain only declarations, null statements and a
   single return statement. There must exist argument values such that,
   after argument substitution, the expression in the return statement
   produces a constant expression.

   -  BUGS \*\* PR001 ! is a keyword, behaves weird #+BEGIN\_SRC ocaml let
      (!) = Sys.command;; val ( ! ) : string -> int = ! "pwd";; Characters
      2-7: ! "pwd";; :sup:`^`\ ^^ Error: This expression has type string
      but an expression was expected of type 'a ref

   #+END\_SRC
