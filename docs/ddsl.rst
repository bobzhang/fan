+OPTIONS: toc:nil html-postamble:nil html-preamble:nil
======================================================

+HTML\_HEAD: 
=============

+TITLE: A brief category of domain specific languages
=====================================================

+OPTIONS: ^:
============

According to wiki: #+BEGIN\_QUOTE A domain-specific language (DSL) is a
type of programming language or specification language in software
development and domain engineering dedicated to a particular problem
domain, a particular problem representation technique, and/or a
particular solution technique. #+END\_QUOTE

There are mainly three kinds of DSLs /e.g./, embeded DSL, external DSL
and delimited DSL. 1.
[[http://www.haskell.org/haskellwiki/Embedded\_domain\_specific\_language][Embeded
DSL]] are essentially normal functions, its functionality is inherently
limited, due to lacking staging, program analysis and optimization, yet
it's easy to implement in some expressive languages. 2. External DSL are
real languages targeted at a small domain, like other full blown
programming languages, /i.e./ C, C++, it may include the whole pipeline
of the compiler, from the front-end to back-end. It's more powerful
compared with embeded DSL, yet much more difficult to build an efficient
compiler, and more importantly, it brings problem: language
inter-operation.

3. Delimited DSL. DDSL stands between embeded DSL and external DSL, it
   needs a front-end and backend. Since its backend is the host
   language, compared with external DSL, language inter-operation is
   much easier, learning curve is relatively low.

Fan advocates DDSL, it adopts OCaml as backend, all DDSLs are compiled
into OCaml, since OCaml is a highly portable langauge, which means Fan
is cross-platform for free.

Adopting a high-level language as backend is popular nowadays, some
famous examples includes: [[http://haxe.org/][Haxe]],
[[http://en.wikipedia.org/wiki/Cfront][Cfront]], and numerous
[[http://altjs.org/][javascript backends]].