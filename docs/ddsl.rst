=====
DDSLs
=====

- Category of Domain Specific Languages

According to wiki::

  A domain-specific language (DSL) is a type of programming language
  or specification language in software development and domain
  engineering dedicated to a particular problem domain, a particular
  problem representation technique, and/or a particular solution
  technique.

There are mainly three kinds of DSLs *e.g.*, embeded DSL, external DSL
and delimited DSL:
  - `Embeded DSL <http://www.haskell.org/haskellwiki/Embedded_domain_specific_language>`_
    are essentially normal functions, its functionality is inherently
    limited, due to lacking staging, program analysis and optimization, yet
    it's easy to implement in some expressive languages.

  - External DSL are real languages targeted at a small domain, like other full blown
    programming languages, *i.e.* C, C++, it may include the whole pipeline
    of the compiler, from the front-end to back-end. It's more powerful
    compared with embeded DSL, yet much more difficult to build an efficient
    compiler, and more importantly, it brings problem: language
    inter-operation.

  - Delimited DSL. DDSL stands between embeded DSL and external DSL, it
    needs a front-end and backend. Since its backend is the host
    language, compared with external DSL, language inter-operation is
    much easier, learning curve is relatively low.

Fan advocates Delimited DSL, it adopts OCaml as backend, all DDSLs are compiled
into OCaml, since OCaml is a highly portable langauge, which means Fan
is cross-platform for free.

Adopting a high-level language as backend is popular nowadays, some
famous examples includes: `Haxe`_, `Cfront`_  and numerous `Altjs`_


.. include:: link_names.txt
