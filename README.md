
Fan is a fully-featured metaprogramming system for OCaml, it's a
superset of [OCaml](http://caml.inria.fr/) with Lisp-like
[macros](http://letoverlambda.com/). It shares the same run-time with
OCaml, its concrete syntax is basically the same as OCaml except for
some minor differences, as we will explain later.


# Why a new metaprogramming system?

Hmm, a good question, it's simply because writing boilerplate code is
tedious and error prone, and being lazy is the virtue of a good programmer. And the core idea for metaprogramming: abstraction
over code provides the ultimate abstraction power,  
We see that a lot of computer scientists are creating languages or
writing mini-compilers to target their domain, but writing a
compiler is itself a domain, Fan is targeted at this domain.

Yes, Fan is aimed to make writing compilers easier by the
combination of metaprogramming and strongly typed functional
languages.

# Why for OCaml?

Fan aims to make
creating a language easier.

There are some projects like Microsoft's Rosyln which aims to make
"Compiler as service", but unlike Rosyln, we do not only expose the
compiler API, but also we provide built-in language support to make
those API easier to use instead, since for any non-trivial
languages, the API is quite complex that average programmers will be
frustrated ans move away.

# Credits:

Fan is a successor to [Camlp4](http://brion.inria.fr/gallium/index.php/Camlp4), which was
mainly developed by [Daniel de Rauglaudre](http://pauillac.inria.fr/~ddr/) and
[Michel Mauny](http://michel.mauny.net/index.en.php), and later was largely renovated by
[Nicolas Pouillard](http://nicolaspouillard.fr/).

Unfortunately, though Camlp4 is a very powerful tool to program
OCaml Ast and Extending OCaml language, Camlp4 is a really complex
beast, that lately it grows out of control. Fan adopts some good
ideas of Camlp4 and merges some new ideas.

Fan is way more **faster** than **Camlp4**, generally 10 times faster!
and Fan has a very robust bootstrapping system compared with Camlp4.

Fan has all the features that Camlp4 has, and much more!

# Status:

Designing a non-toy language is not easy, though Fan is already very
powerful right now, but I have a bunch of ideas which take some time
to implement, so it's not usable now(mostly because some API is
unstable ).


# Contributions

Fan is a non-trivial project, I have already commited millions of
lines revisions during the last year.
Feel free to send me a pull request

# Install

## Requirements

-   [ocaml 4.00 or later](http://caml.inria.fr/ocaml/release.en.html)

-   [ocamlfind or later](http://projects.camlcity.org/projects/findlib.html)


Currently we depend on `ocamlfind`, but the dependency is quite
loose, we will remove the dependency on them when polished

## Initial Install

just type

    ocamlbuild cold/FanDriver.native

## For developers (to continue)

    ./re cold FanDriver.native
    ./hb FanDriver.native 

### What it mainly did

-   Start 
    
    `./re cold FanDriver.native` will make `cold/FanDriver.native`
    the pre-processor engine.  for the cold start(compile from the
    original syntax code).

-   Bootstrap
    `./hb FanDriver.native`
    Using the generated binary to preprocess the revised syntax to
    verify `Fan` reaches a fix point now.

If it reaches fix point, then it succeeds.

### snapshot

    ./snapshot

Pretty print the `revised syntax` to the `cold` Directory for distribution

Everytime, you made some nontrival changes(change the grammar,
lexer), make sure to type `./hb FanDriver.native` or to
verify it can be hot-bootstrapped.
Then type `./snapshot` to snapshot your changes to cold.(This is for
distribution purpose) 

# Directory Structure

## src

The main dev strcuture

## cold

The `mirror` of src, for distribute purpose 

# Docs

It will be coming soon ;-)
