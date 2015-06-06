Overview of Fan's architecture
------------------------------

..
   There is a paper, which gives a high-level priciple about how Fan works,
   available here [[file:pdf/metaprogramming\_for\_ocaml.pdf]]

Fan's philosophy

Unlike traditional programming languages, Fan is *a set of languages*
which can *evolve* itself incrementally. It gets started from the
vanilla OCaml language as Fan\ :sub:`0`, with more and more new languages
added for each version.

For example, to add a new DSL into Fan\ :sub:`n`, we would supply a
front-end and a back-end targeted at Fan\ :sub:`n`, upon being
integrated, Fan\ :sub:`n` becomes Fan\ :sub:`n+1`.

We can grow Fan endlessly in such a way, however, the main interesting
part for Fan is not that me, the main developer of Fan can keep adding
features to it, its interesting point lies in the following two aspects:

#. It encourages the programmer to think about programming in a
   bottom-up way. Unlike the traditional problem-oriented approach,
   language-oriented programming is encouraged: delimited domain
   specific language([[file:ddsl.org][DDSL]]) is the best abstraction
   for a specific domain.

#. Everyone, including you: the reader, can join and develop a
   mini-compiler and share with other people. Fan servers as a platform
   to glue them tegether.

