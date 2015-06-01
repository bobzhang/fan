+OPTIONS: toc:nil html-postamble:nil html-preamble:nil
======================================================

+HTML\_HEAD: 
=============

+TITLE: Overview of Fan
=======================

+OPTIONS: ^:
============

+OPTIONS: toc:nil
=================

+TOC:headines 2
===============

There is a paper, which gives a high-level priciple about how Fan works,
available here [[file:pdf/metaprogramming\_for\_ocaml.pdf]]

-  The philosophy

Unlike traditional programming languages, Fan is /a set of languages/
which can /evolve/ itself incrementally. It gets started from the
vanilla OCaml language as Fan\_{0}, with more and more new languages
added for each version.

For example, to add a new
[[http://en.wikipedia.org/wiki/Domain-specific\_language][DSL]] into
Fan\_{n}, we would supply a front-end and a back-end targeted at
Fan\_{n}, upon being integrated, Fan\_{n} becomes Fan\_{n+1}.

We can grow Fan endlessly in such a way, however, the main interesting
part for Fan is not that I, the main developer of Fan can keep adding
features to it, its interesting point lies in the following two aspects:

1. It encourages the programmer to think about programming in a
   bottom-up way. Unlike the traditional problem-oriented approach,
   language-oriented programming is encouraged: delimited domain
   specific language([[file:ddsl.org][DDSL]]) is the best abstraction
   for a specific domain.

2. Everyone, including you: the reader, can join and develop a
   mini-compiler and share with other people. Fan servers as a platform
   to glue them tegether.


