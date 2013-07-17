mk-cps
======

Explorations of Continuation-Passing Style in miniKanren.

The directory `cps-in-mk` contains the result of translating (direct-style) Scheme code into miniKanren, then CPSing the miniKanren code.

The directory `cps-in-scheme` contains the result of CPSing Scheme code, then translating the CPSed code into miniKanren.

To run all the tests in a directory, run in Scheme:

`(load "all-tests.scm")`
