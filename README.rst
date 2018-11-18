====
levo
====

.. image:: https://img.shields.io/circleci/project/github/buhman/levo.svg?style=flat-square
   :target: https://circleci.com/gh/buhman/levo

overview
--------

levo is a roll expression grammar and interpreter

The language provides multiple infix and unary roll operators on top of
otherwise completely ordinary arithmetic expressions.

why
---

My previous roll expression grammar, unimaginatively called eval_, was not
well-recieved, primarily because it required more syntax for simple
expressions. This was necessary mostly a consequence of the shunting-yard parser
not being able simultaneously provide enough context on each operator while
keeping the language consistent overall.

A direct consequence of this discovery, levo has upgraded from 1961_ to 2004_
technology. The parser is built from PEG `parser combinators`_, yielding an
abstract tree of operators that is recursively evaluated by the interpreter.

.. _eval: https://github.com/buhman/my_first_campaign/tree/master/lua/eval
.. _1961: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
.. _2004: https://en.wikipedia.org/wiki/Parsing_expression_grammar
.. _`parser combinators`: http://wiki.call-cc.org/eggref/4/comparse

comparison
----------

.. list-table::
   :header-rows: 1
   :stub-columns: 1

   * -
     - roll20
     - eval
     - levo
   * - quick maffs
     - `(1 + 2) * -3`
     - `(1 + 2) * (0 - 3)`
     - `(1 + 2) * -3`
   * - one 20-sided die
     - `d20`
     - `1d20s`
     - `d20`
   * - advantage
     - `2d20kh`
     - `2d20kh`
     - `2ld20kh`
   * - dependent dice
     - `[[d10]]d4`
     - `(1d10s)d4s`
     - `d10d4`
