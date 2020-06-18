module Csep501 where

{-
Introduction

compiler transformations

division exception -- float out loops - divide by zero if loops does not execute floating out

intermediate representations - abstract syntax tree
front end tied to source language
backend tied to ir

-}
{-

Scanners :


scanner - parser

regular exppression - token - regular expression grammar
parser -> abstract syntax tree -- formal grammar - context free grammar


-- static semantic analysis

type checking
symbol tables -- declerations


-- optimize speed, size, power (multiply instruction vs other

-- optimization - target architecture code generation -


automaton - recognize language
grammar - generate language

language - a set of string - possible infinite

regular language -- context free - context sensitive language


derive a program

token - split classify (keyword, operator, identifier)


deterministic finite automata  - no backktracking -
non deterministic finite automate  - multiple choices -- backtracking

convert regular expressions to - non deterministic finite automata - to deterministic finite automata - code

how to convert dfa to code
code generator --> tables

n sstate ndfa - at most 2^n dfa

unify states
scanner returns tokens --> lexemes - next string found
has next token function -- provides parser an interface to get next token
-}

{-
Parser - grammars

regular expressions - cannot describe - recursive structures -- nested ifs
general grammars - too general

context free grammar - easy - fast to parse (with some restrictions linear time)
cannot capture sematics (for example -- definition usage relationships)

parsers job : discover parse tree from program


top-down : LL(k), recursive descent
bottom up : start at leaves build the tree up LR(k)

grammar is - Non terminal, E terminal, P productions, Start symbolsl

ambigious grammars - two different derivations for same program / no predence - associativity

reduction - shift

LR(1) - left to right - rightmost derivation - 1 lookahaed - LR(1) grammar

-}