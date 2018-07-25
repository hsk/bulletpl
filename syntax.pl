:- module(syntax,[int/1,expr/1,action1/1,firep/1,bullet1/1,elem/1,t/1]).
:- use_module(library(rtg)).
:- discontiguous syntax_ignore/0. 

syntax(maplist(_)). syntax(float). syntax(integer).
int ::= integer.
[A] ::= maplist(A).
option(A)   ::= none | some(A).

expr        ::= float | int
              | expr+expr | expr-expr | expr * expr | expr / expr | expr mod expr
              | -expr | $(int) | $rand | $rank.
speed       ::= spdAbs(expr) | spdRel(expr) | spdSeq(expr) | none.
direction   ::= dirAbs(expr) | dirSeq(expr) | dirAim(expr) | dirRel(expr) | none.
id(_)       ::= atom.
syntax(firep).
syntax(any). any(_).
action1     ::= repeat(expr,[action1])
              | firep
              | changeSpeed(speed,expr)
              | changeDirection(direction,expr)
              | accel(speed,speed,expr)
              | wait(expr)
              | vanish
              | rankUp
              | text(any)
              | action([action1])
              | actionRef(atom,[expr]).
bullet1     ::= bullet(direction,speed,[action1]).
bulletp     ::= bullet1 | bulletRef(atom,[expr]).
fire1       ::= fire(direction, speed, bulletp).
firep       ::= fire1 | fireRef(atom,[expr]).
orientation ::= horizontal | vertical | none.
elem        ::= atom:bullet1
              | atom:action([action1])
              | atom:fire1.
t           ::= bulletML(orientation,[elem]).
