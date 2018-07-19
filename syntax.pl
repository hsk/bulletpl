:- module(syntax,[int/1,expr/1,action1/1,firep/1,bullet1/1,elem/1,t/1]).
:- use_module(library(rtg)).
:- discontiguous syntax_ignore/0. 

syntax(maplist(_)). syntax(float). syntax(integer).
int ::= integer.
[A] ::= maplist(A).
option(A)   ::= none | some(A).

expr        ::= float | int
              | expr+expr | expr-expr | expr * expr | expr div expr | expr mod expr
              | -expr | $(int) | rand | rank.
speed       ::= spdAbs(expr) | spdRel(expr) | spdSeq(expr).
direction   ::= dirAbs(expr) | dirSeq(expr) | dirAim(expr) | dirRel(expr).
id(_)       ::= atom.
syntax(firep).
action1     ::= repeat(expr,[action1])
              | firep
              | changeSpeed(speed,expr)
              | changeDirection(direction,expr)
              | accel(option(speed),option(speed),expr)
              | wait(expr)
              | vanish
              | action([action1])
              | actionRef(atom,[expr]).
bullet1     ::= bullet(option(direction),option(speed),[action1]).
bulletp     ::= bullet1 | bulletRef(atom,[expr]).
fire1       ::= fire(option(direction), option(speed), bulletp).
firep       ::= fire1 | fireRef(atom,[expr]).
orientation ::= horizontal | vertical | none.
elem        ::= ebullet(atom,bullet1)
              | eaction(atom,[action1])
              | efire(atom,fire1).
t           ::= bulletML(orientation,[elem]).
