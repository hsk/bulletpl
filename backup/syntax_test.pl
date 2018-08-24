:- use_module(syntax).
:- expr(1.0).
:- expr(1).
:- expr(1+2).
:- expr(1-2).
:- expr(1*2).
:- expr(1 / 2).
:- expr(5 mod 2).
:- expr(-(1)).
:- expr($1).
:- expr($rank).
:- expr($rand).
:- expr(1+2-3*4.0 / 2* $1+ $rand+ $rank).

:- firep(fire(none,none,bulletRef(curve,[]))).

:- action1(wait(1)).
:- int(1).
:- maplist(int,[1,2,3]).
:- action1(wait(1)).
:- firep(fire(none,none,bullet(none,none,[]))).
:- bullet1(bullet(none,none,[])).
:- bullet1(bullet(dirAim(0),spdAbs(1),[])).
:- bullet1(bullet(none,none,[])).

:- firep(fire(none,none,bullet(none,none,[]))).
:- firep(fire(none,none,bullet(dirAim(0),spdAbs(1),[]))).
:- action1(fire(none,none,bullet(none,none,[]))).
:- action1(repeat(1000,[])).
:- action1(repeat(100,[fire(none,none,bullet(none,none,[]))])).

:- elem(a:action([repeat(1000,[])])).
:- t(bulletML(none,[a:action([repeat(1000,[])])])).
:- t(bulletML(none,[a:action([repeat(1000,[action([])])])])).

:- t(bulletML(none,[a:action([repeat(1000,[action([
fire(none,none,bullet(none,none,[])),wait(10)
])])])])).

:- t(bulletML(none,[a:action([repeat(1000,[action([
fire(none,none,bullet(dirAim(0),none,[])),wait(10)
])])])])).
:- t(bulletML(none,[top:action([repeat(1000,[action([
fire(none,none,bullet(dirAim(0),spdAbs(1),[])),wait(10)
])])])])).
:- t(bulletML(none,[top:action([repeat(1000,[action([
fire(none,none,bullet(dirAim(0),spdAbs(1),[])),wait(100)
])])])])).
:- t(bulletML(none,[top:action([repeat(1000,[action([fire(none,none,bullet(dirAim(0),spdAbs(1),[])),wait(100)])])])])).
:- halt.
