:- expects_dialect(sicstus).
a :- bb_update(dt,DT,[a|DT]).
b :- bb_update(dt,DT,[b|DT]).
c :- bb_update(dt,DT,[c|DT]),w(10),b.
w :- shift(0).
w(0) :- w.
w(N) :- w,N1 is N - 1, w(N1).
f :- a,w,b,b,w,a,c.
display :- bb_get(dt,DT),writeln(DT).
run(0) :- writeln(end).
run(C) :- bb_put(dt,[]),reset(C,_,C1),display,run(C1).

:- run(f).
:- halt.
