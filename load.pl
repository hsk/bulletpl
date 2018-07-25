:- use_module(syntax).

expr($rank,$rank).
expr($rand,$rand).
expr(F,F) :- (float(F);integer(F)).
expr($I,$I) :- integer(I).
expr(E1+E2,E1_+E2_) :- expr(E1,E1_),expr(E2,E2_).
expr(E1-E2,E1_-E2_) :- expr(E1,E1_),expr(E2,E2_).
expr(-E2,-E2_) :- expr(E2,E2_).
expr(E1*E2,E1_*E2_) :- expr(E1,E1_),expr(E2,E2_).
expr(E1 / E2,E1_ / E2_) :- expr(E1,E1_),expr(E2,E2_).
expr(E1 mod E2,E1_ mod E2_) :- expr(E1,E1_),expr(E2,E2_).
expr(A,R) :- atom(A),
  re_replace('\\$'/g,' $',A,A1),
  re_replace('%'/g,' mod ',A1,A2),
  re_replace('-'/g,' - ',A2,A3),
  atom_string(A4,A3),
  atom_to_term(A4,T,[]),!,
  expr(T,R).

speedType(absolute,E,spdAbs(E)).
speedType(relative,E,spdRel(E)).
speedType(sequence,E,spdSeq(E)).
speedType(none,E,spdSeq(E)).
speed([element(speed,[type=Type],[E])|A],(R,A)) :- !,speedType(Type,E_,R),expr(E,E_).
speed([element(speed,[],[E])|A],(R,A)) :- expr(E,E_),speedType(none,E_,R).
speed(A,(spdAbs(1),A)).

direction([element(direction,[type=absolute],[E])|A],(dirAbs(E_),A)) :- expr(E,E_).
direction([element(direction,[type=sequence],[E])|A],(dirSeq(E_),A)) :- expr(E,E_).
direction([element(direction,[type=aim],[E])|A],(dirAim(E_),A)) :- expr(E,E_).
direction([element(direction,[type=relative],[E])|A],(dirRel(E_),A)) :- expr(E,E_).
direction(A,(dirAim(0),A)).

horizontal([element(horizontal,[type=T],[E])|A],R,A) :- expr(E,E_),speedType(T,E_,R).
horizontal([element(horizontal,_,[E])|A],R,A) :- expr(E,E_),speedType(none,E_,R).
horizontal(A,spdRel(0),A).

vertical([element(vertical,[type=T],[E])|A],R,A) :- expr(E,E_),speedType(T,E_,R).
vertical([element(vertical,_,[E])|A],R,A) :- expr(E,E_),speedType(none,E_,R).
vertical(A,spdRel(0),A).

action1(element(repeat,[],[element(times,[],[E])|A]),repeat(E_,R)) :-
  expr(E,E_), maplist(action1,A,R).
action1(A,R) :- firep(A,R).
action1(element(changeSpeed,[],A),changeSpeed(Sp,E_)) :-
  speed(A,(Sp,[element(term,[],[E])])),expr(E,E_).
action1(element(changeDirection,[],A),changeDirection(D_,E_)) :- !,
  direction(A,(D_,[element(term,[],[E])])),expr(E,E_).
action1(element(accel,[],A),accel(H,V,E_)) :-
  horizontal(A,H,A1),
  vertical(A1,V,[element(term,[],[E])]),expr(E,E_).
action1(element(wait,[],[E]),wait(E_)) :- expr(E,E_).
action1(element(vanish,[],[]),vanish).
action1(element(action,_,A),action(R)) :- maplist(action1,A,R).
action1(element(actionRef,[label=Label],A),actionRef(Label,Ps)) :- params(A,Ps).
action1(A,_) :- writeln(error:action1:A),throw(error).

params(Ps,Ps_) :- maplist(param,Ps,Ps_).
param(element(param,[],[E]),E_) :- expr(E,E_).

bullet1(element(bullet,_,A),bullet(D,S,A4)) :-
  direction(A,(D,A1)),speed(A1,(S,A2)),maplist(action1,A2,A4).
bullet1(element(bulletRef,[label=L],A),bulletRef(L,Ps)) :-
  params(A,Ps).
bullet1(E,_) :- writeln(bullet:ng:E),throw(error).
bulletp(E,R) :- bullet1(E,R).
bulletp(element(bulletRef,[label=Label],A),bulletRef(Label,Ps)) :- params(A,Ps).

firep(element(fire,_,A),fire(D,S,R)) :-
  (direction(A,(D,A1)),speed(A1,(S,[A2])),bulletp(A2,R)
  ;writeln(fire:ng),throw(error)).
firep(element(fireRef,[label=L],A),fireRef(L,Ps)) :- params(A,Ps).
orientation(Attr,T) :- member(type=T,Attr),!.
orientation(_,none).

elem(element(action,[label=Label],A),Label:action(A_)) :- !,
  atom(Label),maplist(action1,A,A_).
elem(element(bullet,[label=Label],A),Label:A_) :- !,
  atom(Label),bullet1(element(bullet,[],A),A_).
elem(element(fire,[label=Label],A),Label:A_) :-
  atom(Label),!,firep(element(fire,[],A),A_).
elem(E,_) :- writeln(error:elem:E),throw(error).
bulletML([element(bulletml,Attr,Elems)], bulletML(Ori_,Elems_)) :- orientation(Attr,Ori_),maplist(elem,Elems,Elems_).

spaceN(N,R2):-findall('  ',between(1,N,_),R),concat_atom(R,R2).
pprint(FP,T) :- tell(FP),pprint(T),tell(user_output).
pprint(T) :- pprint1(0,T),writeln(.).
pprint1(_,T) :- atomic(T),print(T).
pprint1(N,T) :- format(atom(V),'~p',[T]),atom_length(V,N1),N2 is N1+N,N2 < 80,write(V).
pprint1(N,T) :- is_list(T), writeln('['),N1 is N+1,pplist(N1,T),printSpace(N),write(']').
pprint1(N,A:B) :- writeq(A),write(:),pprint1(N,B).
pprint1(N,A) :- A =..[P|Ps], write(P),write('('),ppparams(N,Ps),write(')').
ppparams(_,[]).
ppparams(N,[A]):-pprint1(N,A).
ppparams(N,[A|As]) :- pprint1(N,A),write(,),ppparams(N,As).
printSpace(N) :- spaceN(N,R),write(R).
pplist(_,[]).
pplist(N,[A]):- printSpace(N),pprint1(N,A),nl.
pplist(N,[A|As]) :- printSpace(N),pprint1(N,A),writeln(,),pplist(N,As).

check(F,File) :-
  writeln(----),
  writeln(File),
  load_xml(File, Term, []),
  % writeln(Term),
  catch((
    call(F,Term,R),!,
    %writeln(R),
    pprint(R),
    (syntax:t(R),!;writeln(error:convert_syntax),halt),
    re_replace('examples','bulletpl',File,File2),
    writeln(File2),
    open(File2,write,FP),
    %!,print_term(R,[indent_arguments(4),right_margin(8000),tab_width(0),output(FP)]),write(FP,'.\n'),
    pprint(FP,R),
    close(FP),
    writeln(File:ok),!; halt),
  Err,(
    writeln(File:Err;ng),halt
  )).

:- check(bulletML,'examples/mini/aim.xml').
:- check(bulletML,'examples/mini/aim.1.xml').
:- check(bulletML,'examples/[Dodonpachi]_hibachi.xml').
:- check(bulletML,'examples/[MDA]_circular_sun.xml').
:- check(bulletML,'examples/[1943]_rolling_fire.xml').
:- check(bulletML,'examples/[Dodonpachi]_kitiku_1.xml').

%:- halt.

:- directory_files('examples/',Files),
  include(re_match(".xml$"/i),Files,Xmls),
  maplist(re_replace('^','examples/'),Xmls,Xmls2),
  maplist(check(bulletML),Xmls2),
  length(Xmls2,L),
  writeln(L:ok).

:- halt.
