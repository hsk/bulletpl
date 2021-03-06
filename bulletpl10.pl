:- module(bullet10,[runfile/1]).

user:M.del(K) := M2 :- del_dict(K,M,_,M2);M=M2.
refmap(G,P,N,N1) :- N1 is N+1,P_ is P,member(N:P_,G),!.
refmap(_,P,N,N1) :- N1 is N+1,writeln(waring:parameter($N:P)).
refmap(G,Ps,As) :- foldl(refmap(G),Ps,1,_),term_variables(As,Vs),maplist(=(0),Vs).
actionRef(K,Ps) :- actionV(K,G,As),refmap(G,Ps,As),!,action(As).
action(As) :- maplist(call,As).
move1(B,B4) :-cd(B,D,B1),cs(B1,S,B2),acc(B2,B3),
  D_ is D/180*3.14159,mx(B3,X1,Y1),
  X is X1 + sin(D_)*S,Y is Y1 - cos(D_)*S,
  B4=B3.put([x:X,y:Y,d:D,s:S]).
wait(N) :- N1 is N, N1 < 1, !.
wait(N) :- nb_getval(bullet,B),move1(B,B1),nb_linkval(bullet,B1),
          (cont(B1)->shift(0),N1 is N - 1, wait(N1);shift(1)).
cd(B,D,B1) :- (CD,T)=B.get(cd),
  D is B.d+CD,T1 is T - 1,
  (T1 =< 0,_=B.get(isAim) ->
              nb_getval(ship,Ship),d(B,dirAim(D),Ship,D1),
              B1 = B.del(cd).del(isAim).put(d,D1)
  ;T1 =< 0 -> B1 = B.del(cd)
  ;           B1 = B.put(cd,(CD,T1))).
cd(B,B.d,B).
cs(B,S,B1) :- (CS,T)=B.get(cs),
  S is B.s + CS, T1 is T - 1,
  (T1 =< 0 -> B1 = B.del(cs)
  ;           B1 = B.put(cs,(CS,T1))).
cs(B,B.s,B).
mx(B,X,Y) :- X=B.get(mx)+B.x,Y=B.my+B.y;X=B.x,Y=B.y.
my(B,X,Y) :- X=B.get(mx),Y=B.my;X=0,Y=0.
acc(B,B2) :- (_,_,T)=B.get(acc) -> T =< 0,!,B2=B.del(acc);B2=B.
acc(B,B2) :- (H,V,T)=B.get(acc),!,T1 is T - 1,Mx_ is B.mx+H,My_ is B.my+V,
                                  B2 = B.put([acc:(H,V,T1),mx:Mx_,my:My_]).
acc(_,_) :- writeln(acc:error).
cont(B) :- B.x >= 0, B.y >= 0, B.x =< 300, B.y =< 400. % 画面外で消える
fireRef(K,Ps) :- fireV(K,G,D,S,As),refmap(G,Ps,As),fire(D,S,As).
fire(D,S,bulletRef(K,Ps)) :-
  bulletV(K,G,D_,S_,As),refmap(G,Ps,As),fire1(D,S,bullet(D_,S_,As),K).
fire(D,S,B) :- fire1(D,S,B,normal).
selectParam(none,none,Default,Default).
selectParam(FireP,none,_,FireP).
selectParam(_,BulletP,_,BulletP).
fire1(FD,FS,bullet(BD,BS,As),K) :-
  selectParam(FD,BD,dirAim(0),D),selectParam(FS,BS,spdAbs(1),S),
  nb_getval(bullet,B),nb_getval(ship,Ship),
  s(B,S,S_),!,d(B,D,Ship,D_),!,newBullet(B.x,B.y,B2,K),
  nb_getval(bullet,B1),nb_linkval(bullet,B1.put([fdir:D_,fspd:S_])),
  move1(B2.put([d:D_,s:S_,cont:action(As)]),B3),
  (cont(B3),assertz(bullet(B3));true).
d(_,dirAbs(D),_,D_) :- D_ is D.
d(B,dirSeq(D),_,D_) :- D_ is B.get(fdir) + D.
d(B,dirRel(D),_,D_) :- D_ is B.d + D.
d(B,dirSeq(D),Ship,D_) :- d(B,dirAim(D),Ship,D_).
d(B,dirAim(D),Ship,D_) :- D_ is atan((B.x - Ship.x),(Ship.y - B.y))*180/pi - 180 + D.
s(_,spdAbs(S),S_) :- S_ is S*1.68.
s(B,spdSeq(S),S_) :- S_ is B.get(fspd) + S*1.68.
s(_,spdSeq(_),S_) :- S_ is 1.68.
s(B,spdRel(S),S_) :- S_ is B.s + S*1.68.
changeDirection(dirSeq(D),T) :- nb_getval(bullet,B),D_ is D,nb_linkval(bullet,B.put(cd,(D_,T))).
changeDirection(dirAbs(D),T) :- nb_getval(bullet,B),D_ is(D-B.d)/T,nb_linkval(bullet,B.put(cd,(D_,T))).
changeDirection(dirRel(D),T) :- nb_getval(bullet,B),T_ is floor(T),D_ is D/T_,nb_linkval(bullet,B.put(cd,(D_,T_))).
changeDirection(dirAim(D),T) :- nb_getval(bullet,B),nb_getval(ship,Ship),d(B,dirAim(D),Ship,D1),D2 is (D1-B.d),T_ is T,
                                D_ is (((floor(D2) + 180)mod 360)-180)/T_,nb_linkval(bullet,B.put(cd,(D_,T))).
changeSpeed(spdSeq(S),T) :- nb_getval(bullet,B),S_ is S*1.68,nb_linkval(bullet,B.put(cs,(S_,T))).
changeSpeed(spdAbs(S),T) :- nb_getval(bullet,B),S_ is(S*1.68-B.s)/T,nb_linkval(bullet,B.put(cs,(S_,T))).
changeSpeed(spdRel(S),T) :- nb_getval(bullet,B),S_ is S*1.68/T, nb_linkval(bullet,B.put([cs:(S_,T),isAim:0])).
accelspd(spdSeq(S),_,_,S_) :- S_ is S*1.68.
accelspd(spdAbs(S),M,T,S_) :- S_ is (S*1.68-M)/T.
accelspd(spdRel(S),_,T,S_) :- S_ is S*1.68 / T.
accelspd(none,_,_,0).
accel(H,V,T) :- nb_getval(bullet,B),my(B,X,Y),accelspd(H,X,T,HS),accelspd(V,Y,T,VS),nb_linkval(bullet,B.put([mx:X,my:Y,acc:(HS,VS,T)])).

vanish :- shift(1).
repeat(N,_) :- N1 is N, N1 < 1, !.
repeat(N,As) :- action(As),N1 is N - 1,repeat(N1,As).
text(T) :- format(atom(T3),'~w',[T]),format('send:~q\n',[T3]),text_message(T3).
runBullet(B,Bs1,Bs1_) :-
  nb_linkval(bullet,B),
  reset(B.cont,R,Cont),nb_getval(bullet,B1),
  (Cont=0,!,Bs1_=[B1.put(cont,wait(99999))|Bs1]
  ; R= 1,!, Bs1_=Bs1
  ; Bs1_=[B1.put(cont,Cont)|Bs1]).
move([]) :- !.
move(Bs) :-
  nb_getval(move_cnt,N),N1 is N+1,nb_linkval(move_cnt,N1),!,
  (N1 > 600 -> true;
  foldl(runBullet,Bs,[],Bs1),!,
  reverse(Bs1,Bs1_),
  findall(B,retract(bullet(B)),Bs2),
  append(Bs1_,Bs2,Bs3),
  disp_bullet(Bs3),!,move(Bs3)).
newBullet(X,Y,b{c:C,x:X,y:Y},K) :- color(K,C).
replaceParams(G,$I,T,G) :- member(I:T,G),!.
replaceParams(G,$I,T,[I:T|G]) :- integer(I),!.
replaceParams(G,E,E_,G_) :-
  E=..[N|Ps],
  foldl([P,(Ps1,G1),([P_|Ps1],G1_)]>>replaceParams(G1,P,P_,G1_),Ps,([],G),(Ps_,G_)),
  reverse(Ps_,Ps1),
  E_=..[N|Ps1].

replaceParam(A,A_,G) :- replaceParams([],A,A_,G).
setDef(N:action(As)) :- replaceParam(As,As_,G),asserta(actionV(N,G,As_)).
setDef(N:action(I,As)) :- replaceParam([repeat(I,As)],As_,G),asserta(actionV(N,G,As_)).
setDef(N:bullet(D,S,As)) :- setColor(N),replaceParam(bullet(D,S,As),bullet(D_,S_,As_),G),asserta(bulletV(N,G,D_,S_,As_)).
setDef(N:fire(D,S,B)) :- replaceParam(fire(D,S,B),fire(D_,S_,B_),G),asserta(fireV(N,G,D_,S_,B_)).
setDefs(Ds) :-
  retractall(actionV(_,_)),retractall(bulletV(_,_,_,_)),retractall(fireV(_,_,_,_)),
  maplist(setDef,Ds).
setRank(N) :- V is N, nb_linkval(rank,V).
rankUp :- nb_getval(rank,N),N1 is N+1,nb_linkval(rank,N1).
:- use_module(syntax,[]).
setColor(K) :- nb_getval(color_cnt,C),C1 is C+1,nb_linkval(color_cnt,C1),asserta(color(K,C)).
run(bulletML(Mode,Ds)) :-
  nb_linkval(move_cnt,0),nb_linkval(color_cnt,2),
  retractall(color(_,_)),retractall(bullet(_)),
  asserta(color(normal,0)),asserta(color(top,1)),
  (syntax:t(bulletML(Mode,Ds)),!; writeln(syntax:error),halt),!,
  setRank(0.35),
  repExpr(Ds,Ds_),!,
  setDefs(Ds_),
  findall(B1,
    (member(A:action(As),Ds_),atom_concat(top,_,A),newBullet(150,100,B,top),B1=B.put([d:0,s:0,cont:action(As)])),Bs),
  move(Bs).
runfile(F) :- catch(read_file_to_terms(F,[BML],[]),E,(writeln(error:read(E)),fail)),text(F),!,run(BML).
run_string(S) :- read_term_from_atom(S,T,[]),run(T).
repExpr($rank,Rank) :- nb_getval(rank,Rank).
repExpr($rand,random_float).
repExpr(E,E_) :- E=..[N|Ps],maplist(repExpr,Ps,Ps_),E_=..[N|Ps_].
:- nb_linkval(ship,ship{x:150,y:350}).
