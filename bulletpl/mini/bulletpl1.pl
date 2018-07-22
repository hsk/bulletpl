:- expects_dialect(sicstus).
M.del(K) := M2 :- del_dict(K,M,_,M2);M=M2.
%:- R=a{}.del(a),writeln(R). :- halt.
dir(_,dirAbs(D),_,D).
dir(B,dirSeq(D),_,D_) :- D_ is B.pdir+D.
dir(B,dirAim(D),Ship,D_) :- Ship.y = B.y, Ship.x > B.x, D_ is  3.141592 / 2.0 + D.
dir(B,dirAim(D),Ship,D_) :- Ship.y = B.y,               D_ is -3.141592 / 2.0 + D.
dir(B,dirAim(D),Ship,D_) :- D_ is atan(Ship.x - B.x / Ship.y - B.y) + D.
spd(_,spdAbs(S),_,S).
spd(B,spdSeq(S),_,S_) :- S_ is B.pdir+S.

chgDir(B,Ship,D,B1) :- (_,_,M,M)=B.get(chgDir),!,
  dir(B,B.dir,Ship,D),B1=B.del(chgDir).
chgDir(B,Ship,D,B1) :- (OD,ND,C,M)=B.get(chgDir),!,
  dir(B,OD,Ship,OD1),dir(B,ND,Ship,ND1),
  C1 is C + 1,D is OD1*(1-C1/M)+ND1*C1/M,
  B1 = B.put(chgDir,(OD,ND,C1,M)).
chgDir(B,Ship,D,B) :- dir(B,B.dir,Ship,D).
chgSpd(B,Ship,S,B1) :- (_,_,M,M)=B.get(chgSpd),!,
  spd(B,B.spd,Ship,S),B1=B.del(chgSpd).
chgSpd(B,Ship,S,B1) :- (OS,NS,C,M)=B.get(chgSpd),!,
  spd(B,OS,Ship,OS1),spd(B,NS,Ship,NS1),
  C1 is C + 1,S is OS1*(1-C1/M)+NS1*C1/M,
  B1 = B.put(chgSpd,(OS,NS,C1,M)).
chgSpd(B,Ship,S,B) :- spd(B,B.spd,Ship,S).

moveDefault :-
  bb_get(ship,Ship),bb_get(bullet,B),
  chgDir(B,Ship,D,B1),chgSpd(B1,Ship,S,B2),
  Y is B.y + cos(D)*S,X is B.x - sin(D)*S,
  bb_put(bullet,B2.put(x,X).put(y,Y).put(pdir,D).put(pspd,S)).

moveBullet(A) :- writeln(moveBullet(A)),fail.
moveBullet(action(As)) :- maplist(moveBullet,As).
moveBullet(wait(0)) :- !.
moveBullet(wait(N)) :- moveDefault,shift(N), N1 is N - 1, moveBullet(wait(N1)).
moveBullet(fire(bullet(D,S,As))) :-
  bb_get(bullet,B),
  bb_update(bullets,Bs,[bullet{x:B.x,y:B.y,dir:D,spd:S,cont:moveBullet(action(As))}|Bs]).
moveBullet(changeDirection(D,T)) :- bb_update(bullet,B,B.put(chgDir,(B.dir,D,0,T))).
moveBullet(changeSpeed(S,T)) :- bb_update(bullet,B,B.put(chgSpd,(B.spd,S,0,T))).
moveBullet(repeat(0,_)) :- !.
moveBullet(repeat(N,As)) :- moveBullet(action(As)),N1 is N - 1,moveBullet(repeat(N1,As)).
runBullet(B) :-
  bb_put(bullet,B),reset(B.cont,_,Cont),
  (var(Cont),!;Cont=0,writeln(0)
  ;bb_get(bullet,B1),bb_update(bullets,Bs,[B1.put(cont,Cont)|Bs])).
runBullets :-
  bb_update(bullets,Bs,[]),!,
  (Bs=[] -> true
  ;maplist(runBullet,Bs),!,display,runBullets).

display :- bb_get(bullets,Bs),maplist(drawBullet,Bs).
drawBullet(B) :- writeln(B.del(cont).del(chgSpd).del(chgDir)).

:-
  bb_put(ship,ship{x:400,y:400}),!,
  bb_put(bullets,[bullet{x:200,y:200,dir:dirAim(0),spd:spdAbs(0),cont:moveBullet(action([
    fire(bullet(dirAim(0),spdAbs(1),[changeSpeed(spdAbs(2),100),wait(100)])),
    wait(3)
  ]))}]),!,
  runBullets.
:- halt.
