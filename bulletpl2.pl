:- expects_dialect(sicstus).
initShip :-
  Ship = ship{x:200,y:350},
  bb_put(ship,Ship),!,
  send(@w, display, new(@area, box(10000,10000))),
  send(@w, display, new(@ship, box(20,20))),
  send(@area, recogniser,move_gesture(left)),
  send(@area, center, point(Ship.x,Ship.y)),
  send(@ship, fill_pattern, colour(blue)).
moveShip :- 
  get(@area,center,point(PX,PY)),
  X is max(15,min(415,PX)),
  Y is max(15,min(415,PY)),
  send(@area,center,point(X,Y)),
  send(@ship,center,point(X,Y)),
  bb_put(ship,ship{x:X,y:Y}).

initWindow :- new(@w, dialog(game)),
   send(@w, size, size(430,430)),
   send(@w, open).

M.del(K) := M2 :- del_dict(K,M,_,M2);M=M2.
%:- R=a{}.del(a),writeln(R). :- halt.
dir(_,dirAbs(D),_,D).
dir(B,dirSeq(D),_,D_) :- D_ is B.get(pdir)+D.
dir(_,dirSeq(D),_,D). % :- D_ is D.
dir(B,dirAim(D),Ship,D_) :- Ship.x =:= B.x, Ship.y > B.y, D_ is  3.141592 / 2.0 + D,!.
dir(B,dirAim(D),Ship,D_) :- Ship.x =:= B.x,               D_ is -3.141592 / 2.0 + D,!.
dir(B,dirAim(D),Ship,D_) :- D_ is atan(float(Ship.y - B.y) / float(-Ship.x + B.x)) + D.
spd(_,spdAbs(S),_,S).
spd(B,spdSeq(S),_,S_) :- S_ is B.get(pspd)+S.
spd(_,spdSeq(S),_,S_) :- S_ is S.

chgDir(B,Ship,D,B1) :- (OD,ND,C,M)=B.get(chgDir),!,
  dir(B,ND,Ship,ND1),
  D is OD*(1-C/M)+ND1*C/M,
  (C=M->B1=B.del(chgDir).put(dir,ND);C1 is C + 1,B1 = B.put(chgDir,(OD,ND,C1,M))).
chgDir(B,Ship,D,B) :- dir(B,B.dir,Ship,D).
chgSpd(B,Ship,S,B1) :- (OS,NS,C,M)=B.get(chgSpd),!,
  spd(B,NS,Ship,NS1),
  S is OS*(1-C/M)+NS1*C/M,
  (C=M->B1=B.del(chgSpd).put(spd,NS);C1 is C + 1,B1 = B.put(chgSpd,(OS,NS,C1,M))).
chgSpd(B,Ship,S,B) :- spd(B,B.spd,Ship,S).

moveDefault :-
  bb_get(ship,Ship),bb_get(bullet,B),
  chgDir(B,Ship,D,B1),chgSpd(B1,Ship,S,B2),
  Y is float(B.y + cos(D)*S),X is float(B.x - sin(D)*S),
  bb_put(bullet,B2.put(x,X).put(y,Y).put(pdir,D).put(pspd,S)).

%moveBullet(A) :- writeln(moveBullet(A)),fail.
moveBullet(action(As)) :- maplist(moveBullet,As).
moveBullet(wait(0)) :- !.
moveBullet(wait(N)) :- moveDefault,shift(N), N1 is N - 1, moveBullet(wait(N1)).
moveBullet(fire(bullet(D,S,As))) :-
  bb_get(ship,Ship),
  bb_get(bullet,B),
  newBullet(B.x,B.y,B2),
  spd(B2,S,Ship,S_),
  dir(B2,D,Ship,D_),
  bb_update(bullets,Bs,[B2.put([dir:D,spd:S,pdir:D_,pspd:S_,cont:moveBullet(action(As))])|Bs]).
moveBullet(changeDirection(D,T)) :- bb_update(bullet,B,B.put(chgDir,(B.pdir,D,0,T))).
moveBullet(changeSpeed(S,T)) :- bb_update(bullet,B,B.put(chgSpd,(B.pspd,S,0,T))).
moveBullet(repeat(0,_)) :- !.
moveBullet(repeat(N,As)) :- moveBullet(action(As)),N1 is N - 1,moveBullet(repeat(N1,As)).
runBullet(B) :-
  bb_put(bullet,B),reset(B.cont,_,Cont),
  ( Cont=0, send(B.shape,destroy)
  ; bb_get(bullet,B1),bb_update(bullets,Bs,[B1.put(cont,Cont)|Bs])).
runBullets :-
  moveShip,
  bb_update(bullets,Bs,[]),!,
  ( Bs=[] -> true
  ; maplist(runBullet,Bs),!,display,runBullets).

display :-
  bb_get(bullets,Bs),
  maplist(drawBullet,Bs),
  send(@w,flush), sleep(0.01),
  true.
drawBullet(B) :-
  send(B.shape,move,point(B.x,B.y)),
  %writeln(B.del(cont).del(chgSpd).del(chgDir)),
  true.

newBullet(X,Y,B) :-
%  writeln(newBullet(X,Y)),
   send(@w, display, new(Shape, box(10,10)), point(X,Y)),
   send(Shape, pen, 0),
   send(Shape, fill_pattern, colour(red)),
   B=bullet{shape:Shape,x:X,y:Y}.

:-
  initWindow,
  initShip,
  newBullet(200,50,B),
  bb_put(bullets,[B.put([dir:dirAbs(0.0),spd:spdAbs(0.0),cont:moveBullet(repeat(1000,[
    repeat(10,[
      fire(bullet(dirAbs(3.14159/2.0),spdAbs(1.0),[
        changeSpeed(spdAbs(4.0),30),wait(40),
        changeDirection(dirAbs(0.0),30),wait(80),
        changeSpeed(spdAbs(8.0),30),
        changeDirection(dirAbs(-2.0),30),wait(160)
      ])),
      fire(bullet(dirAbs(-3.14159/2.0),spdAbs(1),[
        changeSpeed(spdAbs(4.0),30),wait(40),
        changeDirection(dirAbs(0.0),30),wait(80),
        changeSpeed(spdAbs(8.0),30),
        changeDirection(dirAbs(2.0),30),wait(160)
      ])),
      wait(10)
    ]),
    wait(200)
  ]))])]),!,
  runBullets.
:- halt.
