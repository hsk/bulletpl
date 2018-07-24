initWindow :-
  new(@w, dialog(game)),
  send(@w, size, size(430,430)),
  send(@w, open).
initShip :-
  send(@w, display, new(@area, box(10000,10000))),
  send(@w, display, new(@ship, box(20,20))),
  send(@area, recogniser,move_gesture(left)),
  send(@area, center, point(200,350)),
  send(@ship, fill_pattern, colour(blue)).
moveShip :- 
  get(@area,center,point(PX,PY)),
  X is max(15,min(415,PX)),Y is max(15,min(415,PY)),
  send(@area,center,point(X,Y)),send(@ship,center,point(X,Y)).
getShip(ship{x:X,y:Y}) :- get(@area,center,point(X,Y)).

M.del(K) := M2 :- del_dict(K,M,_,M2);M=M2.
dir(_,dirAbs(D),_,D).
spd(_,spdAbs(S),_,S).
chgDir(B,Ship,D,B1) :- (OD,ND,C,M)=B.get(chgDir),
  dir(B,ND,Ship,ND1), D is OD*(1-C/M)+ND1*C/M,
  (C = M ->  B1 = B.del(chgDir).put(dir,ND)
  ;C1 is C+1,B1 = B.put(chgDir,(OD,ND,C1,M))).
chgDir(B,Ship,D,B) :- dir(B,B.dir,Ship,D).
chgSpd(B,Ship,S,B1) :- (OS,NS,C,M)=B.get(chgSpd),
  spd(B,NS,Ship,NS1),S is OS*(1-C/M)+NS1*C/M,
  (C = M  -> B1 = B.del(chgSpd).put(spd,NS)
  ;C1 is C+1,B1 = B.put(chgSpd,(OS,NS,C1,M))).
chgSpd(B,Ship,S,B) :- spd(B,B.spd,Ship,S).

action(As) :- maplist(call,As).
wait(0) :- !.
wait(N) :-
  getShip(Ship),retract(bullet(B)),
  chgDir(B,Ship,D,B1),chgSpd(B1,Ship,S,B2),
  Y is B.y + cos(D)*S,X is B.x - sin(D)*S,
  asserta(bullet(B2.put([x:X,y:Y,pdir:D,pspd:S]))),
  shift(N), N1 is N - 1, wait(N1).
fire(D,S,As) :-
  bullet(B),getShip(Ship),spd(B,S,Ship,S_),dir(B,D,Ship,D_),newBullet(B.x,B.y,B2),
  assertz(bullet(B2.put([dir:D,spd:S,pdir:D_,pspd:S_,cont:action(As)]))).
changeDirection(D,T) :- retract(bullet(B)),asserta(bullet(B.put(chgDir,(B.pdir,D,0,T)))).
changeSpeed(S,T) :- retract(bullet(B)),asserta(bullet(B.put(chgSpd,(B.pspd,S,0,T)))).
repeat(0,_) :- !.
repeat(N,As) :- action(As),N1 is N - 1,repeat(N1,As).

runBullet(B) :-
  asserta(bullet(B)),reset(B.cont,_,Cont),retract(bullet(B1)),
  ( Cont=0, send(B1.shape,destroy); assertz(bullet(B1.put(cont,Cont)))).
move :-
  moveShip,
  findall(B,retract(bullet(B)),Bs),!,
  maplist(runBullet,Bs),!,display,move.

display :-
  findall(B,bullet(B),Bs),maplist(dispBullet,Bs),send(@w,flush),
  get_time(Time),retract(time1(OTime)),assertz(time1(Time)),
  W is 0.01-(Time-OTime),sleep(W).
dispBullet(B) :- send(B.shape,move,point(B.x,B.y)).

newBullet(X,Y,bullet{shape:Shape,x:X,y:Y}) :-
   send(@w, display, new(Shape, box(10,10)), point(X,Y)),
   send(Shape, pen, 0),
   send(Shape, fill_pattern, colour(red)).

:-
  initWindow,initShip,newBullet(200,50,B),
  get_time(Time),assertz(time1(Time)),
  assertz(bullet(B.put([dir:dirAbs(0),spd:spdAbs(0),cont:repeat(1000,[
    repeat(30,[
      fire(dirAbs(3.14159/2),spdAbs(1),[
        changeSpeed(spdAbs(4),30),wait(40),
        changeDirection(dirAbs(0),30),wait(80),
        changeSpeed(spdAbs(8),30),
        changeDirection(dirAbs(-2),30),wait(80)
      ]),
      fire(dirAbs(-3.14159/2),spdAbs(1),[
        changeSpeed(spdAbs(4),30),wait(40),
        changeDirection(dirAbs(0),30),wait(80),
        changeSpeed(spdAbs(8),30),
        changeDirection(dirAbs(2),30),wait(80)
      ]),
      wait(5)
    ]),
    wait(200)
  ])]))),!,
  catch(move,_,true).
:- halt.
