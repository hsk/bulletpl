initWindow :-
  new(@w, dialog(game)),
  send(@w, size, size(430,430)),
  send(@w,display,new(@text,text('game'))),
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

:- op(800,xfx,evalto).

%evalto(_,V) :- writeln(evalto;V),fail.
evalto(F,F) :- float(F),!.
evalto(I,I) :- integer(I),!.
evalto(A,A) :- atom(A),!.
evalto(F,$rand) :- !,random(0.0,1.0,F).
evalto(I,$rank) :- !,rank(I).
evalto(V,$P) :- !,bullet(B),!,member(P=V,B.param),!.
evalto(V,E1+E2) :- !,evalto(E1_,E1),evalto(E2_,E2), V is E1_+E2_.
evalto(V,E1-E2) :- !,evalto(E1_,E1),evalto(E2_,E2), V is E1_-E2_.
evalto(V,E1*E2) :- !,evalto(E1_,E1),evalto(E2_,E2), V is E1_*E2_.
evalto(V,E1/E2) :- !,evalto(E1_,E1),evalto(E2_,E2), V is E1_/E2_.
evalto(V,atan(E)) :- !,evalto(E_,E), V is atan(E_).
evalto(V,-E) :- !,evalto(E_,E), V is -E_.
evalto(E1_:E2_,E1:E2) :- !,evalto(E1_,E1),!,evalto(E2_,E2). 
evalto(_,E) :- writeln(error:evalto(E)),halt.
M.del(K) := M2 :- del_dict(K,M,_,M2);M=M2.
dir(_,dirAbs(D),_,D).
dir(B,dirSeq(D),_,D_) :- D_ evalto B.fdir + D.
dir(B,dirRel(D),Ship,D_) :- dir(B,B.dir,Ship,D1), D_ evalto D1 + D.
dir(B,dirAim(D),Ship,D_) :- Ship.y =:= B.y, Ship.x > B.x, D_ evalto D + 90,!.
dir(B,dirAim(D),Ship,D_) :- Ship.y =:= B.y,               D_ evalto D - 90,!.
dir(B,dirAim(D),Ship,D_) :- Ship.y > B.y,!, D_ evalto atan((B.x - Ship.x) / (Ship.y - B.y))*180/3.141592 + 180+ D.
dir(B,dirAim(D),Ship,D_) :-                 D_ evalto atan((B.x - Ship.x) / (Ship.y - B.y))*180/3.141592 + D.
spd(_,spdAbs(S),_,S).
spd(B,spdSeq(S),_,S_) :- S_ evalto B.fspd + S.
spd(B,spdRel(S),Ship,S_) :- spd(B,B.spd,Ship,S1), S_ evalto S1 + S.
chgDir(B,Ship,D,B1) :- (OD,ND,C,M)=B.get(chgDir),
  dir(B,ND,Ship,ND1), D evalto OD*(1-C/M)+ND1*C/M,
  (C = M ->  B1 = B.del(chgDir).put(dir,ND)
  ;C1 evalto C+1,B1 = B.put(chgDir,(OD,ND,C1,M))).
chgDir(B,Ship,D,B) :- dir(B,B.dir,Ship,D).
chgSpd(B,Ship,S,B1) :- (OS,NS,C,M)=B.get(chgSpd),
  spd(B,NS,Ship,NS1),S evalto OS*(1-C/M)+NS1*C/M,
  (C = M  -> B1 = B.del(chgSpd).put(spd,NS)
  ;C1 evalto C+1,B1 = B.put(chgSpd,(OS,NS,C1,M))).
chgSpd(B,Ship,S,B) :- spd(B,B.spd,Ship,S).

eval1(_,[],Prm,Prm).
eval1(N,[V|Vs],Prm,[N=V_|Vs_]) :- evalto(V_,V),N1 is N+1,eval1(N1,Vs,Prm,Vs_).

getParam(B,Prm) :- Prm=B.get(param),!;Prm=[].
setParams(Ps,B1) :-
  eval1(1,Ps,Prm,Ps_),retract(bullet(B1)),getParam(B1,Prm),asserta(bullet(B1.put(param,Ps_))).
rmParams(B1) :- retract(bullet(B)),!,(B.shape=B1.shape;halt),!,
  getParam(B1,Prm),!,asserta(bullet(B.put(param,Prm))).

actionRef(K,Ps) :- setParams(Ps,B),actionV(K,As),action(As),rmParams(B).
action(As) :- maplist(call,As).
cont(B,_) :- (B.x < 0; B.y < 0; B.x > 430; B.y > 430),asserta(bullet(B)),shift(1). % 画面外で消える
cont(B,N) :- asserta(bullet(B)),shift(0),N1 evalto N - 1, wait(N1).
wait(N) :- N1 evalto N, N1 =< 0, !.
wait(N) :-
  getShip(Ship),retract(bullet(B)),
  chgDir(B,Ship,D,B1),chgSpd(B1,Ship,S,B2),
  D_ is D/180*3.14159,
  X is B.x + sin(D_)*S,Y is B.y - cos(D_)*S,
  cont(B2.put([x:X,y:Y,pdir:D,pspd:S]),N).
fireRef(K,Ps):- setParams(Ps,B),fireV(K,D,S,As),fire(D,S,As),rmParams(B).
fire(D,S,bulletRef(K,Ps)) :- setParams(Ps,B),bulletV(K,As), fire(D,S,As),rmParams(B).
fire(D,S,As) :-
  bullet(B),getShip(Ship),
  spd(B,S,Ship,S_),!,
  dir(B,D,Ship,D_),!,newBullet(B.x,B.y,B2),
  retract(bullet(B1)),asserta(bullet(B1.put([fdir:D_,fspd:S_]))),
  assert(bullet(B2.put([dir:dirAbs(D_),spd:spdAbs(S_),pdir:D_,pspd:S_,cont:action(As)]))).
changeDirection(D,T) :- retract(bullet(B)),asserta(bullet(B.put(chgDir,(B.pdir,D,0,T)))).
changeSpeed(S,T) :- retract(bullet(B)),asserta(bullet(B.put(chgSpd,(B.pspd,S,0,T)))).
action(N,As) :- repeat(N,As).
vanish :- shift(1).
repeat(N,_) :- N1 evalto N, N1 =< 0, !.
repeat(N,As) :- action(As),N1 evalto N - 1,repeat(N1,As).
text(T) :- T2 evalto T,!, format(atom(T3),'~w',[T2]),writeln(T3),send(@text,value,T3).
runBullet(B,Bs1,Bs1_) :-
  asserta(bullet(B.del(cont))),reset(B.cont,R,Cont),retract(bullet(B1)),
  (Cont=0,!,Bs1_=[B1.put(cont,wait(99999))|Bs1],dispBullet(B1)
  ; R= 1,!, freeBullet(B.shape),Bs1_=Bs1
  ; Bs1_=[B1.put(cont,Cont)|Bs1],dispBullet(B1)).
move([]).
move(Bs) :-
  moveShip,foldl(runBullet,Bs,[],Bs1),!,
  findall(B,retract(bullet(B)),Bs2),
  append(Bs1,Bs2,Bs3),
  get_time(Time),retract(time1(OTime)),assertz(time1(Time)),
  W is 0.0125-(Time-OTime),sleep(W),
  send(@w,flush),!,
  move(Bs3).

dispBullet(B) :- send(B.shape,move,point(B.x,B.y)).

freeBullet(Shape) :- send(Shape,destroy),!.
newBullet(X,Y,bullet{shape:Shape,x:X,y:Y}) :-
  send(@w, display, new(Shape, box(10,10)), point(X,Y)),
  send(Shape, pen, 0),
  send(Shape, fill_pattern, colour(red)).

setDef(N:action(As)) :- asserta(actionV(N,As)).
setDef(N:action(I,As)) :- asserta(actionV(N,[repeat(I,As)])).
setDef(N:bullet(As)) :- asserta(bulletV(N,As)).
setDef(N:fire(D,S,As)) :- asserta(fireV(N,D,S,As)).
setDefs(Ds) :-
  retractall(actionV(_,_)),retractall(bulletV(_,_)),retractall(fireV(_,_,_,_)),
  maplist(setDef,Ds).

run(bulletml(Ds)) :-
  setRank(1),
  get_time(Time),assertz(time1(Time)),
  member(top:Action,Ds),
  setDefs(Ds),
  newBullet(200,50,B),move([B.put([dir:dirAbs(0),spd:spdAbs(0),cont:Action])]).
setRank(N) :- V evalto N, retractall(rank(_)),asserta(rank(V)).
rankUp :- retract(rank(N)),N1 is N+1,asserta(rank(N1)).
:- initWindow,initShip.

:- text('ランダム分裂弾'),
  run(bulletml([top:action([
    action(6,[
        wait(100),
        action(($rank * 0.5) + 5,[
          wait(3),
          fire(dirAim($rand*100-50),spdAbs(1),[])
        ]),
        rankUp,
        text(rank:($rank))
    ]),
    vanish
  ])])).

:- text('dirAbs 上、右、下、左に飛ぶ'),
  run(bulletml([top:action([
    action(3,[
      wait(30),
      fire(dirAbs(0),spdAbs(1.0),[wait(500)]),
      wait(30),
      fire(dirAbs(90),spdAbs(1.0),[wait(500)]),
      wait(30),
      fire(dirAbs(180),spdAbs(1.0),[wait(500)]),
      wait(30),
      fire(dirAbs(270),spdAbs(1.0),[wait(500)]),
      wait(30)
    ]),
    vanish
  ])])).

:- text('dirAim4 自機方向にあわせて４方向'),
  run(bulletml([top:action([
    action(3,[
      wait(20),
      fire(dirAim(0),spdAbs(1.0),[wait(500)]),
      wait(20),
      fire(dirAim(90),spdAbs(1.0),[wait(500)]),
      wait(20),
      fire(dirAim(180),spdAbs(1.0),[wait(500)]),
      wait(20),
      fire(dirAim(270),spdAbs(1.0),[wait(500)]),
      wait(20)
    ]),
    vanish
  ])])).

:- text('dirSeq 8方向にとぶはず'),
  run(bulletml([top:action([
    action(3,[
      wait(400),
      fire(dirAim(0),spdAbs(1.0),[wait(400)]),
      repeat(7,[
        fire(dirSeq(45),spdAbs(1),[wait(400)])
      ])
    ]),
    vanish
  ])])).

:- text('dirSeq 3way'),
  run(bulletml([top:action([
    action(3,[
      wait(100),
      fire(dirAim(-1*10),spdAbs(1.0),[]),
      repeat(2,[
        fire(dirSeq(10),spdAbs(1),[])
      ])
    ]),
    vanish
  ])])).

:- text('dirSeq 回転弾'),
  run(bulletml([top:action([
    wait(100),
    fire(dirAim(-1*10),spdAbs(1.0),[]),
    repeat(72,[
      wait(4),
      fire(dirSeq(10),spdAbs(1),[])
    ]),
    wait(500),
    vanish
  ])])).

:- text('fire:spdSeq 自機方向にスピードかえて３発'),
  run(bulletml([top:action([
    action(3,[
      wait(100),
      fire(dirAim(0),spdAbs(1.0),[]),
      action(2,[
        fire(dirAim(0),spdSeq(0.1),[])
      ]),
      wait(200)
    ]),
    vanish
  ])])).

:- text('fire:spdRel 自機方向にスピードかえて３発'),
  run(bulletml([top:action([
    action(3,[
      wait(100),
      fire(dirAim(0),spdAbs(1.0),[
        fire(dirAim(0),spdRel(0.1),[]),
        fire(dirAim(0),spdRel(0.2),[])
      ]),
      wait(200)
    ]),
    vanish
  ])])).

:- text('fire:dirRel 自機方向に撃った弾が途中で分裂'),
  run(bulletml([top:action([
    action(6,[
      wait(100),
      fire(dirAim(0),spdAbs(0.5),[
        wait(150),
        fire(dirAim(10),spdRel(0.5),[wait(500)]),
        fire(dirAim(-10),spdRel(0.5),[wait(500)]),
        vanish
      ]),
      wait(200)
    ]),
    vanish
  ])])).

:- text('changeSpeed changeDirection abs 画面端をぐるっと回って後ろから狙う'),
  run(bulletml([top:action([
      wait(200),
      repeat(50,[
        fire(dirAbs(90),spdAbs(1),[
          changeSpeed(spdAbs(4),30),wait(40),
          changeDirection(dirAbs(180),30),wait(80),
          changeSpeed(spdAbs(8),30),
          changeDirection(dirAbs(360-70),30),wait(80)
        ]),
        fire(dirAbs(-90),spdAbs(1),[
          changeSpeed(spdAbs(4),30),wait(40),
          changeDirection(dirAbs(-180),30),wait(80),
          changeSpeed(spdAbs(8),30),
          changeDirection(dirAbs(-360+70),30),wait(80)
        ]),
        wait(5)
      ]),
      vanish
    ])])).

:- text('NWay actionRef fireRef'),
  run(bulletml([
    top:action([
      actionRef(a2,[]),
      vanish
    ]),
    a2:action(12,[
      wait(100),
      actionRef(a3,[$rank,30]),
      rankUp,
      text(rank:($rank))
    ]),
    a3:action([
      wait(3),
      fire(dirAim(- $2/2* ($1-1)),spdAbs(1),[]),
      repeat($1-1,[
        fireRef(f1,[30])
      ])
    ]),
    f1:fire(dirSeq($1),spdAbs(1),[])
  ])).

:- halt.
