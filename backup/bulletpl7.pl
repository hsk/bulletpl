:- use_module(library(pce)).

ratio(1.5).

:- pce_begin_class(canvas, graphical,"canvas").
  initialise(S,W:width=int, H:height=int) :->
    send(S, send_super, initialise, 0, 0, W, H).
  '_redraw_area'(S, _A:area) :->
    send(S, save_graphics_state),
    ratio(Ratio),
    objs(R),
    getShip(ship{x:X,y:Y}),
    send(S, graphics_state, 0, none, colour(c,200*255,200*255,200*255)),
    maplist(view2(Ratio,S),[(X,Y,blue)|R]),
    send(S, graphics_state, 0, none, colour(c2,186*255,186*255,186*255)),
    maplist(view3(Ratio,S),[(X,Y,blue)|R]),
    maplist(view(Ratio,S),[(X,Y,blue)|R]),
    send(S, restore_graphics_state),
    send(S, send_super, redraw).
:- pce_end_class.
:- assert(objs([])).

view(Ratio,S,(X,Y,C)) :-
  send(S, graphics_state, 0, none, C),
  send(S, draw_fill, X*Ratio-4, Y*Ratio-5, 9, 9).
view2(Ratio,S,(X,Y,_)) :-
  send(S, draw_fill, X*Ratio-7, Y*Ratio+15, 15, 15).
view3(Ratio,S,(X,Y,_)) :-
  send(S, draw_fill, X*Ratio-4, Y*Ratio+15+3, 9, 9).
initWindow :-
  ratio(Ratio),
  new(@w, dialog(game)),
  send(@w, size, size(300*Ratio,400*Ratio)),
  send(@w,display,new(@view,canvas(300*Ratio,400*Ratio))),
  send(@w,display,new(@text,text('game'))),
  send(@w, open),
  send(@w,move,point(0,0)).
initShip :-
  ratio(Ratio),
  send(@w, display, new(@area, box(10000,10000))),
  send(@w, display, new(@ship, box(10,10))),
  send(@area, recogniser,move_gesture(left)),
  send(@area, center, point(150*Ratio,350*Ratio)),
  send(@ship, fill_pattern, colour(blue)),
  send(@ship, pen, 0).
moveShip :- 
  ratio(Ratio),
  get(@area,center,point(PX,PY)),
  X is max(15,min(300*Ratio-15,PX)),Y is max(15,min(400*Ratio-15,PY)),
  send(@area,center,point(X,Y)),send(@ship,center,point(X,Y)).
getShip(ship{x:X_,y:Y_}) :- ratio(Ratio),get(@area,center,point(X,Y)),X_ is X/Ratio,Y_ is Y/Ratio.

:- op(800,xfx,evalto).

%evalto(_,V) :- writeln(evalto;V),fail.
evalto(F,F) :- float(F),!.
evalto(I,I) :- integer(I),!.
evalto(A,A) :- atom(A),!.
evalto(F,$rand) :- !,random(0.0,1.0,F).
evalto(I,$rank) :- !,rank(I).
evalto(V,E1+E2) :- !,evalto(E1_,E1),evalto(E2_,E2), V is E1_+E2_.
evalto(V,E1-E2) :- !,evalto(E1_,E1),evalto(E2_,E2), V is E1_-E2_.
evalto(V,E1*E2) :- !,evalto(E1_,E1),evalto(E2_,E2), V is E1_*E2_.
evalto(V,E1/E2) :- !,evalto(E1_,E1),evalto(E2_,E2), V is E1_/E2_.
evalto(V,atan(E)) :- !,evalto(E_,E), V is atan(E_).
evalto(V,-E) :- !,evalto(E_,E), V is -E_.
evalto(E1_:E2_,E1:E2) :- !,evalto(E1_,E1),!,evalto(E2_,E2). 
evalto(_,E) :- writeln(error:evalto(E)),halt.
M.del(K) := M2 :- del_dict(K,M,_,M2);M=M2.
dir(_,dirAbs(D),_,D_) :- D_ evalto D.
dir(B,dirSeq(D),_,D_) :- D_ evalto B.get(fdir) + D.
dir(B,dirSeq(D),Ship,D_) :- dir(B,dirAim(D),Ship,D_).
dir(B,dirRel(D),Ship,D_) :- dir(B,B.dir,Ship,D1), D_ evalto D1 + D.
dir(B,dirAim(D),Ship,D_) :- Ship.y =:= B.y, Ship.x > B.x, D_ evalto D + 90,!.
dir(B,dirAim(D),Ship,D_) :- Ship.y =:= B.y,               D_ evalto D - 90,!.
dir(B,dirAim(D),Ship,D_) :- Ship.y > B.y,!, D_ evalto atan((B.x - Ship.x) / (Ship.y - B.y))*180/3.141592 + 180+ D.
dir(B,dirAim(D),Ship,D_) :-                 D_ evalto atan((B.x - Ship.x) / (Ship.y - B.y))*180/3.141592 + D.
spd(_,spdAbs(S),_,S_) :- S_ evalto S.
spd(B,spdSeq(S),_,S_) :- S_ evalto B.get(fspd) + S.
spd(B,spdSeq(D),Ship,D_) :- spd(B,spdAbs(D),Ship,D_).
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
refmap(G,P,N,N1) :- N1 is N+1,P_ evalto P,member(N:P_,G),!.
refmap(_,P,N,N1) :- N1 is N+1,writeln(waring:parameter($N:P)).
actionRef(K,Ps) :- actionV(K,G,As),foldl(refmap(G),Ps,1,_),!,action(As).
action(As) :- maplist(call,As).
cont(B,_) :- (B.x < 0; B.y < 0; B.x > 300; B.y > 400),asserta(bullet(B)),shift(1). % 画面外で消える
cont(B,N) :- asserta(bullet(B)),shift(0),N1 evalto N - 1, wait(N1).
wait(N) :- N1 evalto N, N1 =< 0, !.
wait(N) :-
  getShip(Ship),retract(bullet(B)),
  chgDir(B,Ship,D,B1),chgSpd(B1,Ship,S,B2),
  D_ is D/180*3.14159,
  X is B.x + sin(D_)*S,Y is B.y - cos(D_)*S,
  cont(B2.put([x:X,y:Y,pdir:D,pspd:S]),N).
fireRef(K,Ps):-
  fireV(K,G,D,S,As),foldl(refmap(G),Ps,1,_),fire(D,S,As).
fire(D,S,bulletRef(K,Ps)) :-
  bulletV(K,G,D_,S_,As),foldl(refmap(G),Ps,1,_),fire1(D,S,bullet(D_,S_,As),K).
fire(D,S,B) :- fire1(D,S,B,normal).
selectParam(none,none,Default,Default).
selectParam(none,BulletP,_,BulletP).
selectParam(FireP,_,_,FireP).
fire1(FD,FS,bullet(BD,BS,As),K) :-
  selectParam(FD,BD,dirAim(0),D),selectParam(FS,BS,spdAbs(1),S),
  bullet(B),getShip(Ship),
  spd(B,S,Ship,S_),!,
  dir(B,D,Ship,D_),!,newBullet(B.x,B.y,B2,K),
  retract(bullet(B1)),asserta(bullet(B1.put([fdir:D_,fspd:S_]))),
  assert(bullet(B2.put([dir:dirAbs(D_),spd:spdAbs(S_),pdir:D_,pspd:S_,cont:action(As)]))).
getPDir(B,PDir) :- PDir=B.get(pdir).
getPDir(B,PDir) :- getShip(Ship),dir(B,B.dir,Ship,PDir).
getPSpd(B,PSpd) :- PSpd=B.get(pspd).
getPSpd(B,PSpd) :- getShip(Ship),spd(B,B.spd,Ship,PSpd).
evalDir(dirAbs(D),dirAbs(D_)) :- D_ evalto D. % todo $rand
evalDir(dirAim(D),dirAim(D_)) :- D_ evalto D.
evalDir(dirSeq(D),dirSeq(D_)) :- D_ evalto D.
evalDir(dirRel(D),dirRel(D_)) :- D_ evalto D.
changeDirection(D,T) :- evalDir(D,D_),retract(bullet(B)),getPDir(B,PDir),asserta(bullet(B.put(chgDir,(PDir,D_,0,T)))).
evalSpd(spdAbs(S),spdAbs(S_)) :- S_ evalto S. % todo $rand
evalSpd(spdSeq(S),spdSeq(S_)) :- S_ evalto S.
evalSpd(spdRel(S),spdRel(S_)) :- S_ evalto S.
evalSpd(S,_) :- writeln(error(evalSpd(S))),halt.
changeSpeed(S,T) :- evalSpd(S,S_),retract(bullet(B)),getPSpd(B,PSpd),asserta(bullet(B.put(chgSpd,(PSpd,S_,0,T)))).
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
  reverse(Bs1,Bs1_),
  findall(B,retract(bullet(B)),Bs2),
  append(Bs1_,Bs2,Bs3),
  get_time(Time),retract(time1(OTime)),assertz(time1(Time)),
  W is 0.0125-(Time-OTime),sleep(W),
  findall(R,obj(R),Rs),assertz(objs(Rs)),retractall(obj(_)),!,
  send(@w,flush),!,
  retract(objs(_)),!,
  move(Bs3).

dispBullet(B) :- assert(obj((B.x,B.y,B.shape))).
freeBullet(_).
newBullet(X,Y,bullet{shape:C,x:X,y:Y},K) :- color(K,C).
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
setRank(N) :- V evalto N, retractall(rank(_)),asserta(rank(V)).
rankUp :- retract(rank(N)),N1 is N+1,asserta(rank(N1)).
:- initWindow,initShip.
:- use_module(syntax,[]).
setColor(K) :- retract(color(C)),asserta(color(K,C)).
setColor(K) :- asserta(color(K,red)).
run(bulletML(Mode,Ds)) :-
  assert(color(a)),assert(color(a,a)),
  retractall(color(_)),retractall(color(_,_)),
  maplist(assert,[color(green), color(blue), color(red), color(yellow), color(black)]),
  asserta(color(normal,white)),asserta(color(top,red)),
  (syntax:t(bulletML(Mode,Ds)),!; writeln(syntax:error),halt),!,
  writeln(syntax:ok),
  setRank(1),
  get_time(Time),assertz(time1(Time)),
  setDefs(Ds),
  newBullet(150,100,B,top),
  member(A:action(As),Ds),re_match('^top',A),writeln(A),!,
  move([B.put([dir:dirAbs(0),spd:spdAbs(0),cont:action(As)])]).
runfile(F) :-
  text(read:F),
  read_file_to_terms(F,[BML],[]),
  writeln(run),!,run(BML).

:- current_prolog_flag(argv, ARGV),maplist(runfile,ARGV).
:- halt.
