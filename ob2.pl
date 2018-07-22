newEnemy(X,Y,B) :-
   send(@w, display, new(B, circle(30)), point(X,Y)),
   send(B, pen, 0),
   send(B, fill_pattern, colour(red)).
draw(B,X,Y) :- send(B, move, point(X, Y)),shift(0).
moveEnemy(F,P,B) :- N is P,call(F,N,B).
moveEnemy1(400,B) :- moveEnemy2(0,B).
moveEnemy1(N,B) :- draw(B,N,0),moveEnemy(moveEnemy1,N+10,B).
moveEnemy2(400,B) :- moveEnemy3(0,B).
moveEnemy2(N,B) :- draw(B,400,N),moveEnemy(moveEnemy2,N+10,B).
moveEnemy3(400,B) :- moveEnemy4(0,B).
moveEnemy3(N,B) :- draw(B,400-N,400),moveEnemy(moveEnemy3,N+10,B).
moveEnemy4(400,B) :- moveEnemy1(0,B).
moveEnemy4(N,B) :- draw(B,0,400-N),moveEnemy(moveEnemy4,N+10,B).
runEnemy(C,C2) :- reset(C,_,C2).
initShip :-
   send(@w, display, new(@area, box(10000,10000))),
   send(@w, display, new(@ship, box(20,20))),
   send(@area, recogniser,move_gesture(left)),
   send(@area, center, point(200,350)),
   send(@ship, fill_pattern, colour(blue)).
moveShip :- 
  get(@area,center,point(PX,PY)),
  X is max(15,min(415,PX)),
  Y is max(15,min(415,PY)),
  send(@area,center,point(X,Y)),
  send(@ship,center,point(X,Y)).
move(Cs) :-
  moveShip,
  maplist(runEnemy,Cs,Cs_),
  send(@w,flush),
  sleep(0.01),
  move(Cs_).

initWindow :- new(@w, dialog(game)),
   send(@w, size, size(430,430)),
   send(@w, open).
initEnemies(Bs2) :-
  findall(B,(between(0,3,_),newEnemy(0,0,B)),Bs),
  maplist([B1,R,F]>>(F =.. [R,0,B1]),Bs,[moveEnemy1,moveEnemy2,moveEnemy3,moveEnemy4],Bs2).
:- initWindow,
   initShip,
   initEnemies(Bs2),
   move(Bs2).
:- halt.
