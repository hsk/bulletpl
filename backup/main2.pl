:- use_foreign_library(foreign(plOpenGL)).
:- use_module(library(plOpenGL)).
:- use_module(library(plGL_defs)).
:- use_module(library(plGLU_defs)).
:- use_module(library(plGLUT_defs)).
:- use_module(library(plGL)).
:- use_module(library(plGLU)).
:- use_module(library(plGLUT)).
:- expects_dialect(sicstus).

{}(A,A).
<<(B,A,D) :- maplist(call,A,V),append(V,[D],V2),apply(B,V2).
B << A :- maplist(call,A,V),apply(B,V).
vertex2f(X,Y) :- Xv is X, Yv is Y, glVertex2f(Xv,Yv).
\/(A,B,C) :- C is A\/B.

keyboard(27,_,_) :- glutDestroyWindow. % esc
keyboard(122,_,_) :- !,bb_update(key,K,K.put(shot,1).put(trigger,1)). % space
keyboard(R,_,_) :- writeln(R).
keyboardUp(122,_,_) :- bb_update(key,K,K.put(shot,0)). %space
keyboardUp(R,_,_) :- writeln(R).

special(100,_,_) :- bb_update(key,K,K.put(left,-1.0)).
special(101,_,_) :- bb_update(key,K,K.put(up,-1.0)).
special(102,_,_) :- bb_update(key,K,K.put(right,1.0)).
special(103,_,_) :- bb_update(key,K,K.put(down,1.0)).
%special(A,_,_) :- writeln(A).
specialUp(100,_,_) :- bb_update(key,K,K.put(left,0.0)).
specialUp(101,_,_) :- bb_update(key,K,K.put(up,0.0)).
specialUp(102,_,_) :- bb_update(key,K,K.put(right,0.0)).
specialUp(103,_,_) :- bb_update(key,K,K.put(down,0.0)).

timer(_) :-
  bb_get(env,Env),
  move,
  glutPostRedisplay,
  c_glutTimerFunc(Env.wait,0).

:- bb_put(env,env{width:465,height:465,wait:20}).
:- bb_put(key,key{left:0.0,right:0.0,up:0.0,down:0.0,shot:0,trigger:0}).
:- bb_put(rank,20).
:- bb_put(ship,ship{x:232.5,y:400.0}).
:- bb_put(shots,[]).
:- bb_put(enemies,[]).
:- bb_put(bullets,[]).
:- bb_put(particles,[]).

clipX(X1,X2) :- X2 is max(0.0,min(465.0,X1)).
clipY(X1,X2) :- X2 is max(0.0,min(465.0,X1)).
normalRad(V,R) :- V >  3.141592, R is V - 2.0 * 3.141592.
normalRad(V,R) :- V < -3.141592, R is V + 2.0 * 3.141592.
normalRad(V,V).

% ship

  moveShip :-
    bb_update(key,K,K.put(trigger,0)),
    moveShipShot(K.shot),
    bb_get(ship,Ship),
    bb_get(env,Env),
    X is min(max(Ship.x + (K.left+K.right)*5.0,30.0),Env.width-30.0),
    Y is min(max(Ship.y + (K.up+K.down)*5.0,30.0),Env.height-30.0),
    bb_put(ship,Ship.put(x,X).put(y,Y)).
  moveShipShot(1) :-
    random(-0.25,0.25,Rnd),
    Rad is -3.141592/2.0+Rnd,
    bb_get(ship,Ship),
    bb_update(shots,Shots,[shot{x:Ship.x,y:Ship.y,rad:Rad}|Shots]).
  moveShipShot(_).

  drawShip :- !,
    bb_get(ship,Ship),
    glBegin<<[kGL_POLYGON],
    glColor4f(1.0, 1.0, 0.0, 0.75),
    vertex2f(Ship.x-10, Ship.y-10),
    vertex2f(Ship.x+10, Ship.y-10),
    vertex2f(Ship.x+10, Ship.y+10),
    vertex2f(Ship.x-10, Ship.y+10),
    glEnd,!.

% shot
  moveShot(It) :-
    It.y > 0,It.y < 465, It.x > 0, It.x < 465,
    Nx is It.x + cos(It.rad) * 25.0,
    Ny is It.y + sin(It.rad) * 25.0, !,
    bb_update(shots,Shots,[It.put(x,Nx).put(y,Ny)|Shots]),
    bb_get(enemies,Enemies),
    exclude(collisionShot(It),Enemies,Enemies_),
    bb_put(enemies,Enemies_).
  moveShot(_).

  collisionShot(It,Enemy) :-
    X is abs(Enemy.x-It.x), X<30.0,
    Y is abs(Enemy.y-It.y), Y<30.0,
    addParticles(Enemy.x, Enemy.y,30).

  moveShots :-
    bb_update(shots,Shots,[]),!,
    maplist(moveShot,Shots).
  drawShots :-
    bb_get(shots,Shots),
    maplist(drawShot,Shots).
  drawShot(Shot) :- !,
    glBegin<<[kGL_POLYGON],
    glColor4f(0.0, 1.0, 1.0, 0.75),!,
    vertex2f(Shot.x-10, Shot.y-10),
    vertex2f(Shot.x+10, Shot.y-10),
    vertex2f(Shot.x+10, Shot.y+10),
    vertex2f(Shot.x-10, Shot.y+10),
    glEnd,!.

% enemy
  newEnemy(X,Y) :-
    bb_get(ship,Ship),
    (X=Ship.x -> Rad = 0.0; Rad is atan((Y-Ship.y)/(X-Ship.x))),
    E=enemy{x:X,y:Y,rad:Rad,move:move1Enemy},
    bb_update(enemies,Enemies,[E|Enemies]).
  moveEnemy(Enemy) :-
    bb_put(enemy,Enemy),
    reset(Enemy.move,_,Cont),
    (var(Cont),!;Cont=0
    ;bb_get(enemy,Enemy1),bb_update(enemies,Enemies,[Enemy1.put(move,Cont)|Enemies])).
  moveEnemy(Enemy) :- writeln(error:moveEnemy:Enemy),throw(error).
  moveBodyEnemy(E_,Rad2) :-
    bb_get(enemy,E), bb_get(ship,Ship),
    Rad2 is atan2(Ship.y-E.y, Ship.x-E.x),
    Rad3 is E.rad-Rad2,
    normalRad(Rad3,NRad),
    (NRad < 0.0 -> Rad4 is E.rad + 0.05
                  ; Rad4 is E.rad - 0.05),
    normalRad(Rad4,Rad5),
    X1 is E.x+cos(Rad5)*2.0,
    Y1 is E.y+sin(Rad5)*2.0,
    clipX(X1,X2),clipY(Y1,Y2),
    E_=E.put(x,X2).put(y,Y2).put(rad,Rad5),
    bb_put(enemy,E_).
  wait(0) :- shift(0).
  wait(N) :- shift(0),moveBodyEnemy(_,_),N1 is N-1,wait(N1).
  move1Enemy :-
    moveBodyEnemy(E_,R),
    random(0,100,R1),
    (R1 < 3 -> addBullet(E_.x,E_.y,R),wait(100);wait(0)),
    move1Enemy.
  addEnemy :-
    bb_get(rank,Rank),
    bb_get(enemies,Enemies),length(Enemies,L),
    (Enemies=[];L<5,random(0.0,100.0,R1),R1<Rank),!,
    random(0,3,R),addEnemy1(R).
  addEnemy.
  addEnemy1(0) :- random(0.0,200.0,Y),newEnemy(100.0,Y).
  addEnemy1(1) :- random(0.0,456.0,X),newEnemy(X,100.0).
  addEnemy1(2) :- random(0.0,200.0,Y),newEnemy(100.0,Y).
  addEnemy1(N) :- writeln(errror_addEnemy1:N).
  moveEnemies :-
    bb_update(enemies,Enemies,[]),
    maplist(moveEnemy,Enemies),
    addEnemy,!.
  drawEnemies :-
    bb_get(enemies,Enemies),
    maplist(drawEnemy,Enemies).
  drawEnemy(Enemy) :- !,
    glBegin<<[kGL_POLYGON],
    glColor4f(0.0, 1.0, 1.0, 0.75),!,
    vertex2f(Enemy.x-10, Enemy.y-10),
    vertex2f(Enemy.x+10, Enemy.y-10),
    vertex2f(Enemy.x+10, Enemy.y+10),
    vertex2f(Enemy.x-10, Enemy.y+10),
    glEnd,!.

% bullet

  addBulletN(X,Y,Rad,N,R) :-
    forall(between(1,N,I),(
      R2 is float(Rad+(I-1-N/2.0)*R),
      addBullet(X,Y,R2),!
    )),!.
  addBullet(X,Y,Rad) :-
    bb_update(bullets,Bullets,[bullet{x:X,y:Y,rad:Rad,speed:10.0}|Bullets]),!.
  moveBullet(This) :-
    X is This.x + cos(This.rad)*This.speed,
    Y is This.y + sin(This.rad)*This.speed,
    (X < 0 ; 465 < X ; Y < 0 ; 465 < Y
    ; bb_update(bullets,Bullets,[This.put(x,X).put(y,Y)|Bullets]),
      (collisionBullet(This);true)
    ).
  collisionBullet(B) :-
    bb_get(ship,Ship),
    X is abs(Ship.x-B.x), X<3.0,
    Y is abs(Ship.y-B.y), Y<3.0,
    addParticles(Ship.x, Ship.y, 50),
    %ship.exists = false;
    %ship.visible = false;
    %gameover.visible = true;
    !.
  drawBullets :-
    bb_get(bullets,Bullets),
    glColor4f(1.0, 0.0, 1.0, 0.75),!,
    maplist(drawBullet,Bullets).
  drawBullet(It) :- !,
    glBegin<<[kGL_POLYGON],
    vertex2f(It.x-4, It.y-4),
    vertex2f(It.x+4, It.y-4),
    vertex2f(It.x+4, It.y+4),
    vertex2f(It.x-4, It.y+4),
    glEnd,!.

  moveBullets :-
    bb_update(bullets,Bullets,[]),
    maplist(moveBullet,Bullets),
    %bb_get(bullets,Es),!,writeln(bullets:Es),
    !.

% particle

  addParticle(X,Y,Rad) :-
    random(0.0,1.0,Rnd),
    Speed is float(Rnd * 50+10),
    Particle = particle{x:X,y:Y,rad:Rad,speed:Speed},
    bb_get(particles,Particles),length(Particles,L),
    (L>500 -> true; bb_put(particles,[Particle|Particles])).

  moveParticle(It) :-
    X is It.x + cos(It.rad)*It.speed,
    Y is It.y + sin(It.rad)*It.speed,
    (X < 0 ; 465 < X ; Y < 0 ; 465 < Y
    ; bb_update(particles,Particles,[It.put(x,X).put(y,Y)|Particles])).
  moveParticles :-
    bb_update(particles,Particles,[]),!,
    maplist(moveParticle,Particles).
  addParticles(X,Y,N) :-
    forall(between(1,N,_),(
      random(0.0,100.0,N1),addParticle(X,Y,N1)
    )).
  :- bb_put(maxLen,0).
  drawParticles :-
    bb_get(particles,Particles),
    length(Particles,L),
    bb_get(maxLen,MaxLen),(L>MaxLen,bb_put(maxLen,L),writeln(L);true),
    glColor4f(1.0, 0.3, 0.1, 1.0),!,
    maplist(drawParticle,Particles).
  drawParticle(It) :- !,
    glBegin<<[kGL_POLYGON],
    vertex2f(It.x-4, It.y-4),
    vertex2f(It.x+4, It.y-4),
    vertex2f(It.x+4, It.y+4),
    vertex2f(It.x-4, It.y+4),
    glEnd,!.

move :- moveShip,moveShots,moveEnemies,moveBullets,moveParticles.
display :-
	glClear<<[kGL_COLOR_BUFFER_BIT],
	drawShip,
  drawShots,
  drawEnemies,
  drawBullets,
  drawParticles,
	glFlush.
reshape :-
  bb_get(env,Env),
	glViewport(0,0,Env.width,Env.height),
	glMatrixMode<<[kGL_PROJECTION],
	glLoadIdentity,
	gluOrtho2D(0.0, 465.0, 465.0, 0.0).
init:-
	glEnable<<[kGL_BLEND],
	glBlendFunc<<[kGL_SRC_ALPHA,kGL_ONE_MINUS_SRC_ALPHA],
	glShadeModel<<[kGL_FLAT],
	glClearColor(0.0, 0.0, 0.0, 0.0).
idle :- garbage_collect.
main :-
  bb_get(env,Env),
	glutInit,
	glutInitDisplayMode<<[(\/)<<[kGLUT_SINGLE,kGLUT_RGB]],
	glutInitWindowSize(Env.width, Env.height),
	glutInitWindowPosition(0,0),
	glutCreateWindow('Alpha'),
  c_glutIgnoreKeyRepeat(true),
  glutDisplayFunc,
	glutReshapeFunc,
	glutKeyboardFunc,
	c_glutKeyboardUpFunc,
	c_glutSpecialFunc,
	c_glutSpecialUpFunc,
  c_glutTimerFunc(20,0),
  glutIdleFunc(idle),
	init,
	glutMainLoop.

:- main.
