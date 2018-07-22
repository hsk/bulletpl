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
vertex2f(X,Y) :- Xv is float(X), Yv is float(Y), glVertex2f(Xv,Yv).
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
specialUp(100,_,_) :- bb_update(key,K,K.put(left,0.0)).
specialUp(101,_,_) :- bb_update(key,K,K.put(up,0.0)).
specialUp(102,_,_) :- bb_update(key,K,K.put(right,0.0)).
specialUp(103,_,_) :- bb_update(key,K,K.put(down,0.0)).

timer(_) :-
  move,
  bb_get(env,Env),
  get_time(NTime),
  bb_update(time,Time,NTime),
  W is max(0,Env.wait-integer((NTime-Time)*1000)),
  glutPostRedisplay,
  c_glutTimerFunc(W,0).
reshape :-
  bb_get(env,Env),
  glViewport(0,0,Env.width,Env.height),
  glMatrixMode<<[kGL_PROJECTION],
  glLoadIdentity,
  gluOrtho2D(0.0, 465.0, 465.0, 0.0).
idle :- garbage_collect.

main :-
  bb_put(env,env{width:465,height:465,wait:25}),
  bb_put(key,key{left:0.0,right:0.0,up:0.0,down:0.0,shot:0,trigger:0}),
  bb_put(ship,ship{x:232.5,y:400.0}),
  bb_put(bullets,[]),
  bb_get(env,Env),
  glutInit,
  glutInitDisplayMode<<[(\/)<<[kGLUT_SINGLE,kGLUT_RGB]],
  glutInitWindowSize(Env.width, Env.height),
  glutInitWindowPosition(0,0),
%  glutIdleFunc(idle),
  glutCreateWindow('Alpha'),
  glutDisplayFunc,glutReshapeFunc,glutKeyboardFunc,
  c_glutIgnoreKeyRepeat(true),c_glutKeyboardUpFunc,
  c_glutSpecialFunc,c_glutSpecialUpFunc,
  get_time(T),bb_put(time,T),
  c_glutTimerFunc(20,0),
  glEnable<<[kGL_BLEND],
  glBlendFunc<<[kGL_SRC_ALPHA,kGL_ONE_MINUS_SRC_ALPHA],
  glShadeModel<<[kGL_FLAT],
  glClearColor(0.0, 0.0, 0.2, 1.0),
  glutMainLoop.

display :- glClear<<[kGL_COLOR_BUFFER_BIT], drawShip,drawBullets,glFlush.
move :- moveShip,moveBullets.

drawRect(X,Y,W,H) :-
  kGL_POLYGON(K),
  glBegin(K),
  vertex2f(X, Y),vertex2f(X+W, Y),vertex2f(X+W, Y+H),vertex2f(X, Y+H),
  glEnd,!.

drawShip :-
  bb_get(ship,Ship),
  glColor4f(1.0, 1.0, 0.0, 0.75),!,
  drawRect(Ship.x-10,Ship.y-10,20,20).
drawBullet(B) :-
  drawRect(B.x-5,B.y-5,10,10).
drawBullets :-
  glColor4f(0.0, 1.0, 1.0, 1.0),!,
  bb_get(bullets,Bs),maplist(drawBullet,Bs),!.

% ship

moveShip :-
  bb_update(key,K,K.put(trigger,0)),
  bb_get(ship,Ship),bb_get(env,Env),
  X is min(max(Ship.x + (K.left+K.right)*5.0,30.0),Env.width-30.0),
  Y is min(max(Ship.y + (K.up+K.down)*5.0,30.0),Env.height-30.0),
  bb_put(ship,Ship.put(x,X).put(y,Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

M.del(K) := M2 :- del_dict(K,M,_,M2);M=M2.
%:- R=a{}.del(a),writeln(R). :- halt.
dir(_,dirAbs(D),D_) :- D_ is D.
spd(_,spdAbs(S),S_) :- S_ is S.

chgDir(B,D,B1) :- (OD,ND,C,M)=B.get(chgDir),!,
  dir(B,ND,ND1),
  CM is C/M,
  D is OD*(1.0-CM)+ND1*CM,
  (CM =:= 1.0 -> B1 = B.del(chgDir).put(dir,ND)
  ;C1 is C + 1.0,B1 = B.put(chgDir,(OD,ND,C1,M))).
chgDir(B,D,B) :- dir(B,B.dir,D).
chgSpd(B,S,B1) :- (OS,NS,C,M)=B.get(chgSpd),!,
  spd(B,NS,NS1),
  CM is C/M,
  S is OS*(1.0-CM)+NS1*CM,
  (CM =:= 1.0 -> B1 = B.del(chgSpd).put(spd,NS)
  ;C1 is C + 1.0,B1 = B.put(chgSpd,(OS,NS,C1,M))).
chgSpd(B,S,B) :- spd(B,B.spd,S).

moveDefault :-
  bb_get(bullet,B),
  chgDir(B,D,B1),chgSpd(B1,S,B2),
  Y is (B.y + cos(D)*S),X is (B.x - sin(D)*S),
  bb_put(bullet,B2.put(x,X).put(y,Y).put(pdir,D).put(pspd,S)).

%moveBullet(A) :- writeln(moveBullet(A)),fail.
moveBullet(action(As)) :- !,maplist(moveBullet,As),!.
moveBullet(wait(0)) :- !.
moveBullet(wait(N)) :- !,moveDefault,shift(0), N1 is N - 1, moveBullet(wait(N1)),!.
moveBullet(fire(bullet(D,S,As))) :- !,
  bb_get(bullet,B),
  newBullet(B.x,B.y,B2),
  spd(B2,S,S_),
  dir(B2,D,D_),
  bb_update(bullets,Bs,[B2.put([dir:D,spd:S,pdir:D_,pspd:S_,cont:moveBullet(action(As))])|Bs]).
moveBullet(changeDirection(D,T)) :- bb_update(bullet,B,B.put(chgDir,(B.pdir,D,0.0,T))).
moveBullet(changeSpeed(S,T)) :- bb_update(bullet,B,B.put(chgSpd,(B.pspd,S,0.0,T))).
moveBullet(repeat(0,_)) :- !.
moveBullet(repeat(N,As)) :- moveBullet(action(As)),N1 is N - 1,moveBullet(repeat(N1,As)).
runBullet(B) :-
  bb_put(bullet,B),reset(B.cont,_,Cont),
  ( Cont=0
  ; bb_get(bullet,B1),bb_update(bullets,Bs,[B1.put(cont,Cont)|Bs])).
moveBullets :- bb_update(bullets,Bs,[]),!,(Bs=[],init;maplist(runBullet,Bs)).

newBullet(X,Y,B) :- B=bullet{x:X,y:Y}.

init :-
  newBullet(200,50,B),
  bb_put(bullets,[B.put([dir:dirAbs(0.0),spd:spdAbs(0.0),cont:moveBullet(action([
    repeat(3,[
      repeat(5,[
        fire(bullet(dirAbs(1.0),spdAbs(1.0),[
          changeSpeed(spdAbs(4.0),3.0),wait(3),
          repeat(3,[
            changeDirection(dirAbs(-1.08),10.0),wait(10),
            changeDirection(dirAbs(1.08),10.0),wait(10)
          ]),
          changeSpeed(spdAbs(8.0),30.0),
          changeDirection(dirAbs(-2.0),30.0),wait(80)
        ])),
        
        fire(bullet(dirAbs(-3.14159/2.0),spdAbs(1.0),[
          changeSpeed(spdAbs(4.0),30.0),wait(40),
          changeDirection(dirAbs(0.1),30.0),wait(80),
          changeSpeed(spdAbs(8.0),30.0),
          changeDirection(dirAbs(2.0),30.0),wait(80)
        ])),
        wait(10)
      ])
    ])

  ]))])]).

:- main.
