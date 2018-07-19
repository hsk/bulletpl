:- use_foreign_library(foreign(plOpenGL)).
:- use_module(library(plOpenGL)).
:- use_module(library(plGL_defs)).
:- use_module(library(plGLU_defs)).
:- use_module(library(plGLUT_defs)).
:- use_module(library(plGL)).
:- use_module(library(plGLU)).
:- use_module(library(plGLUT)).
:- expects_dialect(sicstus).
:- bb_put(rank,0).
rank(R) :- bb_get(rank,R).
:- bb_put(enemies,[]).
enemies(Es) :- bb_get(enemies,Es).

/*
private function onEnter(e:Event):void {
  for each (var enemy:Enemy in enemys) {
          enemy.move();
  }
  if(enemys.length == 0 || Math.random() < 0.01 * rank) {
      switch(int(Math.random() * 3)){
      case 0: addEnemy(0,Math.random()*200,0); break;
      case 1: addEnemy(Math.random()*465,0,Math.PI/2); break;
      case 2: addEnemy(465,Math.random()*200,Math.PI); break;
      }
  }
  ship.move();
  for each (var b:Bullet in bullets) {
          b.move();
          if(Math.abs(ship.x-b.x)<3 && Math.abs(ship.y-b.y)<3) {
              addParticles(ship.x, ship.y, 100);
              ship.exists = false;
              ship.visible = false;
              gameover.visible = true;
          }
      }
  for each (var s:Shot in shots) {
          for each(enemy in enemys) {
              if(enemy.visible && Math.abs(enemy.x-s.x)<30 && Math.abs(enemy.y-s.y)<30) {
                  score += 100;
                  rank = score / 1000;
                  addParticles(enemy.x, enemy.y,30);
                  enemy.visible = false;
              }
          }
          s.move();
      }
  for each (var p:Particle in particles)
          p.move();
}
public function addParticles(x:Number, y:Number, n:int):void {
  for (var i:int = 0; i < n; i++)
      addParticle(x,y,Math.random() * 100);
}

public function addBulletN(x:Number, y:Number, rad:Number,n:int, r:Number):void {
  for (var i:int = 0; i < n; i++) {
    addBullet(x,y,rad+(i-n/2.0)*r);
  }
}

newEnemy(Env,X,Y,E) :-
  Rad is atan((Y-Env.my)/(X-Env.mx)),
  E=enemy{x:X,y:Y,rad:Rad,move:enemy_move1}.

enemy_move1(Env,E,E_) :-
  moveBody(Env,E,R),
    if(Math.random() > 0.8) {
        a.addBullet(x,y,r);
        if(Math.random() > 0.8) {
            r2=0;
            wait(30,move2);
        }
    }
}

class Enemy extends Sprite{
    private var rad:Number = 0;
    private var r:Number = 0.05;
    public var move:Function;
    private var nextMove:Function;
    private var time:int;
*/

enemy_moveBody(Env,E,E_,Rad2) :-
  Rad2 is atan2(Env.my-E.y, Env.mx-E.x),
  Rad3 is E.rad-Rad2,
  normalRad(Rad3,NRad),
  (NRad < 0.0 -> Rad3 is Rad + 0.05
                ; Rad3 is Rad - 0.05),
  normalRad(Rad3,Rad4),
  X1 is X+cos(Rad4)*5.0,
  Y1 is Y+sin(Rad4)*5.0,
  clipX(X1,X2),clipY(Y1,Y2),
  E_=E.put(x,X2).put(y,Y2).put(rad,Rad4).
enemy_wait(Env,W,E,Next,E2) :-
  E2 = E.put(time,W).put(move,enemy_moveWait).put(nextMove,Next).
enemy_moveWait(Env,E,E3) :-
  enemy_moveBody(Env,E,E1,_),
  Time is E1.time - 1,
  E2=E1.put(time,Time),
  (Time =< 0 -> E3=E2.put(move,E2.nextMove)
  ; E3=E2).

enemy_move1(Env,E,E2) :-
  enemy_moveBody(Env,E,E_,R),
  random(0.0,1.0,R1),
  (R1 > 0.8 ->
    % addBullet(x,y,r),
    random(0.0,1.0,R2),
    (R2 > 0.8 ->
      enemy_wait(Env,30,E.put(r2,0.0),enemy_move2,E2)
    ;E2=E)
  ;E2=E).
enemy_move2(Env,E,E2):-
  enemy_moveBody(Env,E,E_,R),
  random(0.1,0.6,RR),
  %addBulletN(x,y,r+r2,rr,0.1),
  R2 is E_.r2+RR*0.1,
  E1 = E_.put(r2,R2),
  random(0.0,1.0,R3),
  (R3 < 0.03 ->
    enemy_wait(Env,60,E1,enemy_move1,E2)
  ;E2=E1).

normalRad(V,R) :- V >  3.141592, R is V - 2.0 * 3.141592.
normalRad(V,R) :- V < -3.141592, R is V + 2.0 * 3.141592.
normalRad(V,V).

/*
class Particle extends Sprite{
    private var speed:Number = 5;
    private var rad:Number;
    public function Particle() {
        graphics.beginFill(0xffff8800);
        graphics.drawRect(-2,-2,4,4);
        graphics.endFill();
        visible = false;
    }
    public function init(x:Number, y:Number, rad:Number):void {
        this.x = x;
        this.y = y;
        this.rad = rad;
        visible = true;
        speed = Math.random() * 50 + 10;
    }
    public function move():Boolean {
        x += Math.cos(rad)*speed;
        y += Math.sin(rad)*speed;
        if(x < 0 || 465 < x || y < 0 || 465 < y) visible = false;
        return visible ;
    }
}

class Bullet extends Sprite{
    private var speed:Number = 10;
    private var rad:Number;
    public function Bullet() {
        graphics.beginFill(0xffff8800);
        graphics.drawRect(-5,-5,10,10);
        graphics.endFill();
        visible = false;
    }
    public function init(x:Number, y:Number, rad:Number):void {
        this.x = x;
        this.y = y;
        this.rad = rad;
        visible = true;
    }
    public function move():Boolean {
        x += Math.cos(rad)*speed;
        y += Math.sin(rad)*speed;
        if(x < 0 || 465 < x || y < 0 || 465 < y) visible = false;
        return visible ;
    }
}
*/

{}(A,A).
<<(B,A,D) :- maplist(call,A,V),append(V,[D],V2),apply(B,V2).
B << A :- maplist(call,A,V),apply(B,V).
vertex2f(X,Y) :- Xv is X, Yv is Y, glVertex2f(Xv,Yv).
\/(A,B,C) :- C is A\/B.

:- 
  Mh is sqrt(3.0) * 30.0,
  bb_put(env,env{
    width:465,height:465,
    mx:232.5,my:400.0,
    mh:Mh,
    shots:[]
  }).

:- bb_put(key,key{left:0.0,right:0.0,shot:0,trigger:0}).

keyboard(27,_,_) :- glutDestroyWindow. % esc
keyboard(32,_,_) :- !,bb_update(key,K,K.put(shot,1).put(trigger,1)). % space
keyboard(R,A,B) :- writeln(R).
keyboardUp(32,_,_) :- bb_update(key,K,K.put(shot,0)). %space
keyboardUp(R,A,B) :- writeln(R).

special(100,_,_) :- bb_update(key,K,K.put(left,-1.0)).
special(102,_,_) :- bb_update(key,K,K.put(right,1.0)).
%special(A,_,_) :- writeln(A).
specialUp(100,_,_) :- bb_update(key,K,K.put(left,0.0)).
specialUp(102,_,_) :- bb_update(key,K,K.put(right,0.0)).

timer(_) :-
  bb_get(env,E),move(E,E_),bb_put(env,E_),
  glutPostRedisplay,c_glutTimerFunc(10,0).

move --> move_ship,move_shots.
move_ship(Env,Env3) :-
  bb_update(key,K,K.put(trigger,0)),
  move_ship_shot(K.shot,Env,Env2),
  Mx is min(max(Env2.mx + (K.left+K.right)*5,30.0),Env.width-30.0),
  Env3=Env2.put(mx,Mx).

move_ship_shot(1,Env,Env2) :-
  random(-0.25,0.25,Rnd),
  Rad is -3.141592/2.0+Rnd,
  Env2=Env.put(shots,[shot{x:Env.mx,y:Env.my,rad:Rad}|Env.shots]).
move_ship_shot(_,E,E).

move_shots(Env,Env.put(shots,Shots)) :- foldl(move_shot(Env),Env.shots,[],Shots).
move_shot(_,Shot,Shots,[Shot.put(x,Nx).put(y,Ny)|Shots]) :-
  Shot.y > 0,Shot.y < 465, Shot.x > 0, Shot.x < 465,
  Nx is Shot.x + cos(Shot.rad) * 25.0,
  Ny is Shot.y + sin(Shot.rad) * 25.0, !.
move_shot(_,_,Shots,Shots).

/*
move_ship(Env,Env_.put(mx,Mx)) :-
  bb_update(key,K,K.put(trigger,0)),
  %move_ship_shot(K.trigger,Env,Env_),
  Mx is min(max(Env.mx + (K.left+K.right)*5,30),Env.width-30).

*/

draw_ship(Env) :- !,
	glBegin<<[kGL_POLYGON],
	glColor4f(1.0, 1.0, 0.0, 0.75),
  vertex2f(Env.mx-10, Env.my-10),
  vertex2f(Env.mx+10, Env.my-10),
  vertex2f(Env.mx+10, Env.my+10),
  vertex2f(Env.mx-10, Env.my+10),
	glEnd,!.

draw_shots(Shots) :- maplist(draw_shot,Shots).
draw_shot(Shot) :- !,
	glBegin<<[kGL_POLYGON],
	glColor4f(0.0, 1.0, 1.0, 0.75),!,
  vertex2f(Shot.x-10, Shot.y-10),
  vertex2f(Shot.x+10, Shot.y-10),
  vertex2f(Shot.x+10, Shot.y+10),
  vertex2f(Shot.x-10, Shot.y+10),
	glEnd,!.

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

display :- bb_get(env,Env),
	glClear<<[kGL_COLOR_BUFFER_BIT],
	draw_ship(Env),
  draw_shots(Env.shots),
	glFlush.

idle :- garbage_collect.

:-
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
  c_glutTimerFunc(10,0),
%  glutIdleFunc(idle),
	init,
	glutMainLoop.
