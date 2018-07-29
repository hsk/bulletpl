:- module(echo_server,[]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/websocket)).
:- http_handler(root(.),http_reply_from_files('.', []), [prefix]).
:- http_handler(root(sock),http_upgrade_to_websocket(sock, []),[]).
:- assert(websock(dummy)).
sock(WebSocket) :- writeln(start),retract(websock(_)),assertz(websock(WebSocket)),main.
server(Port) :- http_server(http_dispatch, [port(Port)]).
:- server(3030).
:- www_open_url('http://localhost:3030/index.html').
sever_end(false).
server_wait :- sleep(0.01),(sever_end(true);server_wait).
%:- server_wait.

:- dynamic ship/2.
ship(150,350).
assert_ship(V) :- assertz(ship(V.x, V.y)),retract(ship(_,_)).
send_message(V) :- websock(WebSocket),ws_send(WebSocket,binary(V)).
get_message(V) :- websock(WebSocket),ws_receive(WebSocket, V, [format(json)]).

getShip(ship{x:X_,y:Y_}) :- ship(X_,Y_).
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
evalto(V,E1 mod E2) :- !,evalto(E1_,E1),evalto(E2_,E2), V is E1_ mod E2_.
evalto(V,atan(E)) :- !,evalto(E_,E), V is atan(E_).
evalto(V,sin(E)) :- !,evalto(E_,E), V is sin(E_).
evalto(V,cos(E)) :- !,evalto(E_,E), V is cos(E_).
evalto(V,floor(E)) :- !,evalto(E_,E), V is floor(E_).
evalto(V,-E) :- !,evalto(E_,E), V is -E_.
evalto(E1_:E2_,E1:E2) :- !,evalto(E1_,E1),!,evalto(E2_,E2). 
evalto(_,E) :- writeln(error:evalto(E)),halt.
user:M.del(K) := M2 :- del_dict(K,M,_,M2);M=M2.
refmap(G,P,N,N1) :- N1 is N+1,P_ evalto P,member(N:P_,G),!.
refmap(_,P,N,N1) :- N1 is N+1,writeln(waring:parameter($N:P)).
actionRef(K,Ps) :- actionV(K,G,As),foldl(refmap(G),Ps,1,_),!,action(As).
action(As) :- maplist(call,As).
wait(N) :- N1 evalto N, N1 =< 0, !.
wait(N) :- retract(bullet(B)),chgDir(B,D,B1),chgSpd(B1,S,B2),acc(B2,D,S,N).
chgDir(B,D,B1) :- (CD,T)=B.get(chgDir),
  D evalto B.dir+CD,T1 evalto T - 1,
  (T1 =< 0,_=B.get(isAim) ->
              getShip(Ship),dir(B,dirAim(D),Ship,D1),
              B1 = B.del(chgDir).del(isAim).put(dir,D1)
  ;T1 =< 0 -> B1 = B.del(chgDir)
  ;           B1 = B.put(chgDir,(CD,T1))).
chgDir(B,B.dir,B).
chgSpd(B,S,B1) :- (CS,T)=B.get(chgSpd),
  S evalto B.spd + CS, T1 evalto T - 1,
  (T1 =< 0 -> B1 = B.del(chgSpd)
  ;           B1 = B.put(chgSpd,(CS,T1))).
chgSpd(B,B.spd,B).
mx(B,X) :- X=B.get(mx);X=0.
my(B,Y) :- Y=B.get(my);Y=0.
acc(B,D,S,N) :- (_,_,T)=B.get(accel),T =< 0,!,acc(B.del(accel),D,S,N).
acc(B,D,S,N) :- (H,V,T)=B.get(accel),!,
  D_ is D/180*3.14159,T1 evalto T - 1,
  mx(B,Mx),my(B,My),Mx_ is Mx+H,My_ is My+V,
  X is B.x + sin(D_)*S+Mx_,Y is B.y - cos(D_)*S+My_,
  cont(B.put([x:X,y:Y,dir:D,spd:S,accel:(H,V,T1),mx:Mx_,my:My_]),N).
acc(B,D,S,N) :-
  mx(B,Mx),my(B,My),
  D_ is D/180*3.14159,X is B.x + sin(D_)*S+Mx,Y is B.y - cos(D_)*S+My,
  cont(B.put([x:X,y:Y,dir:D,spd:S]),N).
cont(B,_) :- (B.x < 0; B.y < 0; B.x > 300; B.y > 400),asserta(bullet(B)),shift(1). % 画面外で消える
cont(B,N) :- asserta(bullet(B)),shift(0),N1 evalto N - 1, wait(N1).
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
  assert(bullet(B2.put([dir:D_,spd:S_,cont:action(As)]))).
dir(_,dirAbs(D),_,D_) :- D_ evalto D.
dir(B,dirSeq(D),_,D_) :- D_ evalto B.get(fdir) + D.
dir(B,dirRel(D),_,D_) :- D_ evalto B.dir + D.
dir(B,dirSeq(D),Ship,D_) :- dir(B,dirAim(D),Ship,D_).
dir(B,dirAim(D),Ship,D_) :- Ship.y =:= B.y,!, (Ship.x > B.x -> D_ is 90+D; D_ = D-90),!.
dir(B,dirAim(D),Ship,D_) :- Ship.y > B.y,!, D_ evalto atan((B.x - Ship.x) / (Ship.y - B.y))*180/3.141592 - 180 + D.
dir(B,dirAim(D),Ship,D_) :-                 D_ evalto atan((B.x - Ship.x) / (Ship.y - B.y))*180/3.141592 + D.
spd(_,spdAbs(S),_,S_) :- S_ evalto S.
spd(B,spdSeq(S),_,S_) :- S_ evalto B.get(fspd) + S.
spd(B,spdRel(S),_,S_) :- S_ evalto B.spd + S.
spd(B,spdSeq(D),Ship,D_) :- spd(B,spdAbs(D),Ship,D_).
changeDirection(dirSeq(D),T) :- retract(bullet(B)),D_ evalto D,asserta(bullet(B.put(chgDir,(D_,T)))).
changeDirection(dirAbs(D),T) :- retract(bullet(B)),D_ evalto(D-B.dir)/T,asserta(bullet(B.put(chgDir,(D_,T)))).
changeDirection(dirRel(D),T) :- retract(bullet(B)),D_ evalto D/T,asserta(bullet(B.put(chgDir,(D_,T)))).
changeDirection(dirAim(D),T) :-
  retract(bullet(B)),getShip(Ship),dir(B,dirAim(D),Ship,D1),D2 evalto (D1-B.dir),
  D_ evalto (((floor(D2) + 180)mod 360)-180)/T,asserta(bullet(B.put(chgDir,(D_,T)))).
changeSpeed(spdSeq(S),T) :- retract(bullet(B)),S_ evalto S,asserta(bullet(B.put(chgSpd,(S_,T)))).
changeSpeed(spdAbs(S),T) :- retract(bullet(B)),S_ evalto(S-B.spd)/T,asserta(bullet(B.put(chgSpd,(S_,T)))).
changeSpeed(spdRel(S),T) :- retract(bullet(B)),S_ evalto S/T, asserta(bullet(B.put([chgSpd:(S_,T),isAim:0]))).
accelspd(_,spdSeq(S),_,_,S_) :- S_ evalto S.
accelspd(B,spdAbs(S),M,T,S_) :- call(M,B,M_),S_ evalto (S-M_)/T.
accelspd(_,spdRel(S),_,T,S_) :- S_ evalto S / T.
accelspd(_,none,_,_,0).
accel(H,V,T) :- retract(bullet(B)),accelspd(B,H,mx,T,HS),accelspd(B,V,my,T,VS),asserta(bullet(B.put(accel,(HS,VS,T)))).

vanish :- shift(1).
repeat(N,_) :- N1 evalto N, N1 =< 0, !.
repeat(N,As) :- action(As),N1 evalto N - 1,repeat(N1,As).
text(T) :- T2 evalto T,!, format(atom(T3),'~w',[T2]),writeln(T3).
runBullet(B,Bs1,Bs1_) :-
  asserta(bullet(B.del(cont))),
  reset(B.cont,R,Cont),retract(bullet(B1)),
  (Cont=0,!,Bs1_=[B1.put(cont,wait(99999))|Bs1],dispBullet(B1)
  ; R= 1,!, freeBullet(B.shape),Bs1_=Bs1
  ; Bs1_=[B1.put(cont,Cont)|Bs1],dispBullet(B1)).
:- assert(move_cnt(0)).
move([]).
move(Bs) :-
  move_cnt(N),N1 is N+1,assertz(move_cnt(N1)),retract(move_cnt(_)),
  (N1 > 600 -> assertz(move_cnt(0)),retract(move_cnt(_)),true;
  foldl(runBullet,Bs,[],Bs1),!,
  reverse(Bs1,Bs1_),
  findall(B,retract(bullet(B)),Bs2),
  append(Bs1_,Bs2,Bs3),
  findall(R,retract(o(R)),Rs),atomic_list_concat(Rs,Rs_),!,
%  writeln(Rs),
  get_message(Msg),!,
  (Msg=exit -> true
  ;assert_ship(Msg.data),send_message(Rs_),!,move(Bs3))).

dispBullet(B) :-
  X is floor(B.x),Y is floor(B.y),
  X1 is X mod 256,Y1 is Y mod 256,
  Z is (X div 256) + (Y div 256)*2 + B.shape*4,
  atom_codes(A,[X1,Y1,Z]),
  assert(o(A)).
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
:- use_module(syntax,[]).
setColor(K) :- retract(color_cnt(C)),C1 is C+1,asserta(color_cnt(C1)),asserta(color(K,C)).
run(bulletML(Mode,Ds)) :-
  assert(color(a,a)),assert(color_cnt(a)),retractall(color(_,_)),retract(color_cnt(_)),
  asserta(color(normal,0)),asserta(color(top,1)), asserta(color_cnt(2)),
  (syntax:t(bulletML(Mode,Ds)),!; writeln(syntax:error),halt),!,
  writeln(syntax:ok),
  setRank(1),
  get_time(Time),assertz(time1(Time)),
  setDefs(Ds),
  newBullet(150,100,B,top),
  (member(A:action(As),Ds),atom_concat(top,_,A),writeln(A:B),!,
  move([B.put([dir:0,spd:0,cont:action(As)])]);true).
runfile(F) :-
  read_file_to_terms(F,[BML],[]),
  text(read:F),!,run(BML).
run_string(S) :-
  read_term_from_atom(S,T,[]),run(T).


main :-
  catch((
    send_message(''),
    get_message(Msg),assert_ship(Msg.data),
    findall(F,(name(_,F)),Fs),
    length(Fs,L),random(0,L,N),
    nth0(N,Fs,Name),
    runfile(Name),
    main
  ),Err,(writeln(Err),retractall(bullet(_)))).

name(0,'../bulletpl/[ESP_RADE]_round_5_alice_clone.xml'). %ok
name(0,'../bulletpl/[Original]_backfire.xml'). %ok
name(0,'../bulletpl/[Original]_guruguru.xml'). %ok
name(0,'../bulletpl/[Original]_housya.xml'). %ok
name(0,'../bulletpl/[Original]_progear_cheap_fake.xml'). %ok
name(0,'../bulletpl/[Original]_zako_atack.xml'). %ok
name(0,'../bulletpl/[Bulletsmorph]_aba_4.xml'). %ok
name(0,'../bulletpl/[Daiouzyou]_round_4_boss_1.xml'). %ok
name(0,'../bulletpl/[Daiouzyou]_round_4_boss_2.xml'). %ok
name(0,'../bulletpl/[Daiouzyou]_round_6_boss_5.xml'). %ok
name(0,'../bulletpl/[Dodonpachi]_kitiku_2.xml'). %ok
name(0,'../bulletpl/[ESP_RADE]_round_5_boss_gara_2.xml'). %ok
name(0,'../bulletpl/[Garegga]_black_heart_mk2_winder.xml'). %ok
name(0,'../bulletpl/[Progear]_round_5_boss_last_round_wave.xml'). %ok
name(0,'../bulletpl/[Progear]_round_5_middle_boss_rockets.xml'). %ok
name(0,'../bulletpl/[Progear]_round_9_boss.xml'). %ok1
name(0,'../bulletpl/[SilverGun]_4D_boss_PENTA.xml'). %ok
name(0,'../bulletpl/[XEVIOUS]_garu_zakato.xml'). %ok
name(a,'../bulletpl/zakato.xml'). %ok
name(0,'../bulletpl/[Original]_kagome.xml'). %ok
name(0,'../bulletpl/[Original]_knight_1.xml'). %ok
name(0,'../bulletpl/[tenmado]_5_boss_1.xml'). %ok
name(0,'../bulletpl/[Bulletsmorph]_aba_1.xml'). %ok
name(0,'../bulletpl/[Bulletsmorph]_aba_3.xml'). %ok
name(0,'../bulletpl/[Daiouzyou]_hibachi_1.xml'). %ok
name(0,'../bulletpl/[Daiouzyou]_hibachi_2.xml').
name(0,'../bulletpl/[Daiouzyou]_hibachi_4.xml').
name(0,'../bulletpl/[ESP_RADE]_round_5_boss_gara_4.xml').
name(0,'../bulletpl/[Guwange]_round_4_boss_eye_ball.xml'). % ok
name(0,'../bulletpl/[Ketui_LT]_3boss_kunekune.xml'). %ok ayashi
name(0,'../bulletpl/[MDA]_acc_n_dec.xml'). %ok
name(0,'../bulletpl/[MDA]_double_w.xml'). %ok
name(0,'../bulletpl/[MDA]_gnnnyari.xml'). %ok
name(0,'../bulletpl/[MDA]_mojya.xml'). %ok
name(0,'../bulletpl/[Noiz2sa]_bit.xml'). %ok
name(0,'../bulletpl/[Original]_dokkaan.xml'). %ok
name(0,'../bulletpl/[Original]_hasami.xml'). %ok
name(0,'../bulletpl/[Original]_knight_3.xml'). %ok
name(0,'../bulletpl/[Original]_oogi_hutatsu.xml'). %ok
name(0,'../bulletpl/[Original]_sakuretudan.xml'). %ok
name(0,'../bulletpl/[Original]_star_in_the_sky.xml'). %ok
name(0,'../bulletpl/[Original]_stone6.xml'). %ok
name(0,'../bulletpl/[Original]_tsunami.xml'). %ok
name(0,'../bulletpl/[OtakuTwo]_dis_bee_3.xml'). %ok
name(0,'../bulletpl/[OtakuTwo]_self-1020.xml'). %ok
name(0,'../bulletpl/[Progear]_round_1_boss_grow_bullets.xml'). %ok ayashi
name(0,'../bulletpl/[Progear]_round_2_boss_struggling.xml'). %ok ayashi
name(0,'../bulletpl/[Psyvariar]_4-D_boss_MZIQ.xml'). %ok
name(0,'../bulletpl/[Psyvariar]_X-A_boss_opening.xml'). %ok
name(0,'../bulletpl/[Psyvariar]_X-B_colony_shape_satellite.xml'). %ok
name(0,'../bulletpl/[Strikers1999]_hanabi.xml'). %ok

name(1,'../bulletpl/[Bulletsmorph]_aba_2.xml').
name(1,'../bulletpl/[Bulletsmorph]_aba_6.xml').
name(1,'../bulletpl/[Bulletsmorph]_convergent.xml').
name(1,'../bulletpl/[Bulletsmorph]_double_seduction.xml').
name(1,'../bulletpl/[Daiouzyou]_round_3_boss.xml').
name(1,'../bulletpl/[Daiouzyou]_round_4_boss.xml').
name(1,'../bulletpl/[Daiouzyou]_round_5_boss_2.xml').
name(1,'../bulletpl/[Daiouzyou]_round_6_boss_1.xml').
name(1,'../bulletpl/[Daiouzyou]_round_6_boss_3.xml').
name(1,'../bulletpl/[Daiouzyou]_round_6_boss_4.xml').
name(1,'../bulletpl/[ESP_RADE]_round_5_boss_gara_3.xml').
name(1,'../bulletpl/[Guwange]_round_3_boss_fast_3way.xml').
name(1,'../bulletpl/[Original]_accusation.xml').
name(1,'../bulletpl/[Original]_balloon_bomb.xml').
name(1,'../bulletpl/[Original]_btb_1.xml').
name(1,'../bulletpl/[Original]_btb_2.xml').
name(1,'../bulletpl/[Original]_btb_3.xml').
name(1,'../bulletpl/[Original]_btb_4.xml').
name(1,'../bulletpl/[Original]_btb_5.xml').
name(1,'../bulletpl/[Original]_btb_6.xml').
name(1,'../bulletpl/[Original]_censored.xml').
name(1,'../bulletpl/[Original]_chimera.xml').
name(1,'../bulletpl/[Original]_ellipse_bomb.xml').
name(1,'../bulletpl/[Original]_fujin_ranbu_fake.xml').
name(1,'../bulletpl/[Original]_fujin_ranbu_true.xml').
name(1,'../bulletpl/[Original]_hajike.xml').
name(1,'../bulletpl/[Original]_knight_2.xml').
name(1,'../bulletpl/[Original]_knight_4.xml').
name(1,'../bulletpl/[Original]_light_lv10.xml').
name(1,'../bulletpl/[Original]_light_lv25.xml').
name(1,'../bulletpl/[Original]_light_max.xml').
name(1,'../bulletpl/[Original]_pan.xml').
name(1,'../bulletpl/[Original]_water_lv10.xml').
name(1,'../bulletpl/[Original]_water_max.xml').
name(1,'../bulletpl/[OtakuTwo]_self-2010.xml').
name(1,'../bulletpl/[OtakuTwo]_self-2011.xml').
name(1,'../bulletpl/[Progear]_round_3_boss_wave_bullets.xml').
name(1,'../bulletpl/[xsoldier]_8_boss_main.xml').
name(2,'../bulletpl/[G_DARIUS]_homing_laser.xml'). %ok ayashi
name(2,'../bulletpl/[1943]_rolling_fire.xml'). %ok1
%fspd_ok:
name(3,'../bulletpl/[ChaosSeed]_big_monkey_boss.xml').
%pspd_ok:
name(4,'../bulletpl/[Daiouzyou]_hibachi_3.xml').
name(4,'../bulletpl/[Daiouzyou]_round_6_boss_2.xml').
name(4,'../bulletpl/[MDA]_circular_sun.xml').
%pdir_ok:
name(5,'../bulletpl/[OtakuTwo]_dis_bee_1.xml').
name(5,'../bulletpl/[Dodonpachi]_hibachi.xml').
name(5,'../bulletpl/[ESP_RADE]_round_5_boss_gara_1_a.xml').
name(5,'../bulletpl/[Original]_gyakuhunsya.xml').
name(5,'../bulletpl/[OtakuTwo]_circle_fireworks.xml').
name(5,'../bulletpl/[OtakuTwo]_circle_fireworks2.xml').
name(5,'../bulletpl/[OtakuTwo]_circle_roll.xml').
name(5,'../bulletpl/[OtakuTwo]_roll_misago.xml').
name(5,'../bulletpl/[OtakuTwo]_self-0062.xml').
name(5,'../bulletpl/[OtakuTwo]_self-0063.xml').
%param_ok_evalSpdDir:
name(6,'../bulletpl/[MDA]_circular.xml').
name(6,'../bulletpl/[MDA]_circular_model.xml').
name(6,'../bulletpl/[Original]_shooting_star.xml').
name(6,'../bulletpl/[OtakuTwo]_circle_trap.xml').
%param_ok:
name(7,'../bulletpl/[tenmado]_3_boss_2.xml').
name(7,'../bulletpl/[ESP_RADE]_round_5_boss_ares_2.xml').
name(7,'../bulletpl/[MDA]_mossari.xml').
name(7,'../bulletpl/[Daiouzyou]_round_3_boss_last.xml').
name(7,'../bulletpl/[Dodonpachi]_kitiku_1.xml').
name(7,'../bulletpl/[Guwange]_round_2_boss_circle_fire.xml').
name(7,'../bulletpl/[MDA]_14b_2-3w.xml').
name(7,'../bulletpl/[MDA]_2f.xml').
name(7,'../bulletpl/[Original]_gurutyo.xml').
name(7,'../bulletpl/[Original]_two_cross.xml').
name(7,'../bulletpl/[Original]_uneri.xml').
name(7,'../bulletpl/[OtakuTwo]_slow_move.xml').
name(7,'../bulletpl/[MDA]_fukuro.xml').
name(7,'../bulletpl/[OtakuTwo]_restriction_stasis.xml').

%prolog_syntax_ok:
name(8,'../bulletpl/[ESP_RADE]_round_5_boss_gara_1_b.xml').
name(8,'../bulletpl/[ESP_RADE]_round_5_boss_kakusi_hakkyou.xml').
name(8,'../bulletpl/[MDA]_wind_cl.xml').
name(8,'../bulletpl/[Noiz2sa]_88way.xml').
name(8,'../bulletpl/[Original]_circle.xml').
name(8,'../bulletpl/[Original]_evil_eye.xml').
name(8,'../bulletpl/[Daiouzyou]_round_4_boss_5.xml').
name(8,'../bulletpl/[Daiouzyou]_round_5_boss_1.xml').
name(8,'../bulletpl/[ESP_RADE]_round_123_boss_izuna_fan.xml').
name(8,'../bulletpl/[ESP_RADE]_round_123_boss_izuna_hakkyou.xml').
name(8,'../bulletpl/[ESP_RADE]_round_123_boss_pelaboy_hakkyou.xml').
name(8,'../bulletpl/[ESP_RADE]_round_123_boss_satoru_5way.xml').
name(8,'../bulletpl/[G-Wange]_round_trip_bit.xml').
%pdir_kieru_ok:
name(9,'../bulletpl/[OtakuTwo]_dis_bee_2.xml').
name(9,'../bulletpl/[OtakuTwo]_self-0034.xml').
name(9,'../bulletpl/[OtakuTwo]_self-0071.xml').
name(9,'../bulletpl/[OtakuTwo]_self-2020.xml').
%top1_action_ok:
name(10,'../bulletpl/[Daiouzyou]_round_1_boss_hakkyou.xml').
name(10,'../bulletpl/[Dodonpachi]_kitiku_3.xml').
name(10,'../bulletpl/[Dodonpachi]_kitiku_5.xml').
name(10,'../bulletpl/[DragonBlaze]_nebyurosu_2.xml').
name(10,'../bulletpl/[G-Wange]_roll_gara.xml').
name(10,'../bulletpl/[GigaWing2]_akurimi.xml').
name(10,'../bulletpl/[Noiz2sa]_rollbar.xml').
name(10,'../bulletpl/[OtakuTwo]_dis_bee_hakkyou.xml').
name(10,'../bulletpl/[OtakuTwo]_self-0012.xml').
name(10,'../bulletpl/[OtakuTwo]_self-1021.xml').
name(10,'../bulletpl/[Progear]_round_10_boss_before_final.xml').
name(10,'../bulletpl/[Psyvariar]_X-A_boss_winder.xml').
name(10,'../bulletpl/[Daiouzyou]_round_3_boss_2.xml').
name(10,'../bulletpl/[Original]_hirahira.xml').
name(10,'../bulletpl/[Daiouzyou]_round_4_boss_4.xml').
name(10,'../bulletpl/[STORM_CALIBAR]_last_boss_double_roll_bullets.xml').
name(10,'../bulletpl/[Daiouzyou]_hibachi_image.xml').
name(10,'../bulletpl/[Daiouzyou]_hibachi_maybe.xml').
name(10,'../bulletpl/[Ketui_LT]_3boss_roll_and_aim.xml').
name(10,'../bulletpl/[Original]_wana.xml').
%heavy_ok:
name(11,'../bulletpl/[Ketui_LT]_2boss_winder_crash.xml'). % heavy
name(11,'../bulletpl/1943_rolling_fire.xml'). %heavy
name(11,'../bulletpl/[Bulletsmorph]_aba_5.xml'). %heavy
name(11,'../bulletpl/[MDA]_10flower_2.xml'). %heavy
name(11,'../bulletpl/[tenmado]_5_boss_3.xml'). %heavy
name(11,'../bulletpl/[Noiz2sa]_5_players.xml'). %heavy
%ng_heavy:
name(13,'../bulletpl/[Bulletsmorph]_aba_7.xml'). %heavy
name(13,'../bulletpl/[Original]_air_elemental.xml'). %heavy
name(13,'../bulletpl/[Original]_extinction.xml'). %heavy
name(13,'../bulletpl/[Original]_optic_seeker.xml'). %heavy
name(13,'../bulletpl/[Original]_time_twist.xml'). %heavy
name(13,'../bulletpl/[Original]_entangled_space.xml'). %heavy
name(13,'../bulletpl/[Original]_kedama.xml'). %heavy
name(13,'../bulletpl/[Original]_kujira.xml'). %heavy
name(13,'../bulletpl/[XII_STAG]_3b.xml'). %heavy
name(13,'../bulletpl/[Bulletsmorph]_kunekune_plus_homing.xml').
%accel_error:

name(14,'../bulletpl/[Bulletsmorph]_fallen_string.xml').
name(14,'../bulletpl/[ESP_RADE]_round_5_boss_gara_5.xml').
name(14,'../bulletpl/[Ketui_LT]_1boss_bit.xml').
name(14,'../bulletpl/[MDA]_75l-42.xml').
name(14,'../bulletpl/[Original]_cont_circle.xml').
name(14,'../bulletpl/[Original]_kunekune.xml').
name(14,'../bulletpl/[Original]_stop_and_run.xml').
name(14,'../bulletpl/[Original]_water_lv25.xml').
name(14,'../bulletpl/[Original]_yokokasoku.xml').
name(14,'../bulletpl/[OtakuTwo]_accel_jump.xml').
name(14,'../bulletpl/[Progear]_round_3_boss_back_burst.xml').
name(14,'../bulletpl/[Progear]_round_4_boss_fast_rocket.xml').
name(14,'../bulletpl/[Progear]_round_6_boss_parabola_shot.xml').
name(14,'../bulletpl/[OtakuTwo]_self-0081.xml').

%ng_none_top:
%name(15,'../bulletpl/[Daiouzyou]_round_1_boss.xml').
