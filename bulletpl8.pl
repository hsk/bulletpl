:- module(echo_server,[]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/websocket)).
:- http_handler(root(.),http_reply_from_files('.', []), [prefix]).
:- http_handler(root(sock),http_upgrade_to_websocket(sock, []),[]).
:- assert(websock(dummy)).
sock(WebSocket) :- retract(websock(_)),assertz(websock(WebSocket)),catch(main,Err,writeln(Err)),writeln(end).
server(Port) :- thread_create(http_server(http_dispatch, [port(Port)]),_).
:- current_prolog_flag(argv, Fs),writeln(Fs),assert(fs(Fs)),server(3030).
:- www_open_url('http://localhost:3030/index.html').


:- assertz(ship(ship{x:150,y:350})).
message(V) :-
  websock(WebSocket),
  atom_concat(a,V,V_),
  ws_send(WebSocket,binary(V_)),!,
  rcv(WebSocket),!.
rcv(WebSocket) :-
  ws_receive(WebSocket, R, [format(json),format(string)]),!,
  ( R.data = "" -> !,throw(close:socket)
  ; R.data = "z" -> !,throw(next)
  ; R.data = "s" -> !,txt(Txt),re_replace('/bulletpl/bulletpl','/bulletpl/examples',Txt,Txt2),writeln(Txt2),!,
    process_create(path(sdmkun),[Txt2],[]), rcv(WebSocket)
  ; !,assertz(ship(R.data)),retract(ship(_))).
text_message(V) :- websock(WebSocket),atom_concat(t,V,V_),ws_send(WebSocket,binary(V_)).

user:M.del(K) := M2 :- del_dict(K,M,_,M2);M=M2.
refmap(G,P,N,N1) :- N1 is N+1,P_ is P,member(N:P_,G),!.
refmap(_,P,N,N1) :- N1 is N+1,writeln(waring:parameter($N:P)).
actionRef(K,Ps) :- actionV(K,G,As),foldl(refmap(G),Ps,1,_),!,action(As).
action(As) :- maplist(call,As).
move1(B,B4) :-cd(B,D,B1),cs(B1,S,B2),acc(B2,B3),
  D_ is D/180*3.14159,mx(B3,X1,Y1),
  X is X1 + sin(D_)*S,Y is Y1 - cos(D_)*S,
  B4=B3.put([x:X,y:Y,d:D,s:S]).
wait(N) :- N1 is N, N1 =< 0, !.
wait(N) :- retract(bullet(B)),move1(B,B1),asserta(bullet(B1)),
          (cont(B1)->shift(0),N1 is N - 1, wait(N1);shift(1)).
cd(B,D,B1) :- (CD,T)=B.get(cd),
  D is B.d+CD,T1 is T - 1,
  (T1 =< 0,_=B.get(isAim) ->
              ship(Ship),d(B,dirAim(D),Ship,D1),
              B1 = B.del(cd).del(isAim).put(d,D1)
  ;T1 =< 0 -> B1 = B.del(cd)
  ;           B1 = B.put(cd,(CD,T1))).
cd(B,B.d,B).
cs(B,S,B1) :- (CS,T)=B.get(cs),
  S is B.s + CS, T1 is T - 1,
  (T1 =< 0 -> B1 = B.del(cs)
  ;           B1 = B.put(cs,(CS,T1))).
cs(B,B.s,B).
mx(B,X,Y) :- X=B.get(mx)+B.x,Y=B.my+B.y;X=B.x,Y=B.y.
my(B,X,Y) :- X=B.get(mx),Y=B.my;X=0,Y=0.
acc(B,B2) :- (_,_,T)=B.get(acc) -> T =< 0,!,B2=B.del(acc);B2=B.
acc(B,B2) :- (H,V,T)=B.get(acc),!,T1 is T - 1,Mx_ is B.mx+H,My_ is B.my+V,
                                  B2 = B.put([acc:(H,V,T1),mx:Mx_,my:My_]).
acc(_,_) :- writeln(acc:error).
cont(B) :- B.x >= 0, B.y >= 0, B.x =< 300, B.y =< 400. % 画面外で消える
fireRef(K,Ps) :- fireV(K,G,D,S,As),foldl(refmap(G),Ps,1,_),fire(D,S,As).
fire(D,S,bulletRef(K,Ps)) :-
  bulletV(K,G,D_,S_,As),foldl(refmap(G),Ps,1,_),fire1(D,S,bullet(D_,S_,As),K).
fire(D,S,B) :- fire1(D,S,B,normal).
selectParam(none,none,Default,Default).
selectParam(none,BulletP,_,BulletP).
selectParam(FireP,_,_,FireP).
fire1(FD,FS,bullet(BD,BS,As),K) :-
  selectParam(FD,BD,dirAim(0),D),selectParam(FS,BS,spdAbs(1),S),
  bullet(B),ship(Ship),
  s(B,S,S_),!,d(B,D,Ship,D_),!,newBullet(B.x,B.y,B2,K),
  retract(bullet(B1)),asserta(bullet(B1.put([fdir:D_,fspd:S_]))),
  move1(B2.put([d:D_,s:S_,cont:action(As)]),B3),
  (cont(B3),assertz(bullet(B3));true).
d(_,dirAbs(D),_,D_) :- D_ is D.
d(B,dirSeq(D),_,D_) :- D_ is B.get(fdir) + D.
d(B,dirRel(D),_,D_) :- D_ is B.d + D.
d(B,dirSeq(D),Ship,D_) :- d(B,dirAim(D),Ship,D_).
d(B,dirAim(D),Ship,D_) :- D_ is atan((B.x - Ship.x),(Ship.y - B.y))*180/pi - 180 + D.
s(_,spdAbs(S),S_) :- S_ is S*1.68.
s(B,spdSeq(S),S_) :- S_ is B.get(fspd) + S*1.68.
s(_,spdSeq(_),S_) :- S_ is 1.68.
s(B,spdRel(S),S_) :- S_ is B.s + S*1.68.
changeDirection(dirSeq(D),T) :- retract(bullet(B)),D_ is D,asserta(bullet(B.put(cd,(D_,T)))).
changeDirection(dirAbs(D),T) :- retract(bullet(B)),D_ is(D-B.d)/T,asserta(bullet(B.put(cd,(D_,T)))).
changeDirection(dirRel(D),T) :- retract(bullet(B)),T_ is floor(T),D_ is D/T_,asserta(bullet(B.put(cd,(D_,T_)))).
changeDirection(dirAim(D),T) :- retract(bullet(B)),ship(Ship),d(B,dirAim(D),Ship,D1),D2 is (D1-B.d),T_ is T,
                                D_ is (((floor(D2) + 180)mod 360)-180)/T_,asserta(bullet(B.put(cd,(D_,T)))).
changeSpeed(spdSeq(S),T) :- retract(bullet(B)),S_ is S*1.68,asserta(bullet(B.put(cs,(S_,T)))).
changeSpeed(spdAbs(S),T) :- retract(bullet(B)),S_ is(S*1.68-B.s)/T,asserta(bullet(B.put(cs,(S_,T)))).
changeSpeed(spdRel(S),T) :- retract(bullet(B)),S_ is S*1.68/T, asserta(bullet(B.put([cs:(S_,T),isAim:0]))).
accelspd(spdSeq(S),_,_,S_) :- S_ is S*1.68.
accelspd(spdAbs(S),M,T,S_) :- S_ is (S*1.68-M)/T.
accelspd(spdRel(S),_,T,S_) :- S_ is S*1.68 / T.
accelspd(none,_,_,0).
accel(H,V,T) :- retract(bullet(B)),my(B,X,Y),accelspd(H,X,T,HS),accelspd(V,Y,T,VS),asserta(bullet(B.put([mx:X,my:Y,acc:(HS,VS,T)]))).

vanish :- shift(1).
repeat(N,_) :- N1 is N, N1 =< 0, !.
repeat(N,As) :- action(As),N1 is N - 1,repeat(N1,As).
text(T) :- format(atom(T3),'~w',[T]),writeln(T3),text_message(T3).
runBullet(B,Bs1,Bs1_) :-
  asserta(bullet(B.del(cont))),
  reset(B.cont,R,Cont),retract(bullet(B1)),
  (Cont=0,!,Bs1_=[B1.put(cont,wait(99999))|Bs1],dispBullet(B1)
  ; R= 1,!, Bs1_=Bs1
  ; Bs1_=[B1.put(cont,Cont)|Bs1],dispBullet(B1)).
:- assert(move_cnt(0)).
move([]) :- !.
move(Bs) :-
  move_cnt(N),N1 is N+1,assertz(move_cnt(N1)),retract(move_cnt(_)),!,
  (N1 > 600 -> true;
  foldl(runBullet,Bs,[],Bs1),!,
  reverse(Bs1,Bs1_),
  findall(B,retract(bullet(B)),Bs2),
  append(Bs1_,Bs2,Bs3),
  findall(R,retract(o(R)),Rs),atomic_list_concat(Rs,Rs_),!,
  message(Rs_),!,move(Bs3)).

dispBullet(B) :-
  X is floor(B.x),Y is floor(B.y),
  X1 is X mod 256,Y1 is Y mod 256,
  Z is (X div 256) + (Y div 256)*2 + B.c*4,
  atom_codes(A,[X1,Y1,Z]),assert(o(A)).
newBullet(X,Y,b{c:C,x:X,y:Y},K) :- color(K,C).
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
setRank(N) :- V is N, retractall(rank(_)),asserta(rank(V)).
rankUp :- retract(rank(N)),N1 is N+1,asserta(rank(N1)).
:- use_module(syntax,[]).
setColor(K) :- retract(color_cnt(C)),C1 is C+1,asserta(color_cnt(C1)),asserta(color(K,C)).
run(bulletML(Mode,Ds)) :-
  assertz(move_cnt(0)),retract(move_cnt(_)),
  assert(color(a,a)),assert(color_cnt(a)),retractall(color(_,_)),retract(color_cnt(_)),retractall(bullet(_)),
  asserta(color(normal,0)),asserta(color(top,1)), asserta(color_cnt(2)),
  (syntax:t(bulletML(Mode,Ds)),!; writeln(syntax:error),halt),!,
  setRank(0.35),
  get_time(Time),assertz(time1(Time)),
  repExpr(Ds,Ds_),
  setDefs(Ds_),
  findall(B1,
    (member(A:action(As),Ds_),atom_concat(top,_,A),newBullet(150,100,B,top),B1=B.put([d:0,s:0,cont:action(As)])),Bs),
  move(Bs).
runfile(F) :- catch(read_file_to_terms(F,[BML],[]),_,fail),text(F),!,run(BML).
run_string(S) :- read_term_from_atom(S,T,[]),run(T).
repExpr($rank,Rank) :- rank(Rank).
repExpr($rand,random_float).
repExpr(E,E_) :- E=..[N|Ps],maplist(repExpr,Ps,Ps_),E_=..[N|Ps_].

main :-
  catch((
    message(''),!,
    (fs([Name2]),writeln(Name2)
    ; directory_files('bulletpl/',Fs),length(Fs,L),!,
      random(0,L,N),nth0(N,Fs,Name),atom_concat('bulletpl/',Name,Name2),exists_file(Name2)),
    retractall(txt(_)),absolute_file_name(Name2,R,[]),assert(txt(R)),
    runfile(Name2)
  ),next,true),!,main.
:- get0(_),halt.
