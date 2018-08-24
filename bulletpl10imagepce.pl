:- use_module(bulletpl10).
:- use_module(library(pce)).
:- assertz(objs([])).
text_message(V) :- send(@text,value,V).
disp_bullet(Bs3) :-
  get_time(Time),nb_getval(time,OTime),WTime is (1/60-(Time-OTime)),sleep(WTime),
  Time2 is Time+max(-1/60,WTime),nb_setval(time,Time2), 
  maplist(disp,Bs3,Bs),assertz(objs(Bs)),retract(objs(_)),(object(@w);throw(close)),
  in_pce_thread((send(@w,flush))),
  send(@img,clear),
  send(@img,draw_in,@view,point(0,0)),
  nb_getval(cnt,Cnt),Cnt1 is Cnt+1,nb_setval(cnt,Cnt1),
  format(atom(Name),'test_~05d.gif',[Cnt]),
  send(@img,save,Name,gif),
  getShip(Ship),nb_setval(ship,Ship),
  !.
disp(B,(X,Y,B.c)) :- ratio(Ratio),X is Ratio * B.x,Y is Ratio * B.y.
:- nb_setval(cnt,0).
ratio(1.5).
setShip(Ship) :- bulletpl10:nb_setval(ship,Ship).
:- pce_begin_class(canvas, graphical,canvas).
  initialise(S,W:width=int, H:height=int) :->
    send(S, send_super, initialise, 0, 0, W, H).
  '_redraw_area'(S, _A:area) :->
    send(S, save_graphics_state),
    objs(R),!,
    getShip(ship{x:X,y:Y}),
    ratio(Ratio),
    send(S, graphics_state, 0, none, colour(c,200*255,200*255,200*255)),
    maplist(view2(S),[(X*Ratio,Y*Ratio,3)|R]),
    send(S, graphics_state, 0, none, colour(c2,186*255,186*255,186*255)),
    maplist(view3(S),[(X*Ratio,Y*Ratio,3)|R]),
    maplist(view(S),[(X*Ratio,Y*Ratio,3)|R]),!,
    send(S, restore_graphics_state),
    send(S, send_super, redraw),
    !.
:- pce_end_class.

view(S,(X,Y,C)) :-
  (nth0(C,[black,red,green,blue,red,yellow,white],Col);Col=white),!,
  send(S, graphics_state, 0, none, Col),!,
  send(S, draw_fill, X-3, Y-3, 7, 7).
view2(S,(X,Y,_)) :- send(S, draw_fill, X-7, Y+15, 15, 15).
view3(S,(X,Y,_)) :- send(S, draw_fill, X-4, Y+18, 9, 9).
initWindow :-
  ratio(Ratio),
  new(@w, dialog(game)),
  send(@w, size, size(300*Ratio,400*Ratio)),
  new(@img,image(@nil,300*Ratio,400*Ratio,pixmap)),
  new(@bmp,bitmap(@img)),
  send(@w,display,new(@view,canvas(300*Ratio,400*Ratio))),
  send(@w,display,new(@text,text(game))),
  send(@w, open),
  send(@w,move,point(0,0)).
initShip :-
  ratio(Ratio),
  send(@w, display, new(@area, box(10000,10000))),
  send(@area, recogniser,move_gesture(left)),
  send(@area, center, point(150*Ratio,350*Ratio)).
moveShip :-
  ratio(Ratio),
  get(@area,center,point(PX,PY)),
  X is max(15,min(300*Ratio-15,PX)),Y is max(15,min(400*Ratio-15,PY)),
  send(@area,center,point(X,Y)).
getShip(ship{x:X_,y:Y_}) :- moveShip,ratio(Ratio),get(@area,center,point(X,Y)),X_ is X/Ratio,Y_ is Y/Ratio.

main :-
  catch((
    (retract(fs([Name2|Fs])),assert(fs(Fs))
    ; current_prolog_flag(argv, [Name2|Fs]),assert(fs(Fs))
    ; directory_files('bulletpl/',Fs),length(Fs,L),!,
      repeat,random(0,L,N),nth0(N,Fs,Name),atom_concat('bulletpl/',Name,Name2),exists_file(Name2)),
    absolute_file_name(Name2,R,[]),retractall(txt(_)),assert(txt(R)),
    runfile(Name2)
  ),next,true),!,(current_prolog_flag(argv, []),main;true).
:- catch((initWindow,initShip,get_time(Time),nb_setval(time,Time),main),close,true).
:- writeln("create anim.gif").
:- process_create(path(convert),['-delay','2','-loop','0','test_*.gif','anim.gif'],[]).
:- process_create(rm,['test_*.gif']).
:- halt.
