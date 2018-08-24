:- use_module(bulletpl10).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/websocket)).
:- http_handler(root(.),http_reply_from_files('.', []), [prefix]).
:- http_handler(root(sock),http_upgrade_to_websocket(sock, []),[]).
sock(Sock) :- writeln(connect),retractall(websock(_)),assertz(websock(Sock)),catch(main,Err,writeln(Err)),ws_close(Sock),writeln(end).
:- thread_create(http_server(http_dispatch, [port(3030)]),_).
:- catch(fork_exec('browser/Browser1'),_,www_open_url('http://localhost:3030/index.html')).
%is(A,B) :- catch(A is B,E,writeln(E:A is B)).
message(V) :- websock(Sock),atom_concat(a,V,V_),ws_send(Sock,binary(V_)),!,rcv(Sock),!.
rcv(Sock) :-
  ws_receive(Sock, R, [format(json)]),!,
  ( R.data = "" -> !,throw(close:socket)
  ; R.data = "z" -> !,garbage_collect,throw(next)
  ; R.data = "s" -> !,txt(Txt),re_replace('/bulletpl/bulletpl','/bulletpl/examples',Txt,Txt2),writeln(Txt2),!,
    process_create(path(sdmkun),[Txt2],[]), rcv(Sock)
  ; !,nb_linkval(ship,R.data)).
text_message(V) :- atom_codes(V,C),utf8_codes(C,R,[]),atom_codes(A,[116|R]),websock(Sock),ws_send(Sock,binary(A)).
disp_bullet(Bs3) :- maplist(disp_bullet,Bs3,As),atomic_list_concat(As,Rs_),message(Rs_).
disp_bullet(B,A) :-
  X is floor(B.x),Y is floor(B.y),
  divmod(X,256,X1,X2),divmod(Y,256,Y1,Y2),
  Z is X1 + Y1*2 + B.c*4,
  atom_codes(A,[X2,Y2,Z]).

main :-
  catch((
    message(''),!,
    (retract(fs([Name2|Fs])),assert(fs(Fs))
    ; current_prolog_flag(argv, [Name2|Fs]),assert(fs(Fs))
    ; directory_files('bulletpl/',Fs),length(Fs,L),!,
      repeat,random(0,L,N),nth0(N,Fs,Name),atom_concat('bulletpl/',Name,Name2),exists_file(Name2)),
    absolute_file_name(Name2,R,[]),retractall(txt(_)),assert(txt(R)),
    runfile(Name2)
  ),next,true),!,main.
:- get0(_),halt.
