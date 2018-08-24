:- new(@w, dialog(game)),
   send(@w, size, size(465,465)),
   send(@w, open),
   send(@w, display, new(@b, circle(30)), point(0,0)),
   send(@b, pen, 0),
   send(@b, fill_pattern, colour(gold)).

run(N) :-
 	send(@b, move, point(0, N)),
 	send(@w, flush),
 	sleep(0.01),
  N1 is N + 1,
 	run(N1).
:- catch(run(0),E,(E=error(existence_error(pce(object),@w),_);writeln(E))).
:- halt.
