%Controller!

-module(controller).

-export([leave_while_hungry_test/1]).
-define (PHILOSOPHERS, ['a@amazonia', 'b@arden', 'c@ash']).
-define (COOKIE, 'philosopher').
-define (SEND_TO (NAME), {philosopher, NAME}).

leave_while_hungry_test(NumPhil) ->
  Phils = dsutils:first_n_elements(NumPhil, ?PHILOSOPHERS),
  send_become_hungry_commands(tl(Phils), []),
  RefsLeave = send_leave_commands(tl(Phils), []),
  expect_gone(RefsLeave)
  RefsHungry = send_become_hungry_commands([hd(Phils)]),
  expect_eating(RefsHungry).

send_become_hungry_commands(Phils, Refs) ->
  send(Phils, Refs, 'become_hungry').

send_leave_commands(Phils, Refs) ->
  send(Phils, Refs, 'leave').

send_stop_eating_commands(Phils, Refs) ->
  send(Phils, Refs, 'stop_eating').

send([], R, C) ->
  dsutils:log("All ~p commands.", [C]), R;
send([P | Ps], R, C) ->
  dsutils:log("Sending ~p to ~p", [C, P]),
  Ref = make_ref(),
  ?SEND_TO(P) ! {self(), Ref, C},
  send(Ps, [Ref | R], C).


expect_eat(Rs) -> expect_atom(Rs, eating).

expect_gone(Rs) -> expect_atom(Rs, gone).

expect_gone([], _) -> true;
expect_atom([R | Rs], Atom) -> true;
	receive
		{R, Atom} ->
			expect_atom(Rs, Atom);
	after 10000 -> dsutils:log("Didn't receive ~p message from ref '~p'", [Atom, R])
	end.
