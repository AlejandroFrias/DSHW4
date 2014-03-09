%Controller!

-module(controller).

-export([leave_while_hungry_test/1, everyone_eats/1, setup/0]).
-define (PHILOSOPHERS, ['a@ash', 'b@birnam', 'c@clover', 'd@dittany', 'e@elm']).
-define (SEND_TO (NAME), {philosopher, NAME}).

setup() ->
  _ = os:cmd("epmd -daemon"),
  {_, _, Micro} = now(),
  net_kernel:start([list_to_atom("controller" ++ integer_to_list(Micro)), shortnames]),
  erlang:set_cookie(node(), 'philosopher').

% Makes everyone but the first guy become hungry. Then tells everyone but the 
% first guy to leave. Then tells the last remaining guy to become hungry and
% we expect he'll eat right away. This tests that people leave and say goodbye
% properly
leave_while_hungry_test(NumPhil) ->
  setup(),
  Phils = dsutils:first_n_elements(NumPhil, ?PHILOSOPHERS),
  send_become_hungry_commands(tl(Phils), []),
  RefsLeave = send_leave_commands(tl(Phils), []),
  expect_gone(RefsLeave),
  RefsHungry = send_become_hungry_commands([hd(Phils)], []),
  expect_eating(RefsHungry),
  RefFinal = send_leave_commands([hd(Phils)], []),
  expect_gone(RefFinal).

% Makes sure that everyone who is hungry gets to eat eventually
everyone_eats(NumPhil) ->
  setup(),
  Phils = dsutils:first_n_elements(NumPhil, ?PHILOSOPHERS),
  Eating = send_become_hungry_commands(Phils, []),
  Thinking = send_stop_eating_commands(Phils, []),
  expect_eating(Eating),
  expect_thinking(Thinking),
  Gone = send_leave_commands(Phils, []),
  expect_gone(Gone).



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


expect_eating(Rs) -> expect_atom(Rs, eating).

expect_gone(Rs) -> expect_atom(Rs, gone).

expect_thinking(Rs) -> expect_atom(Rs, thinking).

expect_atom([], Atom) -> 
  dsutils:log("All ~p atoms.", [Atom]), Atom;
expect_atom([R | Rs], Atom) ->
	receive
		{R, Atom} ->
			expect_atom(Rs, Atom)
	after 10000 -> 
    dsutils:log("ERROR: Didn't receive ~p message from ref '~p'", [Atom, R])
	end.
