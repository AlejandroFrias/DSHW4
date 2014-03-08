%Controller!

-module(controller).

-export([main/1]).

main([Test]) ->


twoPerson


expect_eating([]) -> true;
expect_eat(Rs) -> expect_atom(Rs, eating).

expect_gone([]) -> true;
expect_gone(Rs) -> expect_atom(Rs, gone).

expect_atom([], Atom) -> true;
	receive
		{R, Atom} ->
			expect_atom(Rs, Atom);
	after 10000 -> dsutils:log("Didn't receive ~p message from ref '~p'", [Atom, R])
	end.