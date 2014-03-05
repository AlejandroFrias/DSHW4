%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% The Philosopher Node
%% @author Alejandro Frias
%% @doc "philosopher" The Philosopher.

-module (philosopher).

-export ([main/1]).

main([Name | Neighbors]) ->
    try 
    _ = os:cmd("epmd -daemon"),
    net_kernel:start([list_to_atom(Name), shortnames]),
    register(philosopher, self()),
    dsutils:timestamp("My node name is '~s'", [node()]),
    Forks = [{dirty, N} || N <- Neighbors],
    joining(Neighbors, Forks)
  catch
    _:_ -> dsutils:timestamp("Error parsing command line parameters.")
  end,
  halt().

joining(Neighbors, Forks) ->
  ActualNeighbors = connect_to_neighbors(Neighbors),
  thinking(Neighbors, Forks, []).

thinking(Neighbors, Forks, Requests) -> 
  % handle_messages:
  %      request for fork
  %      become eating
  %      new neighbor joining
  %      current neighbor leaving
  %      you are leaving
  eating(Neighbors, Forks, []).

eating(Neighbors, Forks, Requests) ->
  % dirty all the forks
  % handle_messages:
  %      request for fork
  %      become eating
  %      new neighbor joining
  %      current neighbor leaving
  %      you are leaving



