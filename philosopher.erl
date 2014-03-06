%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% The Philosopher Node
%% @author Alejandro Frias
%% @doc "philosopher" The Philosopher.

-module (philosopher).

-export ([main/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Exported Functions                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main([Name | Neighbors]) ->
  try 
    _ = os:cmd("epmd -daemon"),
    net_kernel:start([list_to_atom(Name), shortnames]),
    register(philosopher, self()),
    dsutils:log("My node name is '~s'", [node()]),
    joining(Neighbors) % initially joining
  catch
    _:_ -> dsutils:log("Error parsing command line parameters.")
  end,
  halt().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   States                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% JOINING state
% Sends out join requests. Once all requests have been confirmed, begins THINKING.
% Receives:
%   Join Accept - Mark that neighbor has having accepted you (by removing it from Waiting list)
%   Goodbye Message - Remove that leaving neighbor from both lists (Neighbors and Waiting)
%   
joining(N) ->
  send_join_requests(N),
  Neighbors = confirm_join(N, N),
  dsutils:log("Becoming THINKING."),
  thinking(Neighbors, [{dirty, F} || F <- Neighbors], []).


% THINKING state
%   First will process any unprocessed fork request requests that were stored when we had priority
% Receives:
%   Join Request - Responds with a Join Accept after adding the new neighbor to the list of neighbors
%   Fork Request - Sends the fork and removes it from the list of forks
%   Goodbye Message - Removes the philosopher and the corresponding fork
%   Become Hungry Command - Sends fork requests for missing forks and becomes HUNGRY
%   Leave Command - Says goodbye to everyone and becomes LEAVING
%   
thinking(Neighbors, Forks, []) ->
  receive
    {Name, join_request} ->
      dsutils:log("Received Join Request from ~p. Adding them.", [Name]),
      N = [Name | Neighbors],
      {philosopher, Name} ! {node(), join_accept},
      thinking(N, Forks, []);
    {Name, fork_request} ->
      dsutils:log("Received fork request from ~p.", [Name]),
      F = send_fork(Name, Forks),
      thinking(Neighbors, F, []);
    {Name, goodbye} ->
      dsutils:log("Received Leave command from external controller."),
      N = lists:delete(Name, Neighbors),
      F = lists:delete({dirty, Name}, Forks),
      thinking(N, F, []);
    {Pid, Ref, become_hungry} ->
      dsutils:log("Received command to become HUNGRY. Sending fork requests."),
      N = [X || X <- Neighbors, not(lists:member({dirty, X}, Forks))],
      send_fork_requests(N),
      hungry(Neighbors, Forks, [], Pid, Ref);
    {Pid, Ref, leave} ->
      dsutils:log("Received Leave command from external controller."),
      send_goodbye_messages(Neighbors),
      leaving(Neighbors, Forks, Pid, Ref)
  end;
thinking(Neighbors, Forks, [C | Cs]) ->
  F = send_fork(C, Forks),
  thinking(Neighbors, F, Cs).


% HUNGRY state
% Receives:
%   Fork Request - if it's dirty, send it and remove it from list of forks
%                  if it's clean, I have priority, so store the request for later
%   Fork - Add the fork to the list of forks
%   Goodbye Message - remove philosopher and fork from list.
%   Leave Command - Say goodbye to everyone and start LEAVING state
%   
hungry(Neighbors, Forks, CleanForkRequests, ECPid, ECRef) ->
  case have_all_forks(Neighbors, Forks) of
    true ->
      dsutils:log("I have all the forks. Starting to eat now."),
      ECPid ! {ECRef, eating},
      F = [{dirty, X} || {_, X} <- Forks], % dirty all the forks
      eating(Neighbors, F, CleanForkRequests);
    false ->
      receive
        {Name, fork_request} ->
          dsutils:log("Received a fork request from ~p.", [Name]),
          case lists:member({dirty, Name}, Forks) of
            true ->
              F = send_fork(Name, Forks),
              hungry(Neighbors, F, CleanForkRequests, ECPid, ECRef);
            false ->
              dsutils:log("I have priority. My fork is clean. Storing request for later."),
              C = [Name | CleanForkRequests],
              hungry(Neighbors, Forks, C, ECPid, ECRef)
          end;
        {Name, fork} ->
          dsutils:log("Received fork from ~p.", [Name]),
          F = [{clean, Name} | Forks],
          hungry(Neighbors, F, CleanForkRequests, ECPid, ECRef);
        {Name, goodbye} ->
          dsutils:log("Goodbye ~p.", [Name]),
          N = lists:delete(Name, Neighbors),
          F = lists:delete({dirty, Name}, Forks),
          hungry(N, F, CleanForkRequests, ECPid, ECRef);
        {Pid, Ref, leave} ->
          dsutils:log("Received Leave command from external controller."),
          send_goodbye_messages(Neighbors),
          leaving(Neighbors, Forks, Pid, Ref)
      end 
  end.

eating(Neighbors, Forks, CleanForkRequests) ->
  dsutils:log("Not yet implemented.").

leaving(Neighbors, Forks, ECPid, ECRef) ->
  dsutils:log("Not yet implemented.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Helper Functions                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sends the join requests to all neighbors
send_join_requests([]) ->
  dsutils:log("All Join Requests Sent.");
send_join_requests([N | Ns]) ->
  dsutils:log("Sending a Join Request to ~p.", [N]),
  {philosopher, N} ! {self(), node(), join_request},
  send_join_requests(Ns).

% Receives all the Join Accepts, or Goodbye Messages instead
confirm_join(N, []) ->
  dsutils:log("All Join Accepts have been received."), N;
confirm_join(Neighbors, Waiting) ->
  receive
    {Name, join_accept} -> 
      dsutils:log("Received a Join Accept from ~p. Removed ~p from ~p.", [Name, Name, Waiting]),
      W = lists:delete(Name, Waiting),
      confirm_join(Neighbors, W);
    {Name, goodbye} ->
      dsutils:log("Received a Goodbye Message from ~p.", []),
      W = lists:delete(Name, Waiting),
      N = lists:delete(Name, Neighbors),
      confirm_join(N, W)
  end.
  
% Sends a fork request to each neighbor in the list
send_fork_requests([]) ->
  dsutils:log("All fork requests sent.");
send_fork_requests([N | Ns]) ->
  dsutils:log("Sending fork request to ~p", [N]),
  {philosopher, N} ! {self(), node(), fork_request},
  send_fork_requests(Ns).

% Sends a goodbye message to each neighbor in the list
send_goodbye_messages([]) ->
  dsutils:log("Said all my goodbyes.");
send_goodbye_messages([N | Ns]) ->
  dsutils:log("Saying goodbye to ~p", [N]),
  {philosopher, N} ! {node(), goodbye},
  send_goodbye_messages(Ns).

% TODO: Currently just comparing size of lists. Ideally we would check that
% they match properly. Possibly not necessary
have_all_forks(N, F) ->
  length(N) == length(F).

% Sends a fork for Name to Pid.
% Returns a list of Forks with the sent fork removed.
send_fork(Name, Forks) ->
  dsutils:log("Sending a fork to ~p.", [Name]),
  {philosopher, Name} ! {Name, fork},
  lists:delete({dirty, Name}, Forks).