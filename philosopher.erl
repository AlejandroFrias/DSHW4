%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% The Philosopher Node
%% @author Alejandro Frias, Ravi Kumar
%% @version 2014-03-07
%% @doc "philosopher" The Philosopher as in Dining Philosophers problem, but 
%%                    with added capability of adding and removing philosophers 
%%                    from the system.

-module(philosopher).

-export([main/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Exported Functions                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starts up the philosopher and sets it to JOINING state initially
main([Name | Neighbors]) ->
  try 
    % Erlang networking boilerplate 
    _ = os:cmd("epmd -daemon"),
    net_kernel:start([list_to_atom(Name), shortnames]),
    register(philosopher, self()),
    %Set the cookie to PHILOSOPHER (was causing an error when I tried to connect to my partner's nodes)
    erlang:set_cookie(node(), 'philosopher'),
    dsutils:log("My node name is '~s'", [node()]),
    N = [list_to_atom(X) || X <- Neighbors],
    joining(N) % We begin in the joining state
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
  dsutils:log("STATE: JOINING -> THINKING."),
  thinking(Neighbors, [{dirty, F} || F <- Neighbors], []).


% THINKING state
%   First will process any unprocessed fork request requests that were stored 
%   when we had priority
% Receives:
%   Join Request - Responds with a Join Accept after adding the new neighbor to 
%                  the list of neighbors
%   Fork Request - Sends the fork and removes it from the list of forks
%   Goodbye Message - Removes the philosopher and the corresponding fork
%   Become Hungry Command - Sends fork requests for missing forks and becomes HUNGRY
%   Leave Command - Says goodbye to everyone and becomes LEAVING
%   
thinking(Neighbors, Forks, []) ->
  receive
    {Name, fork_request} ->
      dsutils:log("Received fork request from ~p.", [Name]),
      %As our algorithm is implemented, a node can never receive a request for a fork it doesn't have
      F = send_fork(Name, Forks),
      thinking(Neighbors, F, []);

    {Name, join_request} ->
      N = [Name | Neighbors],
      dsutils:log("Received Join Request from ~p. My neighbors are now ~p.", [Name, N]),
      dsutils:log("Sending Join Accept to ~p.", [Name]),
      {philosopher, Name} ! {node(), join_accept},
      thinking(N, Forks, []);

    {Name, goodbye} ->
      dsutils:log("Received goodbye notification from ~p.", [Name]),
      N = lists:delete(Name, Neighbors),
      F = lists:delete({dirty, Name}, Forks),
      thinking(N, F, []);

    {Pid, Ref, become_hungry} ->
      dsutils:log("Received command to become HUNGRY. Sending fork requests."),
      N = [X || X <- Neighbors, not(lists:member({dirty, X}, Forks))],
      dsutils:log("Sending Fork Requests to: ~p", [N]),
      send_fork_requests(N),
      dsutils:log("STATE: THINKING -> HUNGRY"),
      hungry(Neighbors, Forks, [], Pid, Ref);

    {Pid, Ref, leave} ->
      dsutils:log("Received Leave command from external controller."),
      send_goodbye_messages(Neighbors),
      dsutils:log("STATE: THINKING -> LEAVING"),
      leaving(Neighbors, Forks, Pid, Ref)
  end;

% Process the stored fork requests
% If we get fork requests when hungry but have priority, we save them for later,
% and deal with the requests here (which is only called when transitioning from EATING->THINKING)
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
      dsutils:log("STATE: HUNGRY -> EATING"),
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
          dsutils:log("Received goodbye notification from ~p.", [p]),
          N = lists:delete(Name, Neighbors),
          dsutils:log("Neighbors left: ~p.", [N]),
          F = lists:delete({clean, Name}, lists:delete({dirty, Name}, Forks)), % deletes the clean or dirty fork
          dsutils:log("Forks left: ~p.", [F]),
          hungry(N, F, CleanForkRequests, ECPid, ECRef);

        {Pid, Ref, leave} ->
          dsutils:log("Received Leave command from external controller."),
          send_goodbye_messages(Neighbors),
          dsutils:log("STATE: HUNGRY -> LEAVING"),
          leaving(Neighbors, Forks, Pid, Ref)
      end 
  end.

% EATING state
% Receives:
%   Stop Eating - Message from the controller to stop eating. Immediately dirties
%     all forks and goes to thinking, which will send out forks
%   Leave Command - Say goodbye to everyone and start LEAVING state
%   
eating(Neighbors, Forks, CleanForkRequests) ->
    receive
    {Pid, Ref, stop_eating} ->
      dsutils:log("Received message from external controller to stop eating."),
      DirtyForks = [{dirty, F} || {_, F} <- Forks],
      dsutils:log("STATE: EATING -> THINKING"),
      dsutils:log("Sending thinking notification to external controller"),
      Pid ! {Ref, thinking},
      thinking(Neighbors, DirtyForks, CleanForkRequests);

    {Pid, Ref, leave} ->
      dsutils:log("Received message to leave the network."),
      send_goodbye_messages(Neighbors),
      dsutils:log("STATE: EATING -> LEAVING"),
      leaving(Neighbors, Forks, Pid, Ref)
    end.

% LEAVING state
% Receives no possible messages
% Sends a message to the controller that we're gone

leaving(_, _, ECPid, ECRef) ->
  dsutils:log("Sent goodbyes."),
  ECPid ! {ECRef, gone},
  dsutils:log("STATE: LEAVING -> GONE"),
  gone().

% GONE state
%   Receives no messages!
gone() -> 
  dsutils:log("Quitting..."),
  halt().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Helper Functions                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sends the join requests to all neighbors
send_join_requests([]) ->
  dsutils:log("All Join Requests Sent.");
send_join_requests([N | Ns]) ->
  dsutils:log("Sending a Join Request to ~p.", [N]),
  {philosopher, N} ! {node(), join_request},
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
      dsutils:log("Received a Goodbye Message from ~p.", [Name]),
      W = lists:delete(Name, Waiting),
      N = lists:delete(Name, Neighbors),
      confirm_join(N, W)
  end.
  
% Sends a fork request to each neighbor in the list
send_fork_requests([]) ->
  dsutils:log("All fork requests sent.");
send_fork_requests([N | Ns]) ->
  dsutils:log("Sending fork request to ~p", [N]),
  {philosopher, N} ! {node(), fork_request},
  send_fork_requests(Ns).

% Sends a goodbye message to each neighbor in the list
send_goodbye_messages([]) ->
  dsutils:log("Said all my goodbyes.");
send_goodbye_messages([N | Ns]) ->
  dsutils:log("Saying goodbye to ~p", [N]),
  {philosopher, N} ! {node(), goodbye},
  send_goodbye_messages(Ns).

%Check to see if we have all the forks. This just checks if the number of forks we need
% is the same as the number of forks we have, as we will never have a fork we don't need while hungry.
have_all_forks(N, F) ->
  length(N) == length(F).

% Sends a fork to Name.
% Returns the list of Forks we have (with the sent fork removed).
send_fork(Name, Forks) ->
  dsutils:log("Sending a fork to ~p.", [Name]),
  {philosopher, Name} ! {node(), fork},
  lists:delete({dirty, Name}, Forks).
