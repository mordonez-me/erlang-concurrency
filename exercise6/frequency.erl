%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([start_server/0, start_supervisor/0, init_supervisor/0, supervisor/1]).

%% These are the start functions used to create and
%% initialize the server.

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      io:fwrite("Requestin allocate ~p~n", [Pid]),
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      io:fwrite("Requestin deallocate ~n", []),
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      io:fwrite("Requestin stop ~p~n", [Pid]),
      Pid ! {reply, stopped}
  end.

%% Functional interface

allocate() -> 
  frequency ! {request, self(), allocate},
  receive 
    {reply, Reply} -> Reply
  end.

deallocate(Freq) -> 
  frequency ! {request, self(), {deallocate, Freq}},
  receive 
    {reply, Reply} -> Reply
  end.

stop() -> 
  frequency ! {request, self(), stop},
  receive 
    {reply, Reply} -> Reply
  end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),                                               %%% ADDED
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),  %%% ADDED
  unlink(Pid),                                             %%% ADDED
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->                %%% FUNCTION ADDED
  case lists:keysearch(Pid,2,Allocated) of
    {value,{Freq,Pid}} ->
      NewAllocated = lists:keydelete(Freq,1,Allocated),
      {[Freq|Free],NewAllocated}; 
    false ->
      {Free,Allocated} 
  end.

% Starts the supervisor
start_supervisor() ->
  register(supervisor,
    spawn(frequency, init_supervisor, [])).

% Init supervisor and trap_exit true to capture exits
init_supervisor() ->
  process_flag(trap_exit, true),
  supervisor([]).

% Supervisor actor, reactos to startServer and exit messages
supervisor(Servers) ->
  receive 
    {startServer, Pid} -> 
      io:fwrite("Requesting a server start ~n",[]),
      % Spawn a new server and link it
      ServerPid = spawn_link(frequency, init, []),
      % Reply to actor who request the creation
      Pid ! {ok, ServerPid},
      % Loop adding the newly created server
      supervisor(Servers ++ [ServerPid]);
    {'EXIT', Pid, _Reason} ->
      io:fwrite("Server Pid=~p has been killed, restarting... ~n", [Pid]),
      % The server has been killed, remove from the servers lists
      Servers2 = lists:delete(Pid, Servers),
      % Create new server (this do not preserve state)
      ServerPid = spawn_link(frequency, init, []),
      % Loop adding the newly created server
      supervisor(Servers2 ++ [ServerPid])
  end.

% Creates a new server
start_server() ->
  supervisor ! {startServer, self()},
  receive 
    {ok, Pid} -> 
      io:fwrite("server with PID=~p started correctly ~n",[Pid]);
    {'EXIT', Pid, _Reason} ->                   %%% CLAUSE ADDED
      io:fwrite("Server Pid=~p has been killed ~n", [Pid])
  end.

% Usage:
% c(frequency),
% frequency:start_supervisor(),
% frequency:start_server().  <- This can be N times to create more servers

