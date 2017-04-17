%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([allocate_on_server/1,deallocate_from_server/2,stop_server/1,pick_server/0, get_servers/0, send_wrong_message/1]).
-export([init/0]).
-export([start_servers/0, start_supervisor/0, init_supervisor/0, supervisor/1]).

%% These are the start functions used to create and
%% initialize the server.

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

get_servers() -> [server1, server2, server3].

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
      % Handle error on deallocating to return a message
      try deallocate(Frequencies, Freq) of 
        NewFrequencies -> 
          Pid ! {reply, ok},
          loop(NewFrequencies)
      catch 
        throw:frequency_not_found ->
          Pid ! {reply, frequency_not_found},
          loop(Frequencies)
      end;
    {request, Pid, stop} ->
      io:fwrite("Requestin stop ~p~n", [Pid]),
      Pid ! {reply, stopped};
    _Msg -> 
      io:fwrite("Received unknown message... ~n", [])
  end.

%% Functional interface

allocate_on_server(Server) -> 
  Server ! {request, self(), allocate},
  receive 
    {reply, Reply} -> Reply
  end.

deallocate_from_server(Server, Freq) ->
  Server ! {request, self(), {deallocate, Freq}},
  receive 
    {reply, frequency_not_found} -> 
      io:fwrite("Frequency not found... ~n", []);
    {reply, Reply} -> 
      Reply
  end.

stop_server(Server) -> 
  Server ! {request, self(), stop},
  receive 
    {reply, Reply} -> Reply
  end.

send_wrong_message(Server) ->
  Server ! wrong_message_format,
  io:fwrite("Sending unknown message ~n", []).

pick_server() ->
  Index = rand:uniform(length(get_servers())),
  lists:nth(Index,get_servers()).


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),                                               %%% ADDED
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  % Throw an error if the frequency do not exists
  case lists:keysearch(Freq,1,Allocated) of
    {value,{Freq,Pid}} ->
      unlink(Pid),           
      NewAllocated=lists:keydelete(Freq, 1, Allocated),
      {[Freq|Free],  NewAllocated};
    false -> 
      throw(frequency_not_found)
  end.

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
    {startServer, Pid, ServerName} -> 
      io:fwrite("Requesting a server start ~n",[]),
      % Spawn a new server, link it and register with a name
      ServerPid = spawn_link(frequency, init, []),
      register(ServerName, ServerPid),
      % New servers
      Servers2 = Servers ++ [ServerPid],
      % Reply to actor who request the creation
      Pid ! {ok, ServerPid, Servers2},
      % Loop adding the newly created server
      supervisor(Servers2);
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
start_servers() ->
  ServersName = get_servers(),
  Self = self(),
  CreateServer = 
    fun(CreateServer, Servers, Pid) ->
      case Servers of
        [Head|Tail] -> 
          io:fwrite("Call to supervisor to start a server from PID=~p~n",[Pid]),
          supervisor ! {startServer, Pid, Head},
          CreateServer(CreateServer, Tail, Pid);
        [] -> ok
      end
    end,
  CreateServer(CreateServer, ServersName, Self).

% Usage:
% c(frequency),
% frequency:start_supervisor(),                         <- Start supervisor
% frequency:start_servers(),                            <- Start multiple servers
% Servers = frequency:get_servers(),                    <- Get servers names
% Server1 = lists:nth(1, Servers),                      <- Get first server
% Server2 = lists:nth(2, Servers),                      <- Get second server
% {ok, Freq1} = frequency:allocate_on_server(Server1),  <- Get one frequency on Server1
% {ok, Freq2} = frequency:allocate_on_server(Server2),  <- Get one frequency on Server2
% frequency:deallocate_from_server(Server1, Freq1),     <- Deallocate freq1 from server 1
% frequency:deallocate_from_server(Server1, Freq2),     <- Deallocate freq2 from server 1 (produce error)
% frequency:send_wrong_message(Server1).                <- Send wrong message

