%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0,init_server/1]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(router,
	     spawn(frequency, init, [])).

init_server(Frequencies) ->
  server_loop(Frequencies).

init() ->
  BackendServer1 = spawn(
    frequency, 
    init_server, 
    [{lists:seq(10,15), []}]
  ),
  BackendServer2 = spawn(
    frequency, 
    init_server, 
    [{lists:seq(20,25), []}]
  ),
  loop([BackendServer1, BackendServer2], 1).

%% The Main Loop

server_loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      server_loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      server_loop(NewFrequencies)
  end.

loop(Servers, Index) ->
  receive
    {request, _, allocate} = Request ->
      Server = lists:nth(Index, Servers),
      Server ! Request;
    {request, Pid , {deallocate, Freq}} = Request ->
      case Freq of
        _ when Freq > 10, Freq < 15 -> 
          lists:nth(1, Servers) ! Request;
        _ when Freq > 20, Freq < 25 -> 
          lists:nth(2, Servers) ! Request;
        _  -> {frequency_not_found, ok}
      end;
    {request, Pid, stop} = Request ->
      lists:map(fun(Server) -> Server ! Request end, Servers)
  end,
  case Index of 
    _ when Index < length(Servers) -> loop(Servers, Index + 1);
    _ when Index == length(Servers) -> loop(Servers, 1)
  end.
%% Functional interface

allocate() -> 
    router ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    router ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    end.

stop() -> 
    router ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.
