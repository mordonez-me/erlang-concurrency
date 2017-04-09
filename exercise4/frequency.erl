%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0,clear/0]).
-export([init/0]).

%% READ THIS
%% The solution for this is to add a timeout for the client, after nothing 
%% has been received after the time defined (1s) a message is printed. 

%% These are the start functions used to create and
%% initialize the server.

start() ->
  register(frequency,
     spawn(frequency, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  % Simulate overload for frequency server
  timer:sleep(10000),
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      io:fwrite("receiving message~n",[]),
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

allocate() -> 
  % This is called to discard mailbox messages that were not catched
  clear(),
  frequency ! {request, self(), allocate},
  receive 
    {reply, Reply} -> Reply
  % Add 1000ms timeout after requesting allocate to frequency server
  after 1000 -> 
    io:fwrite("Response has not been received in allocate after 1000 ms~n", [])
  end.

deallocate(Freq) -> 
  % This is called to discard mailbox messages that were not catched
  clear(),
  frequency ! {request, self(), {deallocate, Freq}},
  receive 
    {reply, Reply} -> Reply
  % Add 1000ms timeout after requesting deallocate to frequency server
  after 1000 -> 
    io:fwrite("Response has not been received in deallocated after 1000 ms~n", [])
  end.

stop() -> 
  frequency ! {request, self(), stop},
  receive 
    {reply, Reply} -> Reply
  end.

clear() ->
  receive
    _Msg -> 
      io:fwrite("~w is discarded ~n", [_Msg]),
      clear()
  after 0 ->
    ok
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
