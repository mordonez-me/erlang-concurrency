%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

init() ->
  Frequencies = {get_frequencies(), dict:new()},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  KeyFound = dict:find(Pid, Allocated),
  case KeyFound of
    {ok, Value} -> 
      {{Free, Allocated}, {ok, Value}};
    error -> 
      NewAllocated = dict:store(Pid, Freq, Allocated),
      {{Free, NewAllocated}, {ok, Freq}}
  end.

deallocate({Free, Allocated}, Freq, Pid) ->
  KeyFound = dict:find(Pid, Allocated),
  case KeyFound of 
    {ok, Value} ->
      case Value =:= Freq of
        true ->
          NewAllocated = dict:erase(Pid, Allocated),
          {{[Freq|Free], NewAllocated}, {ok, Freq}};
        false ->
          {{Free, Allocated}, {not_belong, Freq}}
      end;
    error ->
      {{Free, Allocated}, {not_found, Freq}}
  end.
