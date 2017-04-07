-module(palin).
-export([serve/1, internal_server/1, is_palindrome/1, response_is_palindrome/1]).

rem_punct(String) -> 
  lists:filter(fun(Ch) ->
    not(lists:member(Ch, "\"\'t\n "))
  end,
  String).

to_small(String) -> 
  lists:map(fun(Ch) ->
    case ($A =< Ch andalso Ch =< $Z ) of 
      true -> Ch + 32;
      false -> Ch
    end
  end,
  String).

is_palindrome(String) -> 
  Normalise = to_small(rem_punct(String)),
  lists:reverse(Normalise) == Normalise.

response_is_palindrome(String) ->
  case (is_palindrome(String)) of
    true -> "is a palindrome.";
    false -> "is not a palindrome."
  end.

frontend_server(Pid, Servers) ->
  receive 
    {check, Msg} ->
      [Server | _] = Servers,
      Server ! {Pid, check, Msg},
      frontend_server(Pid, lists:reverse(Servers))
  end.

internal_server(Index) ->
  receive
    {Pid, check, Msg} ->
      Strings = [Msg, response_is_palindrome(Msg)],
      ReturnMessage = string:join(Strings, " "),
      io:format("Receiving message from in index ~B~n", [Index]),
      Pid ! { result,  ReturnMessage },
      internal_server(Index);
    stop ->
      io:format("Terminating internal server ~n", [])
  end.

serve(Pid) ->
  io:write("Starting server ~n"),
  Server_1 = spawn(palin, internal_server, [1]),
  Server_2 = spawn(palin, internal_server, [2]),
  frontend_server(Pid, [Server_1, Server_2]).