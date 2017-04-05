-module(palin).
-export([palindrome_check/0, is_palindrome/1, response_is_palindrome/1]).

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
    true -> "es palindrome.";
    false -> "no es palindrome."
  end.

palindrome_check() ->
  receive
    Msg ->
      io:format("Answer: ~s~n", [response_is_palindrome(Msg)]),
    palindrome_check()
  end.
