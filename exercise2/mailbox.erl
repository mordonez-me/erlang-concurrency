-module(mailbox).
-export([test/0, test2_pattern_matching/0, test2_top_level/0]).

test() ->
  receive
    {first, FirstString} -> 
      % timer:sleep(2000),
      io:format("message:~w~n", [{first, FirstString}]);
    {second, SecondString} -> 
      % timer:sleep(2000),
      io:format("message:~w~n", [{second, SecondString}])
  end,
  test().

test2_pattern_matching() ->
  receive
    Msg ->
      case Msg of
        {first, FirstString} -> 
          timer:sleep(2000),
          io:format("message:~w~n", [{first, FirstString}]),
          test2_pattern_matching();
        {second, SecondString} -> 
          timer:sleep(2000),
          io:format("message:~w~n", [{second, SecondString}]),
          test2_pattern_matching();
        {stop, MessageString} ->
          timer:sleep(2000),
          io:format("message:~w~n", [{stop, MessageString}])
      end
  end.

test2_top_level() ->
  receive
    {first, FirstString} -> 
      timer:sleep(2000),
      io:format("message:~w~n", [{first, FirstString}]);
    {second, SecondString} -> 
      timer:sleep(2000),
      io:format("message:~w~n", [{second, SecondString}]);
    {stop, MessageString} ->
      timer:sleep(2000),
      io:format("message:~w~n", [{stop, MessageString}])
  end,
  test().

% c(mailbox).
% Pid = spawn(mailbox, test, []).
% Pid ! {first, "1"}.
% Pid ! {first, "2"}.
% Pid ! {second, "3"}.
% Pid ! {second, "4"}.
% Pid ! {second, "5"}.


% c(mailbox).
% Pid = spawn(mailbox, test2_pattern_matching, []).
% Pid ! {first, "1"}.
% Pid ! {first, "2"}.
% Pid ! {second, "3"}.
% Pid ! {second, "4"}.
% Pid ! {second, "5"}.
% Pid ! {stop, "1"}.
% Pid ! {first, "1"}.


% c(mailbox).
% Pid = spawn(mailbox, test2_top_level, []).
% Pid ! {first, "1"}.
% Pid ! {first, "2"}.
% Pid ! {second, "3"}.
% Pid ! {second, "4"}.
% Pid ! {second, "5"}.
% Pid ! {stop, "1"}.
% Pid ! {first, "1"}.


% c(mailbox).
% Pid = spawn(mailbox, test2_pattern_matching, []).
% Pid ! {second, "4"}.
% Pid ! {first, "1"}.