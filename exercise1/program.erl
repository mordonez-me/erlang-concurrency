-module(program).
-export([start/0]).

start() ->
  Process = spawn(palin, palindrome_check, []),
  Process ! "Madam I\'m Adam".