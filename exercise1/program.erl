-module(program).
-export([start/0]).

start() ->
  Process = spawn(palin, serve, [self()]),
  Process ! {check,"Madam I\'m Adam"},
  Process ! {check,"Madam I\'m Adam"},
  Process ! {check,"Madam I\'m Adam"},
  Process ! {check,"Madam I\'m Adam"},
  Process ! {check,"Madam I\'m Adam"},
  Process ! {check,"Madam I\'m Adam"},
  Process ! {check,"Madam I\'m Adam"}.