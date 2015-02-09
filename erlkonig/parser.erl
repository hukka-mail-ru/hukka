-module(parser).

-export([parse/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(Command) ->
            [H|T] = Command,
            io:format("Command Head: ~p~n",[H]).