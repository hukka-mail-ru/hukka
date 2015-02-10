-module(parser).

-export([parse/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(Message) ->
	[Protocol|Command] = Message,
	io:format("Head: ~p~n",[H]),
	case Protocol of
		90 -> io:format("Protocol valid ~n"),
                      execute(Command);
		_ -> io:format("Protocol invalid ~n")
	end.


execute(Command) ->