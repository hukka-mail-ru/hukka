-module(parser).

-export([parse/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(Message) ->

	[Protocol|Command] = Message,
	io:format("Protocol: ~p~n",[Protocol]),
	case Protocol of
		90 -> io:format("Protocol valid ~n"),
                      execute(Command);
		_ -> io:format("Protocol invalid ~n")
	end.




execute(Command) ->

	{Size, Rest} = lists:split(4, Command).

	