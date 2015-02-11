-module(parser).

-export([parse/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



parse(Bin) ->
	<<Protocol:8, Size:32, Version:8, Service:8, Command:8, Rest/binary>> = Bin,

	io:format("Protocol ~p~n", [Protocol]),
	io:format("Size ~p~n", [Size]),
	io:format("Version ~p~n", [Version]),
	io:format("Service ~p~n", [Service]),
	io:format("Command ~p~n", [Command]),
	io:format("Rest ~p~n", [Rest]).

	%%case Command of
	%%	90 -> io:format("Protocol valid ~n"),
        %%             read(Message);
	%%	_ -> io:format("Protocol invalid ~n")
	%%end.

	%%[Protocol|Command] = Message,
	%%io:format("Protocol: ~p~n",[Protocol]),
	%%case Protocol of
	%%	90 -> io:format("Protocol valid ~n"),
       %%              read(Message);
	%%	_ -> io:format("Protocol invalid ~n")
	%%end.




	