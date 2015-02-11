-module(parser).


-export([parse/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



parse(Bin) ->
	<<Protocol:8, Size:32, Version:8, Service:8, Command:8, Data/binary>> = Bin,

	io:format("Protocol ~p~n", [Protocol]),
	io:format("Size ~p~n", [Size]),
	io:format("Version ~p~n", [Version]),
	io:format("Service ~p~n", [Service]),
	io:format("Command ~p~n", [Command]),
	io:format("Rest ~p~n", [Data]),

	SRV_REG = 01,
	CMD_REG = 01,

	case Service of
	SRV_REG -> io:format("Service SRV_REG ~n"),
		case Command of
		CMD_REG -> io:format("Command CMD_REG ~n"),
                           authorize(Data);

		_  -> io:format("Command unknown ~n")
		end;

	_  -> io:format("Service unknown ~n")
	end.



authorize(Data) ->
	io:format("authorize ~n"),

	[User, Pwd] = re:split(Data,"\t"),
	io:format("user ~p~n", [binary_to_list(User)]),
	io:format("pwd ~p~n", [binary_to_list(Pwd)])
.

	