-module(parser).


-export([parse/2]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



parse(Bin, Socket) ->
	<<Protocol:8, Size:32, Version:8, Service:8, Command:8, Data/binary>> = Bin,

	io:fwrite("Protocol ~p~n", [Protocol]),
	io:format("Size ~p~n", [Size]),
	io:format("Version ~p~n", [Version]),
	io:format("Service ~p~n", [Service]),
	io:format("Command ~p~n", [Command]),
	io:format("Rest ~p~n", [Data]),

	SRV_SRV = 01,
	CMD_LOGIN = 01,

	case Service of
	SRV_REG -> io:format("Service SRV_SRV ~n"),
		case Command of
		CMD_LOGIN -> io:format("Command CMD_LOGIN ~n"),
                             Res = login(Data),
			     sendMessage(Socket, Service, Res);

		_  -> io:format("Command unknown ~n")
		end;

	_  -> io:format("Service unknown ~n")
	end.



login(Data) ->
	io:format("login ~n"),

	[User, Pwd] = re:split(Data,"\t"),
	io:format("user ~p~n", [binary_to_list(User)]),
	io:format("pwd ~p~n", [binary_to_list(Pwd)]),

%% TODO check name/pwd

	NOERR = 0,

	NOERR
.





	