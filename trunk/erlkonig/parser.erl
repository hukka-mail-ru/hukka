-module(parser).

-define(SRV_REG, 01).
-define(CMD_LOGIN, 01).

-define(PROTOCOL, 90).
-define(VERSION, 65).

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

	case Service of
	?SRV_REG -> io:format("Service SRV_SRV ~n"),
		case Command of
		?CMD_LOGIN -> io:format("Command CMD_LOGIN ~n"),
                             Res = reg:login(Data),
			     sendMessage(Socket, Service, Res);

		_  -> io:format("Command unknown ~n")
		end;

	_  -> io:format("Service unknown ~n")
	end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendMessage(Socket, Serv, Res) ->

	    Protocol =  <<?PROTOCOL>>,
	    Size =      <<00, 00, 00, 00>>,
	    Version =   <<?VERSION>>,
	    Service =   <<Serv>>,
	    Command =   <<Res>>,

	    BinPacket = [Protocol, Size, Version, Service, Command],

	    io:format("Reply = ~p~n", [BinPacket]),

	    ok = gen_tcp:send(Socket, BinPacket)
.




	