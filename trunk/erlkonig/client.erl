-module(client).

-export([start/0]).


start() -> start_client("Real").




start_client(Str) -> 

    {ok, Socket} = gen_tcp:connect("localhost", 2345, 
                        [binary, {packet, 4}]), 
    
    Protocol =  <<90>>,
    Size =      <<00, 00, 00, 00>>,
    Version =   <<77>>,
    Service =   <<01>>,
    Command =   <<01>>, %% CMD_LOGIN
    Username =  <<"Hukka">>,
    Delimiter = <<09>>,
    Pwd =       <<"Pwd">>,

    BinPacket = [Protocol, Size, Version, Service, Command, 
                Username, Delimiter, Pwd],

    io:format("Client sends binary = ~p~n", [BinPacket]),

    ok = gen_tcp:send(Socket, BinPacket), 

receive 
    {tcp,Socket,Bin} -> 
        io:format("Client received binary = ~p~n",[Bin]),
        Val = binary_to_term(Bin), 
        io:format("Client result = ~p~n",[Val]), 
        gen_tcp:close(Socket) 
end. 