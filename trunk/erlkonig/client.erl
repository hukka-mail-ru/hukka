-module(client).

-export([start/0]).


start() -> start_client("Real").




start_client(Str) -> 

    {ok, Socket} = gen_tcp:connect("localhost", 2345, 
                        [binary, {packet, 4}]), 

%%    BinPacket = term_to_binary(Str),
    Protocol = [91],
    Command =  [01, 02],
    Packet = [Protocol] ++ [Command],
    BinPacket = list_to_binary(Packet),

    io:format("Client sends binary = ~p~n", [BinPacket]),

    ok = gen_tcp:send(Socket, BinPacket), 

receive 
    {tcp,Socket,Bin} -> 
        io:format("Client received binary = ~p~n",[Bin]),
        Val = binary_to_term(Bin), 
        io:format("Client result = ~p~n",[Val]), 
        gen_tcp:close(Socket) 
end. 