-module(server).

-export([start/0]).


start() -> start_nano_server().

start_nano_server() -> 
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true}, 
                                         {active, true}]), 
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    loop(Socket). 

loop(Socket) -> 
    receive 
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n",[Bin]),
            Str = binary_to_term(Bin), %% (9)
            io:format("Server (unpacked) ~p~n",[Str]),
            Reply = [Str | " World"], %% lib_misc:string2value(Str), %% (10)
            io:format("Server replying = ~p~n",[Reply]),
            gen_tcp:send(Socket, term_to_binary(Reply)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end. 