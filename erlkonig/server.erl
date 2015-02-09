-module(server).

-export([start/0]).


start() -> start_parallel_server().

start_nano_server() -> 
	io:format("listen"),
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true}, 
                                         {active, true}]), 
	io:format("accept"),
    {ok, Socket} = gen_tcp:accept(Listen),
	io:format("close"),
    gen_tcp:close(Listen),
    loop(Socket). 

start_parallel_server() -> 
io:format("before listen~n"),
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true}, 
                                         {active, true}]), 
io:format("before par_connect(Listen) ~n"),
    spawn(fun() ->  par_connect(Listen) end),

        receive
                die -> void
        end.



par_connect(Listen) -> 
io:format("before accept~n"),
    {ok, Socket} = gen_tcp:accept(Listen), 
io:format("internal before par_connect(Listen)~n"),
    spawn(fun() -> par_connect(Listen) end), 
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