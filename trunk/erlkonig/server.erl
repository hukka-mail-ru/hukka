-module(server).

-export([start/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> start_parallel_server().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_parallel_server() -> 
    Port = 2345,
    io:fwrite("Listen port ~p ~n", [Port]),
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 4},
                                         {reuseaddr, true}, 
                                         {active, true}]), 
    spawn(fun() ->  par_connect(Listen) end),

    receive
        die -> void
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

par_connect(Listen) -> 
    {ok, Socket} = gen_tcp:accept(Listen), 
    {ok, {Address, Port}} = inet:peername(Socket),
    io:fwrite("Connection from ~p~n", [Address]),
    
    spawn(fun() -> par_connect(Listen) end), 

    io:fwrite("To Add: ~p~n", [Socket]),
    %% TODO add Socket to Table 	
    loop(Socket).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop(Socket) -> 
    receive 
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n",[Bin]),
        

	    parser:parse(Bin, Socket),

           %% Str = binary_to_list(Bin), %% (9)
           %% Reply = [Str | " World"], %% lib_misc:string2value(Str), %% (10)
           %% io:format("Server replying = ~p~n", [Reply]),
           %% gen_tcp:send(Socket, term_to_binary(Reply)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n"),
	    io:fwrite("To Remove: ~p~n", [Socket])
            %% TODO remove Socket from Table 		
    end. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
