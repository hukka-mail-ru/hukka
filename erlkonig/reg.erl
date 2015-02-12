-module(reg).

-define(NOERR, 0).

-export([login/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
login(Data) ->
	io:format("login ~n"),

	[User, Pwd] = re:split(Data,"\t"),
	io:format("user ~p~n", [binary_to_list(User)]),
	io:format("pwd ~p~n", [binary_to_list(Pwd)]),

%% TODO check name/pwd


	?NOERR
.