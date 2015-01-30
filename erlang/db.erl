-module(db).

-export([new/0]).
-export([write/3]).
-export([delete/2]).
-export([read/2]).
-export([match/2]).

new() -> [].

%% db:write(Key, Element, Db)
%% Db1 = db:write(francesco, london, Db).

write(Key, Value, Db) -> Db ++ [{Key, Value}].

read(Key, Db) -> [ Y || {X,Y} <- Db, X == Key].

match(Value, Db) -> [ X || {X,Y} <- Db, Y == Value].

delete(Key, Db) -> [ {X,Y} || {X,Y} <- Db, X =/= Key].

