-module(storage_ets).
-behaviour(storage_behaviour).
-include_lib("defines.hrl").
-export([init/0, put/3, get/2, matching_delete/2, dump/1]).

init() ->
    Table = ets:new(key_val, [set, {read_concurrency, true}]),
    {ok, Table}.

put(Table, Key, Value) ->
    %?DEBUG("Saving ~p -> ~p ~n", [Key, Value]),
    ets:insert(Table, {Key, Value}),
    ok.

get(Table, Key) ->
    %?DEBUG("Getting ~p~n", [Key]),
    Reply = case ets:lookup(Table, Key) of
                [{Key, Val}] ->
                    {ok, Val};
                [] ->
                    not_found
            end,
    Reply.

matching_delete(Table, Matcher) ->
    ?DEBUG("Matching delete ~n", []),
    ets:foldl(fun({Key, Value}, DontCare) ->
                      case Matcher(Key, Value) of
                          true ->
                              ets:delete(Table, Key);
                          _ ->
                              ok
                      end,
                      DontCare
              end, notused, Table),
    ok.

dump(Table) ->
    ets:foldl(fun({Key, Value}, Cont) ->
                      ?DEBUG("~p -> ~p~n", [Key, Value]),
                      Cont
              end, notused, Table),
    ok.
