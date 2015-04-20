-module(storage_ets).
-behaviour(storage_behaviour).
-behaviour(gen_server).
-include_lib("defines.hrl").
-export([put/2, get/1, start_link/0, matching_delete/1, dump/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-record(state, {table = nil}).

%% TODO: Don't use ?MODULE or ?SERVER to name the gen_server. we want
%% to be able to use this code in multiple apps on the same node.
start_link() ->
    Config = nil,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

put(Key, Value) ->
    %?DEBUG("Saving ~p -> ~p ~n", [Key, Value]),
    gen_server:cast(?MODULE, {put, Key, Value}),
    ok.

get(Key) ->
    %?DEBUG("Getting ~p~n", [Key]),
    gen_server:call(?MODULE, {get, Key}).

matching_delete(Matcher) ->
    ?DEBUG("Matching delete ~n", []),
    gen_server:cast(?MODULE, {matching_delete, Matcher}),
    ok.

dump() ->
    gen_server:cast(?MODULE, {dump}),
    ok.


init([_Config]) ->
    Table = ets:new(key_val, [set]),
    {ok, #state{table = Table}}.

handle_call({get, Key}, _From, #state{table = Table} = State) ->
    Reply = case ets:lookup(Table, Key) of
                [{Key, Val}] ->
                    {ok, Val};
                [] ->
                    not_found
            end,
    {reply, Reply, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({put, Key, Value}, #state{table = Table} = State) ->
    ets:insert(Table, {Key, Value}),
    {noreply, State};
handle_cast({matching_delete, Matcher}, #state{table = Table} = State) ->
    ets:foldl(fun({Key, Value}, DontCare) ->
                      case Matcher(Key, Value) of
                          true ->
                              ets:delete(Table, Key);
                          _ ->
                              ok
                      end,
                      DontCare
              end, notused, Table),
    {noreply, State};
handle_cast({dump}, #state{table = Table} = State) ->
    ets:foldl(fun({Key, Value}, Cont) ->
                      ?DEBUG("~p -> ~p~n", [Key, Value]),
                      Cont
              end, notused, Table),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
