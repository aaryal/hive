-module(chord_server).
-behaviour(gen_server).
-include_lib("defines.hrl").
-compile(export_all).

-define(SERVER, ?MODULE).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

get(Key) ->
    run_with_one_retry(fun get_unsafe/1, Key).

put(Key, Value) ->
    run_with_one_retry(fun put_unsafe/1, {Key, Value}).

run_with_one_retry(F, Args) ->
    try F(Args) of
        Value ->
            Value
    catch
        exit:{noproc, _Rest} ->
            gen_server:call(?SERVER, run_stabilization_tasks),
            F(Args)
    end.


get_unsafe(Key) ->
    Id = chord:create_key(Key),
    Successor = successor(Id),
    get(Successor, Key).


put_unsafe({Key, Value}) ->
    Id = chord:create_key(Key),
    Successor = successor(Id),
    ok = put(Successor, Key, Value),
    {ok, Successor, Id}.


%% ------------------------------------------------------------------
%% Private
%% ------------------------------------------------------------------
get(#node{pid = Pid}, Key) ->
    gen_server:call(Pid, {get, Key}).

put(#node{pid = Pid}, Key, Value) ->
    gen_server:cast(Pid, {put, Key, Value}),
    ok.

successor(Id) ->
    try
        gen_server:call(?SERVER, {find_successor, Id})
    catch
        exit:{timeout, _Rest} ->
            gen_server:call(?SERVER, run_stabilization_tasks),
            gen_server:call(?SERVER, {find_successor, Id});
        exit:{noproc, _Rest} ->
            gen_server:call(?SERVER, run_stabilization_tasks),
            gen_server:call(?SERVER, {find_successor, Id})
    end.



%%====================================================================
%% API (admin interface)
%%====================================================================
start_link() ->
    start_link(?DEFAULT_CONFIG).

start() ->                                      % This is only for development
    start(?DEFAULT_CONFIG).

start_all() ->
    %% this doesn't work because of timing issues.
    net_adm:world(),
    rpc:multicall([node()| nodes()], ?MODULE, start, []),
    rpc:multicall([node()| nodes()], ?MODULE, join, []).

reload_all() ->
    rpc:multicall([node() | nodes()], ?MODULE, reload, []).

stop() ->
    gen_server:call(?SERVER, terminate).

status() ->
    gen_server:call(?SERVER, status).

join() ->
    gen_server:call(?SERVER, initiate_join).

reload() ->                                     % This is only for development
    sys:suspend(?SERVER),
    code:purge(?MODULE),
    code:load_file(?MODULE),
    sys:change_code(?SERVER, ?SERVER, "0", []),
    sys:resume(?SERVER).

start_link(Config) ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []),
    Result.

start(Config) ->
    Result = gen_server:start({local, ?SERVER}, ?MODULE, [Config], []),
    Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OTP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([_Config]) ->
    ShaInt = create_id(),
    SelfNode = #node{id = ShaInt, pid = self()},
    StorageMod = storage_ets,              %TODO: make this a config variable
    {ok, Opaque} = StorageMod:init(),
    {ok, TRef} = timer:send_interval(timer:seconds(5), run_stabilization_tasks),
    {ok, #server_state{
            self = SelfNode,
            predecessor = SelfNode,
            storage = StorageMod,
            storage_arg = Opaque,
            fingers = chord:create_empty_fingers(SelfNode,1,[]),
            tref = TRef}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({change_state, NewState}, _From, _State) ->
    NewState1 = setup_monitors(NewState),
    {reply, ok, NewState1};
handle_call(run_stabilization_tasks, _From, State) ->
    {noreply, State1} = handle_info(run_stabilization_tasks, State),
    {reply, ok, State1};
handle_call(Msg, From, State) ->
    %% TODO: spawn and call handle_call, meanwhile, reply with {noreply, State}
    F = fun() ->
                Reply = chord:handle_call(Msg, From, State),
                case Reply of
                    {reply, R, State} ->
                        gen_server:reply(From, R);
                    {reply, R, #server_state{self = #node{ pid = Pid}} = State1} ->
                        gen_server:reply(From, R),
                        gen_server:call(Pid, {change_state, State1});
                    _ ->
                        ok
                end,
                ok
        end,
    FF = fun() ->
                 try F() of
                     _ -> ok
                 catch
                     Type:Exception ->
                         error_logger:info_msg("Error ~p:~p~n", [Type, Exception])
                 end
         end,
    spawn_link(FF),
    {noreply, State}.

handle_cast(Msg, State) ->
    chord:handle_cast(Msg, State).

code_change(_OldVsn, State, _Extra) ->
    io:format(user, "Code change..~n", []),
    {ok, State}.

terminate(_Reason, State) ->
    Successor = chord:get_successor(State),
    chord:migrate_all(State, Successor),
    ok.


handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    error_logger:info_msg("Pid down...~p (~p)~n", [Pid, Reason]),
    demonitor(Ref),
    State1 = replace_dead_successor(State, Pid),
    %% Does this cause race conditions??
    %%self() ! run_stabilization_tasks,
    {noreply, State1};
handle_info(run_stabilization_tasks, State) ->
    try chord:run_stabilization_tasks(State) of
        State1 -> 
            State2 = setup_monitors(State1),
            {noreply, State2}
    catch
        Type:Exception ->
            error_logger:info_msg("Error stabilizing..~p:~p~n~p~n", [Type, Exception, State]),
            {noreply, State}
    end;
%% handle_info(run_stabilization_tasks, State) ->
%%     State1 = run_stabilization_tasks(State),
%%     {noreply, State1};
handle_info(Info, State) ->
    error_logger:info_msg("Got message...~p~n", [Info]),
    {noreply, State}.



%% internal clean-up routines.

setup_monitors(#server_state{fingers = Fingers, successor_list = SL,
                             predecessor = Predecessor} = State) ->
    All = [chord:pid_of(X) || X <- [Predecessor | Fingers ++ SL], X =/= nil],
    %% TODO: use a set difference here..
    setup_process_monitor(State, All).

setup_process_monitor(#server_state{monitors = Lst} = State, [Pid | Rest]) ->
    case lists:member(Pid, Lst) of
        false ->
            ?DEBUG("Monitoring: ~p~n", [Pid]),
            monitor(process, Pid),
            LL = [Pid | Lst],
            State1 = State#server_state{monitors = LL},
            setup_process_monitor(State1, Rest);
        _ ->
            setup_process_monitor(State, Rest)
    end;
setup_process_monitor(State, []) ->
    State.

replace_dead_successor(#server_state{fingers = Fingers, successor_list = SL,
                                     monitors = Monitors,
                                     predecessor = Predecessor} = State, Pid) ->
    SL1 = remove_dead_pid(SL, Pid),
    NewSuccessor = hd(SL1),
    NewFingers = replace_dead_successor(Fingers, Pid, NewSuccessor, []),
    NewPredecessor = replace_dead_predecessor(Predecessor, Pid),
    Monitors1 = [X || X <- Monitors, X =/= Pid],
    State#server_state{fingers = NewFingers,
                       predecessor = NewPredecessor,
                       monitors = Monitors1,
                       successor_list = SL1}.

replace_dead_predecessor(#node{pid = Pid}, Pid) ->
    nil;
replace_dead_predecessor(Finger, _Pid) ->
    Finger.

replace_dead_successor([#finger{node = #node{pid = Pid}} = Finger | T], Pid, Replacement, Accl) ->
    Finger1 = Finger#finger{node = Replacement},
    replace_dead_successor(T, Pid, Replacement, [Finger1 | Accl]);
replace_dead_successor([], _Pid, _Replacement, Accl) ->
    lists:reverse(Accl);
replace_dead_successor([H | T], Pid, Replacement, Accl) ->
    replace_dead_successor(T, Pid, Replacement, [H | Accl]).


remove_dead_pid(SL, Pid) ->
    [X || X <- SL, X#node.pid =/= Pid].


registered_name() ->
    P = self(),
    case lists:keyfind(registered_name, 1, process_info(P)) of
        {registered_name, Name} ->
            atom_to_list(Name);
        _ ->
            %% TODO: Raise an error here. we want to use named chords.
            pid_to_list(P)
    end.

create_id() ->
    %% TODO: if we create an id that hashes to something another
    %% existing node hashes to, we're in trouble. Maybe, retry with a
    %% random bit added to the string?
    Id = atom_to_list(node()) ++ registered_name(),
    chord:create_key(Id).

