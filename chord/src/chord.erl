-module(chord).
-include_lib("defines.hrl").
-compile(export_all).

-define(SERVER, chord_server).

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
    ShaInt = chord_server:create_id(),
    SelfNode = #node{id = ShaInt, pid = self()},
    StorageMod = storage_ets,              %TODO: make this a config variable
    {ok, TRef} = timer:send_interval(timer:seconds(5), run_stabilization_tasks),
    {ok, #server_state{
            self = SelfNode,
            predecessor = SelfNode,
            storage = StorageMod,
            fingers = create_empty_fingers(SelfNode,1,[]),
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
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};
handle_call(status, _From, State) ->
    {reply, State, State};
handle_call(get_self, _From, #server_state{self = Self} = State) ->
    {reply, Self, State};
handle_call(initiate_join, _From, State) ->
    {Pids, _BadNodes} = rpc:multicall(nodes(), erlang, whereis, [?SERVER]),
    ChordPids = [X || X <- Pids, X =/= undefined],
    State1 = join(State, ChordPids),
    {reply, ok, State1};
handle_call({closest_preceding_finger, Id}, _Fron, State) ->
    Reply = closest_preceding_finger(State, Id),
    {reply, Reply, State};
handle_call({find_successor, Id}, _From, State) ->
    Reply = find_successor(State, Id),
    {reply, Reply, State};
handle_call(get_successor, _From, State) ->
    Reply = get_successor(State),
    {reply, Reply, State};
handle_call(get_predecessor, _From, State) ->
    Reply = get_predecessor(State),
    {reply, Reply, State};
handle_call({get, Key}, _From, #server_state{storage = StorageMod} = State) ->
    Result = StorageMod:get(Key),
    {reply, Result, State}.

handle_cast({set_predecessor, #node{} = Predecessor}, State) ->
    State1 = set_predecessor(State, Predecessor),
    {noreply, State1};
handle_cast({notify, Nprime}, State) ->
    State1 = notify(State, Nprime),
    {noreply, State1};
handle_cast({put, Key, Value}, #server_state{storage = StorageMod} = State) ->
    %?DEBUG("Saving ~p~n", [Key]),
    StorageMod:put(Key, Value),
    %apply(StorageMod, put, [Key, Value]),
    {noreply, State};
handle_cast({migrate_keys, MigrateTo}, State) ->
    ?DEBUG("Migrating some keys to ~p~n", [MigrateTo]),
    spawn(chord, migrate_keys, [State, MigrateTo]),    %Not sure if i should use spawn_link here.
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


run_stabilization_tasks(State) ->
    State1 = stabilize(State),
    State2 = fix_fingers(State1),
    State2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal stuff (Chord Algorithm)
%%
%% the goal here is to be as close as possible to the pesudocode in
%% the paper.  it will have erlang-isms. for example, pattern matching
%% and recursion instead of if-then-else and for-loops
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

migrate_all(#server_state{ self = #node{ pid = Pid}}, #node{pid = Pid}) ->
    %% migrating to self..
    ok;
migrate_all(#server_state{storage = Storage}, MigrateTo) ->
    F = fun(Key, Value) ->
                chord_server:put(MigrateTo, Key, Value),
                true
        end,
    Storage:matching_delete(F).

migrate_keys(#server_state{self = #node{pid = Pid}}, #node{pid = Pid}) ->
    %% migrating to self..
    ok;
migrate_keys(#server_state{storage = Storage}, #node{id = Id} = MigrateTo) ->
    F = fun(Key, Value) ->
                KeyId = create_key(Key),
                case KeyId =< Id of
                    true ->
                        chord_server:put(MigrateTo, Key, Value),
                        true;
                    _ ->
                        false
                end
        end,
    Storage:matching_delete(F).

find_successor(#server_state{ self = #node{ pid = Pid}} = State, #node{pid = Pid}, Id) ->
    find_successor(State, Id);
find_successor(_, #node{pid = Pid}, Id) ->
    gen_server:call(Pid, {find_successor, Id}).

find_successor(State, Id) ->
    Nprime = find_predecessor(State, Id),
    get_successor(State, Nprime).

find_predecessor(#server_state{self = N} = State, Id) ->
    find_predecessor(State, Id, N).

find_predecessor(State, Id, Nprime) ->
    Successor = get_successor(State, Nprime),
    case between_oc(Id, {id_of(Nprime), id_of(Successor)}) of
        true ->
            Nprime;
        _ ->
            Nprime1 = closest_preceding_finger(State, Nprime, Id),
            find_predecessor(State, Id, Nprime1)
    end.

closest_preceding_finger(#server_state{fingers = Fingers} = State, Id) ->
    RFingers = lists:reverse(Fingers),
    closest_preceding_finger(State, Id, RFingers).

closest_preceding_finger(#server_state{ self = #node{ pid = Pid}} = State, #node{pid = Pid}, Id) ->
    closest_preceding_finger(State, Id);
closest_preceding_finger(_, #node{pid = Pid}, Id) ->
    gen_server:call(Pid, {closest_preceding_finger, Id});
closest_preceding_finger(State, Id, [FingerI | RemainingFingers]) ->
    case between_oo(id_of(FingerI), {id_of(State), Id}) of
        true ->
            FingerI#finger.node;
        _ ->
            closest_preceding_finger(State, Id, RemainingFingers)
    end;
closest_preceding_finger(State, _Id, []) ->
    State#server_state.self.




join(State, [Pid | _T]) when is_pid(Pid) ->
    Nprime = get_self(State, Pid),
    join(State, Nprime);
join(#server_state{} = State, []) -> % This is the only node on the network
    State1 = State#server_state{predecessor = nil},
    State1;
join(#server_state{} = State, Nprime) ->
    State1 = State#server_state{predecessor = nil},
    Successor = find_successor(State1, Nprime, id_of(State1)),
    State2 = set_successor(State1, Successor),
    %% TODO: move keys in (predecessor, n] from successor to Self.
    gen_server:cast(Successor#node.pid, {migrate_keys, State2#server_state.self}),
    State2.


stabilize(State) ->
    Successor = get_successor(State),
    X = get_predecessor(State, Successor),
    case X =/= nil andalso between_oo(id_of(X), {id_of(State), id_of(Successor)}) of
        true ->
            State1 = set_successor(State, X),
            State2 = notify(X, State1),                  % X is now the successor
            State2;
        _ ->
            State1 = notify(Successor, State),
            State1
    end.

notify(#node{pid = Pid}, #server_state{self = #node{pid = Pid}} = State) ->
    State;
notify(#node{pid = Pid}, #server_state{self = Node} = State) ->
    gen_server:cast(Pid, {notify, Node}),
    State;
notify(#server_state{predecessor = Predecessor} = State, Nprime) ->
    case Predecessor =:= nil orelse between_oo(id_of(Nprime), {id_of(Predecessor), id_of(State)}) of
        true ->
            State1 = set_predecessor(State, Nprime),
            State1;
        _ ->
            State
    end.

fix_fingers(#server_state{fingers = Fingers} = State) ->
    I = random:uniform(?MAXFINGERS - 1) + 1,
    IthFinger = lists:nth(I, Fingers),
    Successor = find_successor(State, IthFinger#finger.start),
    NewIthFinger = IthFinger#finger{node = Successor},
    Fingers1 = lists:sublist(Fingers, I-1) ++ [NewIthFinger] ++ lists:nthtail(I, Fingers),
    State1 = State#server_state{fingers = Fingers1},
    State1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Getters / Setters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_self(#server_state{ self = #node{ pid = Pid} = Self}, #node{pid = Pid}) ->
    Self;
get_self(_State, Pid) ->
    gen_server:call(Pid, get_self).

get_successor(#server_state{ self = #node{ pid = Pid}} = State, #node{pid = Pid}) ->
    get_successor(State);
get_successor(_State, #node{pid = Pid}) ->
    gen_server:call(Pid, get_successor);
get_successor(State, State) ->
    get_successor(State).

get_successor(#server_state{fingers = [#finger{node = Node} | _Tail]} = _State) ->
    Node.

set_successor(#server_state{fingers = [H | Tails], successor_list = SL} = State,
              #node{} = Successor) ->
    OldSuccessor = H#finger.node,
    OldSuccessorList = case lists:member(OldSuccessor, SL) of
                           true -> SL;
                           false -> [OldSuccessor | SL]
                       end,
    State#server_state{fingers = [H#finger{node = Successor} | Tails],
                       successor_list = OldSuccessorList}.


get_predecessor(#server_state{ self = #node{ pid = Pid}} = State, #node{pid = Pid}) ->
    get_predecessor(State);
get_predecessor(_State, #node{pid = Pid}) ->
    gen_server:call(Pid, get_predecessor);
get_predecessor(State, State) ->
    get_predecessor(State).

get_predecessor(#server_state{predecessor = Predecessor} = _State) ->
    Predecessor.


set_predecessor(#server_state{self = #node{ pid = Pid}} = State, #node{pid = Pid}, Predecessor) ->
    set_predecessor(State, Predecessor);
set_predecessor(State, #node{pid = Pid}, Predecessor) ->
    gen_server:cast(Pid, {set_predecessor, Predecessor}),
    State;
set_predecessor(State, State, Predecessor) ->
    set_predecessor(State, Predecessor).

set_predecessor(#server_state{} = State, #node{} = Predecessor) ->
    State1 = State#server_state{predecessor = Predecessor},
    State1;
set_predecessor(#node{pid = Pid}, #server_state{self = Predecessor} = State) ->
    gen_server:cast(Pid, {set_predecessor, Predecessor}),
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utility stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_empty_fingers(_Node, ?MAXFINGERS+1, Accl) ->
    lists:reverse(Accl);
create_empty_fingers(#node{id = N} = Node, K, Accl) ->
    Start = round(N + math:pow(2, K-1)) rem ?NBITMOD,
    KthFinger = #finger{start = Start, node = Node},
    create_empty_fingers(Node, K+1, [KthFinger | Accl]).

id_of(#server_state{self = Node}) ->
    id_of(Node);
id_of(#finger{node = Node}) ->
    id_of(Node);
id_of(#node{id = Id}) ->
    Id.


pid_of(#finger{node = Node}) ->
    pid_of(Node);
pid_of(#node{pid = Pid}) ->
    Pid;
pid_of(Pid) when is_pid(Pid) ->
    Pid.


%% Half-open interval (n, s]
between_oc(_QueryId, {Start, Start}) ->
    true;
between_oc(QueryId, {Start, End}) when Start < End  ->  % interval does not wrap
    Start < QueryId andalso QueryId =< End;
between_oc(QueryId, {Start, End})                   ->  % interval wrap
    Start < QueryId orelse  QueryId =< End.

%% Close-open interval [n, s)
between_co(_QueryId, {Start, Start}) ->         % degenerate interval
    true;
between_co(QueryId, {Start, End}) when Start < End -> % interval does not wrap
    Start =< QueryId andalso QueryId < End;
between_co(QueryId, {Start, End}) ->
    Start =< QueryId orelse QueryId < End.

%% Open-Open interval (n, s)
between_oo(QueryId, {Start, Start}) -> % when Start == End -> % degenerate interval
    Start =/= QueryId;
between_oo(QueryId, {Start, End}) when Start < End -> % interval does not wrap
    Start < QueryId andalso QueryId < End;
between_oo(QueryId, {Start, End}) ->           % interval wraps
    Start < QueryId orelse QueryId < End.


create_key(Str) ->
    Sha = hexstring(Str),
    Int = hex_to_int(Sha),
    Int rem ?NBITMOD.


binstring(S) ->
    crypto:hash(sha, S).

hexstring(S) ->
    X = binstring(S),
    lists:flatten([io_lib:format("~2.16.0B",[A]) || <<A>> <= X]).

hex_to_int(HexStr) ->
    {ok, [Num], _} = io_lib:fread("~16u", HexStr),
    Num.
