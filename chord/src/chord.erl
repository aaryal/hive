-module(chord).
-behaviour(gen_server).
-include_lib("defines.hrl").
-compile(export_all).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
successor(Id) ->
    gen_server:call(?SERVER, {find_successor, Id}).

%%====================================================================
%% API (admin interface)
%%====================================================================
start_link() ->
    start_link(?DEFAULT_CONFIG).

start() ->                                      % This is only for development
    start(?DEFAULT_CONFIG).

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
    {ok, TRef} = timer:send_interval(timer:minutes(60), run_stabilization_tasks), % stub
    {ok, #server_state{
            self = SelfNode,
            predecessor = SelfNode,
            fingers = create_empty_fingers(SelfNode,1,[]),
            tref = TRef}}.


create_empty_fingers(_Node, ?MAXFINGERS+1, Accl) ->
    lists:reverse(Accl);
create_empty_fingers(#node{id = N} = Node, K, Accl) ->
    Start = round(N + math:pow(2, K-1)) rem ?NBITMOD,
    KthFinger = #finger{start = Start, node = Node},
    create_empty_fingers(Node, K+1, [KthFinger | Accl]).

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
    {reply, Reply, State}.

handle_cast({set_predecessor, #node{} = Predecessor}, State) ->
    State1 = set_predecessor(State, Predecessor),
    {noreply, State1};
handle_cast({update_finger_table, Node, S, I}, State) ->
    State1 = update_finger_table(State, Node, S, I),
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    io:format(user, "Code change..~n", []),
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_info(run_stabilization_tasks, State) ->
    %% TODO: do stabilization here... this is invoked by timers started in init.
    io:format(user, "Stabalizing ~p~n", [State]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal stuff (Chord Algorithm)
%%
%% the goal here is to be as close as possible to the pesudocode in
%% the paper.  it will have erlang-isms. for example, pattern matching
%% and recursion instead of if-then-else and for-loops
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%% remote symantics.. this should have a different signature %%%%%%%%%%%%%%%%%%%%
closest_preceding_finger(#server_state{ self = #node{ pid = Pid}} = State, #node{pid = Pid}, Id) ->
    closest_preceding_finger(State, Id);
closest_preceding_finger(_, #node{pid = Pid}, Id) ->
    gen_server:call(Pid, {closest_preceding_finger, Id});
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
join(#server_state{self = N, fingers = Fingers} = State, []) -> % This is the only node on the network
    NewFingers = [X#finger{node = N} || X <- Fingers],
    State1 = State#server_state{fingers = NewFingers, predecessor = N},
    State1;
join(State, Nprime) ->                             % Nprime is another node that already exists
    State1 = init_finger_table(State, Nprime),
    %error_logger:info_msg("before updating others: ~n~p~n", [State1]),
    State2 = update_others(State1),
    %error_logger:info_msg("after updating others: ~n~p~n", [State2]),
    %% TODO: move keys in (predecessor, n] from successor
    State2.

init_finger_table(#server_state{fingers = [Finger1 | Rest]} = State, Nprime) ->
    Successor = find_successor(State, Nprime, Finger1#finger.start),

    Fingers1 = [Finger1#finger{node = Successor} | Rest],

    State1 = State#server_state{fingers = Fingers1},
    Predecessor = get_predecessor(State1, Successor),
    State2 = set_predecessor(State1, Predecessor),
    set_predecessor(Successor, State2),

    [FirstFinger | OtherFingers] = Fingers1,
    Fingers2 = init_finger_table_helper(State2, Nprime, FirstFinger, OtherFingers, [FirstFinger]),
    State3 = State2#server_state{fingers = Fingers2},
    State3.

init_finger_table_helper(_State, _Nprime, _IFinger, [], Accl) ->
    lists:reverse(Accl);
init_finger_table_helper(State, Nprime, IFinger, [IplusOneFinger | OtherFingers], Accl) ->
    case between_co(IplusOneFinger#finger.start, {id_of(State), id_of(IFinger)}) of
        true ->
            IplusOneFinger1 = IplusOneFinger#finger{node = IFinger#finger.node},
            init_finger_table_helper(State, Nprime, IplusOneFinger1, OtherFingers, [IplusOneFinger1 | Accl]);
        _ ->
            Successor = find_successor(State, Nprime, IplusOneFinger#finger.start),
            IplusOneFinger1 = IplusOneFinger#finger{node = Successor},
            init_finger_table_helper(State, Nprime, IplusOneFinger1, OtherFingers, [IplusOneFinger1 | Accl])
    end.


update_others(State) ->
    State1 = update_others(State, 1),
    State1.

update_others(State, ?NBIT+1) ->
    State;
update_others(#server_state{self = #node{id = Nid}} = State, I) ->
    T = round(Nid - math:pow(2, I-1)),
    P = find_predecessor(State, T),
    %error_logger:info_msg("updating others finger table: ~nState: ~p~nI: ~p~nT: ~p~nP: ~p~n", [State, I, T, P]),

    State1 = update_finger_table(State, P, State#server_state.self, I),
    update_others(State1, I+1).

update_finger_table(State, Node, Node, _I) ->
    %% don't need to tell the node to make itself what's pointed at
    %% since that's how it would have started out anyway.
    State;
update_finger_table(#server_state{self = #node{ pid = Pid}} = State, #node{pid = Pid}, S, I) ->
    %error_logger:info_msg("I'm told to update my finger table: ~nState: ~p~nS: ~p~nI: ~p~n", [State, S, I]),
    State1 = update_finger_table_helper(State, S, I),
    %error_logger:info_msg("after updating finger table: ~nState: ~p~n~n ******************** ~n", [State1]),
    State1;
update_finger_table(State, #node{pid = Pid} = Node, S, I) ->
    %error_logger:info_msg("telling other to update their finger table: ~nState: ~p~nNode: ~p~nS: ~p~nI: ~p~n", [State, Node, S, I]),
    gen_server:cast(Pid, {update_finger_table, Node, S, I}),
    State.

update_finger_table_helper(#server_state{fingers = Fingers} = State, #node{} = S, I) ->
    IthFinger = lists:nth(I, Fingers),
    case between_co(id_of(S), {id_of(State), id_of(IthFinger)}) of
        true ->
            %error_logger:info_msg("**** ~p E [~p, ~p) is TRUE~n", [id_of(S), id_of(State), id_of(IthFinger)]),
            NewIthFinger = IthFinger#finger{node = S},
            Fingers1 = lists:sublist(Fingers, I-1) ++ [NewIthFinger] ++ lists:nthtail(I, Fingers),
            State1 = State#server_state{fingers = Fingers1},
            P = get_predecessor(State1),
            State2 = update_finger_table(State1, P, S, I),
            State2;
        _ ->
            %error_logger:info_msg("**** ~p E [~p, ~p) is FALSE. No Change~n", [id_of(S), id_of(State), id_of(IthFinger)]),
            State
    end.


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

get_successor(#server_state{fingers = Fingers} = _State) ->
    Finger = hd(Fingers),
    Finger#finger.node.


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

id_of(#server_state{self = Node}) ->
    id_of(Node);
id_of(#finger{node = Node}) ->
    id_of(Node);
id_of(#node{id = Id}) ->
    Id.

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
    Id = atom_to_list(node()) ++ registered_name(),
    create_key(Id).

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
