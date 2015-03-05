-module(chord).
-behaviour(gen_server).
-include_lib("defines.hrl").
-compile(export_all).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
successor(Id) ->
    gen_server:call(?SERVER, {find_successor, Id}).

join([RemotePid | _T]) ->
    join(RemotePid);
join([]) ->
    ok;
join(RemotePid) when is_pid(RemotePid) ->
    Finger = gen_server:call(RemotePid, {get_finger}),
    io:format(user, "remote finger: ~p~n", [Finger]),
    ok;
join(#finger{} = RemoteFinger) ->
    gen_server:call(?SERVER, {join, RemoteFinger}).


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
    gen_server:call(?SERVER, {status}).

reload() ->                                     % This is only for development
    sys:suspend(?SERVER),
    code:purge(?MODULE),
    code:load_file(?MODULE),
    sys:change_code(?SERVER, ?SERVER, "0", []),
    sys:resume(?SERVER).

start_link(Config) ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []),
    discover_and_join(),
    Result.

start(Config) ->
    Result = gen_server:start({local, ?SERVER}, ?MODULE, [Config], []),
    discover_and_join(),
    Result.

discover_and_join() ->
    {Pids, _BadNodes} = rpc:multicall(nodes(), erlang, whereis, [?SERVER]),
    ChordPids = [X || X <- Pids, X =/= undefined],
    io:format(user, "other Pids.. ~p~n", [ChordPids]),
    join(ChordPids).

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
    %TODO: this is how you can configure what module recieves the messages 'send' etc..
    %[Comm] = config:fetch_or_default_config([comm], Config, ?DEFAULT_CONFIG),
    ShaInt = create_id(),
    SelfFinger = #finger{id = ShaInt, node = node(), pid = self()},
    {ok, TRef} = timer:send_interval(timer:minutes(60), run_stabilization_tasks), % stub
    {ok, #server_state{
            id = ShaInt,
            self = SelfFinger,
            fingers = [SelfFinger],
            pid = self(),
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
handle_call({status}, _From, State) ->
    {reply, State, State};
handle_call({find_successor, Id}, _From, State) ->
    {{ok, Finger}, _} = find_successor(Id, State),
    {reply, Finger, State};
handle_call({get_finger}, _From, #server_state{self = Finger} = State) ->
    {reply, Finger, State};

%% TODO: remove this clause after i'm done with dev. don't want a catch-all clause... let it fail.
handle_call(_Request, _From, State) ->
    Reply = invalid,
    {reply, Reply, State}.



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
%% Internal stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% Half-open interval (n, s]
id_between_oc(Start, Start, _QueryId) -> % when Start == End -> % degenerate interval
    true;
id_between_oc(Start, End, QueryId) when Start < End  ->  % interval does not wrap
    Start < QueryId andalso End >= QueryId;
id_between_oc(Start, End, QueryId)                   ->  % interval wrap
    Start < QueryId orelse  End >= QueryId.

%% Closed interval (n, s)
id_between_oo(Start, Start, QueryId) -> % when Start == End -> % degenerate interval
    Start =/= QueryId;
id_between_oo(Start, End, QueryId) when Start < End -> % interval does not wrap
    Start < QueryId andalso End > QueryId;
id_between_oo(Start, End, QueryId) ->           % interval wraps
    Start < QueryId orelse End > QueryId.


get_successor(#server_state{successor = Successor}) ->
    Successor;
get_successor(#finger{pid = Pid}) ->
    gen_server:call(Pid, {get_successor}).

find_successor(#server_state{} = State, Id) ->
    Nprime = find_predecessor(State, Id),
    get_successor(Nprime).

find_predecessor(#finger{id = MyId} = N, Id) ->
    Successor = get_successor(N),
    case id_between_oc(MyId, Successor#finger.id, Id) of
        true ->
            N;
        _ ->
            %% closest_preceding_finger needs to be done on the remote node
            PreceedingFinger = closest_preceding_finger(N, Id),
            find_predecessor(PreceedingFinger, Id)
    end.

closest_preceding_finger(#finger{pid = Pid} = _Finger, Id) ->
    gen_server:call(Pid, {closest_preceding_finger, Id});
closest_preceding_finger(#server_state{fingers = Fingers} = State, Id) ->
    RFingers = list:reverse(Fingers),           % TODO is there a better way to do this instead
                                                % of reversing the list every time?
    closest_preceding_finger(State, Id, RFingers).

closest_preceding_finger(#server_state{id = MyId} = State, Id, [#finger{id = FingerId} = H | T]) ->
    case id_between_oo(MyId, FingerId, Id) of
        true ->
            H;
        _ ->
            closest_preceding_finger(State, Id, T)
    end;
closest_preceding_finger(#server_state{self = Self} = _State, _Id, []) ->
    Self.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utility stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




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
