-module(chord).
-behaviour(gen_server).
-include_lib("defines.hrl").
-compile(export_all).

-record(server_state, {
          id,
          pid,
          fingers = [],
          tref,
          comm
         }).

-record(finger, {
          id,
          node,
          pid
         }).

%% Macros
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
successor(Id) ->
    gen_server:call(?SERVER, {successor, Id}).

join([RemotePid | _T]) ->
    join(RemotePid);
join([]) ->
    ok;
join(RemotePid) ->
    Finger = gen_server:call(RemotePid, {get_finger}),
    io:format(user, "remote finger: ~p~n", [Finger]),
    ok.

%%====================================================================
%% API (admin interface)
%%====================================================================
start_link() ->
    start_link(?DEFAULT_CONFIG).

start() ->                                      % This is only for development
    start(?DEFAULT_CONFIG).

stop() ->
    gen_server:call(?SERVER, terminate).

status_dump() ->
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
    io:format(user, "other Pids.. ~p~n", [Pids]),
    join(Pids).

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
    Comm = chord,
    ShaInt = create_id(),
    {ok, TRef} = timer:send_interval(timer:minutes(60), run_stabilization_tasks), % stub
    {ok, #server_state{
            id=ShaInt,
            pid=self(),
            tref=TRef,
            comm=Comm}}.


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
handle_call({successor, Id}, _From, State) ->
    {{ok, Finger}, _} = find_successor(Id, State),
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



get_successor(#server_state{fingers = [H | _T]}) ->
    H;
get_successor(#server_state{fingers = []} = State) ->
    make_finger_from_self(State).

make_finger_from_self(#server_state{id = Id}) ->
    #finger{id = Id, node = node(), pid = self()}.


%% Half-open interval (n, s]
id_between_oc(Start, Start, _QueryId) -> % when Start == End -> % degenerate interval
    true;
id_between_oc(Start, End, QueryId) when Start < End  ->  % interval does not wrap
    Start < QueryId andalso End >= QueryId;
id_between_oc(Start, End, QueryId)                   ->  % interval wrap
    Start < QueryId orelse  End >= QueryId.

id_between_oo(Start, Start, QueryId) -> % when Start == End -> % degenerate interval
    Start =/= QueryId;
id_between_oo(Start, End, QueryId) when Start < End -> % interval does not wrap
    Start < QueryId andalso End > QueryId;
id_between_oo(Start, End, QueryId) ->           % interval wraps
    Start < QueryId orelse End > QueryId.


find_successor(Id, #server_state{id = MyId, comm = Comm} = State) ->
    SuccessorFinger = get_successor(State),
    SuccessorId = SuccessorFinger#finger.id,
    case id_between_oc(MyId, SuccessorId, Id) of
        true ->
            {{ok, SuccessorFinger}, State};
        _ ->
            {{ok, Finger}, State} = find_closest_preceding_node(Id, State),
            case Finger#finger.id =:= State#server_state.id of
                true ->
                    {{ok, Finger}, State};
                false ->
                    {Comm:send(Finger, {find_successor, Id}), State}
            end
    end.


find_closest_preceding_node(Id, State) ->
    FingersR = lists:reverse(State#server_state.fingers), % fingers are stored in ascending order
    find_closest_preceding_node(Id, State, FingersR).

find_closest_preceding_node(Id, State, [Finger|T]) ->
    case ch_id_utils:id_between_oo(State#server_state.id, Id, Finger#finger.id) of % one book says _oc
        true  ->
           {{ok, Finger}, State};
        false ->
           find_closest_preceding_node(Id, State, T)
    end;
find_closest_preceding_node(_Id, State, []) ->
    {{ok, make_finger_from_self(State)}, State}.


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
