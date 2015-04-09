-define (DEBUG, true).

-define (DICT, dict).
-define (TRACE(X, M),  io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, My])).
-define (NTRACE(X, M), io:format(user,
                                 "NTRACE ~p:~p ~p ~p ~p ~p ~p~n",
                                 [?MODULE, ?LINE, chord:registered_name(),
                                  node(), self(), X, M])).
-define (RECONNECT_TIMEOUT, 10000).
-define (MAXFINGERS, ?NBIT). % number of bits in the hash function
-define (NBITMOD, round(math:pow(2, ?NBIT))).

-ifdef (DEBUG).
-define (NBIT, 3). % number of bits in the hash function
-else.
-define (NBIT, 160). % number of bits in the hash function
-endif.

-define (DEFAULT_CONFIG,  [
                            {comm, chord}
                          ]).
-record(node, {
          id,                                   % int
          pid                                   % pid
         }).

-record(server_state, {
          self,                                 % #node{}
          predecessor,                          % node{}
          fingers = [],                         % [node{} | ...]
          successor_list = [],                  % [node{} | ...]
          monitors = [],                        % [pid() | pid() ...]
          tref
         }).

-record(finger, {
          start,                                % int
          node                                  % #node{}
         }).

%% Macros
-define(SERVER, ?MODULE).
