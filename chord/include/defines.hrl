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
-define (NBIT, 7). % number of bits in the hash function
-else.
-define (NBIT, 160). % number of bits in the hash function
-endif.

-define (DEFAULT_CONFIG,  [
                            {comm, chord}
                          ]).

-record(server_state, {
          id,
          pid,
          self,
          successor,
          fingers = [],
          tref
         }).

-record(finger, {
          id,
          node,
          pid
         }).

%% Macros
-define(SERVER, ?MODULE).
