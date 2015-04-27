-module(storage_behaviour).

-callback init() ->
    {ok, any()}.

-callback get(Opaque :: any(), Key :: string()) ->
    tuple('ok', Value :: any()) | 'not_found'.

-callback put(Opaque :: any(), Key :: string(), Value :: any()) ->
    'ok' | tuple('error', Reason :: string()).

-callback matching_delete(Opaque :: any(), Fun :: fun()) ->
    'ok'.
