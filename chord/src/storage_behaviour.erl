-module(storage_behaviour).

-callback start_link() ->
    ok.

-callback get(Key :: string()) ->
    tuple('ok', Value :: any()) | 'not_found'.

-callback put(Key :: string(), Value :: any()) ->
    'ok' | tuple('error', Reason :: string()).

-callback matching_delete(Fun :: fun()) ->
    'ok'.
