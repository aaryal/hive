-module(mcd).
-include_lib("defines.hrl").
-compile(export_all).

recv(Socket) ->
    case recv_header(Socket) of
        {error, Err} ->
            {error, Err};
        Header ->
            recv_body(Socket, Header)
    end.

send(Socket, Response) ->
    Bin = encode_response(Response),
    gen_tcp:send(Socket, Bin).

recieve_send(Socket) ->
    Request = recv(Socket),
    Response = handle(Request),
    send(Socket, Response).


build_response(not_found, #response{} = Resp) ->
    Resp#response{status = ?RS_KeyNotFound, value = <<"Not Found">>};
build_response({ok, Value}, #response{} = Resp) ->
    Resp#response{extras = <<16#deadbeef>>, cas = 16#1, value = Value}.


handle(#request{op_code = ?OP_GetK, key = Key}) ->
    Resp = chord_server:get(Key),
    build_response(Resp, #response{key = Key});
handle(#request{op_code = ?OP_Get, key = Key}) ->
    Resp = chord_server:get(Key),
    build_response(Resp, #response{});
handle(#request{op_code = ?OP_Set, key = Key, value = Value}) ->
    chord_server:put(Key, Value),
    #response{op_code = ?RS_KeyExists};
handle(#request{op_code = ?OP_Add, key = Key, value = Value}) ->
    chord_server:put(Key, Value),
    #response{op_code = ?RS_KeyExists};
handle(#request{op_code = ?OP_Quit}) ->
    #response{};
handle(#request{op_code = ?OP_Flush}) ->
    %% TODO: delete all entries from chord. the 'extras' field might specify the timeout for when to do it.
    #response{};
handle(#request{} = _Request) ->
    #response{status = ?RS_UnknownCmd};
handle({error, _Err}) ->
    #response{status = ?RS_UnknownCmd}.


encode_response(#response{op_code = Opcode, data_type = DataType,
                          status = Status,
                          opaque = Opaque,
                          cas = CAS,
                          extras = Extras,
                          key = Key,
                          value = Value
                         } = _Response) ->
    % error_logger:info_msg("Encoding: ~p~n", [Response]),
    Magic = 16#81,
    KeySize = size(Key),
    ExtrasSize = size(Extras),
    Body = <<Extras:ExtrasSize/binary, Key:KeySize/binary, Value/binary>>,
    BodySize = size(Body),
    Resp = <<Magic:8, Opcode:8, KeySize:16, ExtrasSize:8, DataType:8,
             Status:16, BodySize:32, Opaque:32, CAS:64, Body/binary>>,
    % error_logger:info_msg("Encoded to: ~p~n", [Resp]),
    Resp.

recv_header(Socket) ->
    decode_request_header(recv_bytes(Socket, 24)).

recv_body(Socket, #request{total_body_length = TotalBodyLength} = Req) ->
    decode_request_body(recv_bytes(Socket, TotalBodyLength), Req).

decode({data, <<Header:192/bitstring, Body/bitstring>>}) ->
    Request = decode_request_header(Header),
    Request1 = decode_request_body(Body, Request),
    Request1;
decode({data, Any}) ->
    error_logger:info_msg("Recieved data that I can't decode: ~p:~p~n", [Any, size(Any)]),
    {error, not_decoded}.

decode_request_body({error, Err}, _) ->
    {error, Err};
decode_request_body(Bin, #request{key_length = KeySize, extras_length = ExtrasSize} = Req) ->
    <<Extras:ExtrasSize/binary, Key:KeySize/binary, Value/binary>> = Bin,
    Req#request{
      extras = Extras,
      key = Key,
      value = Value
     }.

decode_request_header({error, Err}) -> {error, Err};
decode_request_header(<<16#80:8, Opcode:8, KeyLength:16,
                        ExtraLength:8, DataType:8, Reserved:16,
                        TotalBodyLength:32, Opaque:32, CAS:64>>) ->
    #request{
       op_code = Opcode,
       key_length = KeyLength,
       extras_length = ExtraLength,
       data_type = DataType,
       reserved = Reserved,
       total_body_length = TotalBodyLength,
       opaque = Opaque,
       cas = CAS
      }.



recv_bytes(_, 0) -> <<>>;
recv_bytes(Socket, NumBytes) ->
    case gen_tcp:recv(Socket, NumBytes) of
        {ok, Bin} -> Bin;
        Err -> Err
    end.
