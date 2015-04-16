%% Op codes
-define(OP_Get,       16#00).
-define(OP_Set,       16#01).
-define(OP_Add,       16#02).
-define(OP_Replace,   16#03).
-define(OP_Delete,    16#04).
-define(OP_Increment, 16#05).
-define(OP_Decrement, 16#06).
-define(OP_Quit,      16#07).
-define(OP_Flush,     16#08).
-define(OP_GetQ,      16#09).
-define(OP_Noop,      16#0A).
-define(OP_Version,   16#0B).
-define(OP_GetK,      16#0C).
-define(OP_GetKQ,     16#0D).
-define(OP_Append,    16#0E).
-define(OP_Prepend,   16#0F).
-define(OP_Stat,      16#10).

-define(RS_NoError,      16#00).
-define(RS_KeyNotFound,  16#01).
-define(RS_KeyExists,    16#02).
-define(RS_ValueTooLarge,16#03).
-define(RS_InvalidArgs,  16#04).
-define(RS_ItemNotStored,16#05).
-define(RS_NonNumeric,   16#06).
-define(RS_UnknownCmd,   16#81).
-define(RS_OutOfMemory,  16#82).

-record(request, {op_code, key_length, extras_length, data_type=16#00, reserved=16#00,
                  total_body_length,
                  opaque=16#00, cas=16#00, extras = <<>>, key = <<>>, value = <<>>}).
-record(response, {op_code=16#00, data_type=16#00, status=16#00, opaque=16#00, cas=16#00, extras = <<>>, key = <<>>, value = <<>>}).
