-module(lib_md5).

-export([
         hexdigest/1
        ]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

hexdigest(String) ->
    Accumulator = fun (Byte, Digest) ->
            Digest ++ lists:flatten(io_lib:format("~2.16.0b", [Byte]))
        end,
    lists:foldl(Accumulator, "", binary_to_list(erlang:md5(String))).
