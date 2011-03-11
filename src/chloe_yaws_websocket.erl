-module(chloe_yaws_websocket).

-include("../deps/yaws/include/yaws_api.hrl").

-export([out/1]).

-define(ACTIVE, true).

out(A) ->
    case get_upgrade_header(A#arg.headers) of
        undefined ->
            {content, "text/plain", "Websockets only"};
        "WebSocket" ->
            {ok, WebSocketOwner} = chloe_websocket_sup:start_child(),
            {websocket, WebSocketOwner, ?ACTIVE}
    end.

get_upgrade_header(#headers{other=L}) ->
    lists:foldl(fun({http_header, _, K0, _, V}, undefined) ->
                        K = case is_atom(K0) of
                            true ->
                                atom_to_list(K0);
                            false ->
                                K0
                            end,
                        case string:to_lower(K) of
                            "upgrade" ->
                                V;
                            _ -> undefined
                        end;
                    (_, Acc) ->
                        Acc
                end, undefined, L).
