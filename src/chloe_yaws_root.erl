-module(chloe_yaws_root).

-include("../deps/yaws/include/yaws_api.hrl").

-export([out/1]).

out(_A) ->
    {ehtml, [{h1, [], "Chloe"}]}.
