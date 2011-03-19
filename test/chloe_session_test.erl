-module(chloe_session_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/chloe.hrl").

send_to_browser_test() ->
    {ok, MockPid} = gen_server_mock:new(),
    gen_server_mock:expect_cast(MockPid, fun({send, [[Message]]}, State) ->
                "/all" = Message#message.channel,
                "some test data" = Message#message.data,
                {ok, State}
        end),

    {ok, SessionPid} = chloe_session:start_link(MockPid),
    chloe_session:send_to_browser(SessionPid, "/all", "some test data"),
    gen_server_mock:assert_expectations(MockPid).
