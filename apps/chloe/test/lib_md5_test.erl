-module(lib_md5_test).

-include_lib("eunit/include/eunit.hrl").

hexdigest_test() ->
    "49f68a5c8493ec2c0bf489821c21fc3b" = lib_md5:hexdigest("hi"),
    "c58d6a0c84499d3f992fb23d1348af52" = lib_md5:hexdigest("trotter").
