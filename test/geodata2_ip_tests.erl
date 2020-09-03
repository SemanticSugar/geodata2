-module(geodata2_ip_tests).

-include_lib("eunit/include/eunit.hrl").

make_ip_test() ->
    IPs =
        [{"127.0.0.1", {ok, <<127, 0, 0, 1>>, 4}},
         {{127, 0, 0, 1}, {ok, <<127, 0, 0, 1>>, 4}},
         {2130706433, {ok, <<127, 0, 0, 1>>, 4}},
         {"2607:f8b0:4005:802::1002",
          {ok, <<38, 7, 248, 176, 64, 5, 8, 2, 0, 0, 0, 0, 0, 0, 16, 2>>, 6}},
         {"ccccc:ddddd::a", {error, einval}},
         {{300, 0, 0, 1}, {error, format}},
         {"12a.3.3.3", {error, einval}},
         {"127.0.0.*", {error, einval}},
         {<<"127.0.0.1">>, {ok, <<127, 0, 0, 1>>, 4}},
         {"12.34.23.0", {ok, <<12, 34, 23, 0>>, 4}},
         {{127, 0, 0, 1, 1}, {error, format}}],

    [?assertEqual(Expected, geodata2_ip:make_ip(IP)) || {IP, Expected} <- IPs],
    ok.
