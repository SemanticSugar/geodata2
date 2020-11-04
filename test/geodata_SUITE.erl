-module(geodata_SUITE).
-include_lib("eunit/include/eunit.hrl").

%% Common test generic exports
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

%% Export here the tests
-export([
    lookup/1,
    domain_lookup/1
]).

%%%===================================================================
%%% CT callbacks
%%%===================================================================
all() ->
    [
        lookup,
        domain_lookup
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Suite, Config) ->
    %% TODO, use smaller test file
    application:load(geodata2),
    IpToDomain = filename:join(code:priv_dir(geodata2), "ip-to-domain.mmdb.gz"),
    DBFilePath = filename:join(code:priv_dir(geodata2), "magellan.mmdb.gz"),
    application:set_env(geodata2, ip_to_domain, IpToDomain),
    application:set_env(geodata2, dbfile, DBFilePath),
    ?assertEqual({ok, IpToDomain}, geodata2:get_env(geodata2, ip_to_domain)),
    ?assertEqual({ok, DBFilePath}, geodata2:get_env(geodata2, dbfile)),
    {ok, _} = application:ensure_all_started(geodata2),
    Config.

end_per_testcase(_Suite, Config) ->
    application:stop(geodata2),
    Config.

%%%===================================================================
%%% Tests
%%%===================================================================

%% Test a normal lookup
lookup(_) ->
    ?assert(fixme_result, geodata2:lookup(<<"1.1.1.1">>)),
    ok.

%% Test a domain lookup
domain_lookup(_) ->
    ?assert(fixme_result, geodata2:lookup_iptodomain(<<"1.1.1.1">>)),
    ok.