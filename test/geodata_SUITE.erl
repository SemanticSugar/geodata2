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
    domain_lookup/1,
    no_file_is_found/1,
    domain_file_not_found/1
]).

%%%===================================================================
%%% CT callbacks
%%%===================================================================
all() ->
    [
        lookup,
        domain_lookup,
        no_file_is_found,
        domain_file_not_found
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.
init_per_testcase(no_file_is_found, Config) ->
    Config;
init_per_testcase(domain_file_not_found, Config) ->
    Config;
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
    ?assertMatch({ok, _Data}, geodata2:lookup(<<"1.1.1.1">>)),
    ok.

%% Test a domain lookup
domain_lookup(_) ->
    ?assertEqual(fixme_result, geodata2:lookup_iptodomain(<<"1.1.1.1">>)),
    ok.

%% Testing that if we dont setup a domain everything will work as usual anyway
domain_file_not_found(_) ->
    application:load(geodata2),
    IpToDomain = filename:join(code:priv_dir(geodata2), "notfound.mmdb.gz"),
    DBFilePath = filename:join(code:priv_dir(geodata2), "magellan.mmdb.gz"),
    application:set_env(geodata2, ip_to_domain, IpToDomain),
    application:set_env(geodata2, dbfile, DBFilePath),
    ?assertEqual({ok, IpToDomain}, geodata2:get_env(geodata2, ip_to_domain)),
    ?assertEqual({ok, DBFilePath}, geodata2:get_env(geodata2, dbfile)),
    {ok, _} = application:ensure_all_started(geodata2),

    %% Now a normal lookup working ok
    ?assertMatch({ok, _Data}, geodata2:lookup(<<"1.1.1.1">>)),
    ?assertEqual(not_found, geodata2:lookup_iptodomain(<<"1.1.1.1">>)),
    %%
    application:stop(geodata2).

%% Testing that if we dont setup a domain everything will work as usual anyway
no_file_is_found(_) ->
    application:load(geodata2),
    IpToDomain = filename:join(code:priv_dir(geodata2), "notfound.mmdb.gz"),
    DBFilePath = filename:join(code:priv_dir(geodata2), "notfound.mmdb.gz"),
    application:set_env(geodata2, ip_to_domain, IpToDomain),
    application:set_env(geodata2, dbfile, DBFilePath),
    ?assertEqual({ok, IpToDomain}, geodata2:get_env(geodata2, ip_to_domain)),
    ?assertEqual({ok, DBFilePath}, geodata2:get_env(geodata2, dbfile)),
    {error, _} = application:ensure_all_started(geodata2).