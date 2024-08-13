-module(geodata_SUITE).

-include_lib("eunit/include/eunit.hrl").

-behaviour(ct_suite).

%% Common test generic exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
%% Export here the tests
-export([lookup/1, domain_lookup/1, no_file_is_found/1, domain_file_not_found/1,
         domain_lookup_weird_ip/1, domain_lookup_not_found/1, domain_lookup_format_not_allowed/1,
         domain_lookup_invalid_ip_with_port/1, domain_not_in_config_should_start/1,
         reload_files/1]).

%%%===================================================================
%%% CT callbacks
%%%===================================================================
all() ->
    [lookup,
     domain_lookup,
     no_file_is_found,
     domain_file_not_found,
     domain_lookup_weird_ip,
     domain_lookup_not_found,
     domain_lookup_format_not_allowed,
     domain_lookup_invalid_ip_with_port,
     domain_not_in_config_should_start].

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
    IpToDomain =
        filename:join(
            code:priv_dir(geodata2), "test.mmdb.gz"),
    DBFilePath =
        filename:join(
            code:priv_dir(geodata2), "test-mgll.mmdb.gz"),
    application:set_env(geodata2, ip_to_domain, IpToDomain),
    application:set_env(geodata2, dbfile, DBFilePath),
    application:set_env(geodata2, reload_milliseconds, 5000),
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
    ?assertMatch({ok, _}, geodata2:lookup(<<"216.58.202.14">>)),
    ok.

%% Test a domain lookup
domain_lookup(_) ->
    ?assertEqual({ok, [{<<"domain">>, <<"google.com">>}]},
                 geodata2:lookup_iptodomain(<<"216.58.202.0">>)),
    ok.

%% Test a domain lookup with weird input
domain_lookup_weird_ip(_) ->
    ?assertEqual(not_found, geodata2:lookup_iptodomain(<<"0.0">>)),
    ok.

domain_lookup_format_not_allowed(_) ->
    ?assertEqual({error, format}, geodata2:lookup_iptodomain(atom_not_allowed_format_error)),
    ok.

domain_lookup_not_found(_) ->
    ?assertEqual(not_found, geodata2:lookup_iptodomain(<<"192.168.0.1">>)),
    ok.

domain_lookup_invalid_ip_with_port(_) ->
    ?assertEqual({error, einval}, geodata2:lookup_iptodomain(<<"192.168.0.1:8080">>)),
    ok.

%% Testing that if we dont setup a domain everything will work as usual anyway
domain_file_not_found(_) ->
    application:load(geodata2),
    IpToDomain =
        filename:join(
            code:priv_dir(geodata2), "notfound.mmdb.gz"),
    DBFilePath =
        filename:join(
            code:priv_dir(geodata2), "test-mgll.mmdb.gz"),
    application:set_env(geodata2, ip_to_domain, IpToDomain),
    application:set_env(geodata2, dbfile, DBFilePath),
    ?assertEqual({ok, IpToDomain}, geodata2:get_env(geodata2, ip_to_domain)),
    ?assertEqual({ok, DBFilePath}, geodata2:get_env(geodata2, dbfile)),
    {ok, _} = application:ensure_all_started(geodata2),

    %% Now a normal not_found lookup working
    ?assertEqual(not_found, geodata2:lookup_iptodomain(<<"1.1.1.1">>)),
    %%
    application:stop(geodata2).

%% Testing that if we dont setup a domain everything will work as usual anyway
no_file_is_found(_) ->
    application:load(geodata2),
    IpToDomain =
        filename:join(
            code:priv_dir(geodata2), "notfound.mmdb.gz"),
    DBFilePath =
        filename:join(
            code:priv_dir(geodata2), "notfound.mmdb.gz"),
    application:set_env(geodata2, ip_to_domain, IpToDomain),
    application:set_env(geodata2, dbfile, DBFilePath),
    ?assertEqual({ok, IpToDomain}, geodata2:get_env(geodata2, ip_to_domain)),
    ?assertEqual({ok, DBFilePath}, geodata2:get_env(geodata2, dbfile)),
    {error, _} = application:ensure_all_started(geodata2).

%% Testing that if we dont setup the files everything will work as usual
domain_not_in_config_should_start(_) ->
    % DBFilePath =
    %     filename:join(
    %         code:priv_dir(geodata2), "test-mgll.mmdb.gz"),
    application:unset_env(geodata2, ip_to_domain),
    % application:set_env(geodata2, dbfile, DBFilePath),
    application:unset_env(geodata2, dbfile),
    application:unset_env(geodata2, reload_milliseconds),
    ?assertEqual(undefined, geodata2:get_env(geodata2, dbfile)),
    ?assertEqual(undefined, geodata2:get_env(geodata2, ip_to_domain)),
    ?assertEqual(undefined, geodata2:get_env(geodata2, reload_milliseconds)),
    application:load(geodata2),
    {ok, _} = application:ensure_all_started(geodata2),

    %% Now a normal not_found lookup working
    ?assertEqual(not_found, geodata2:lookup_iptodomain(<<"1.1.1.1">>)),
    %%
    application:stop(geodata2).

reload_files(_) ->
    application:set_env(geodata2, reload_milliseconds, 4000),

    %% Now a normal not_found lookup working
    ?assertEqual(not_found, geodata2:lookup(<<"214.78.120.0">>)),

    NewFilePath =
        filename:join(
            code:priv_dir(geodata2), "test-mgll-2.mmdb.gz"),
    application:set_env(geodata2, dbfile, NewFilePath),

    %% waits until the reload happens..
    timer:sleep(7000),
    ?assertMatch({ok, _}, geodata2:lookup(<<"61.233.0.0">>)),
    %%
    application:stop(geodata2).
