-module(geodata2).

%% @todo Remove once https://github.com/inaka/elvis_core/issues/308 is dealt with
-elvis([{elvis_style, export_used_types, disable}]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
%% API
-export([lookup/1, lookup_iptodomain/1, start/0, start_link/1, stop/0, get_env/2, id/1]).

-define(GEODATA2_STATE_TID, geodata2_state).
-define(GEODATA2_IPV4_DOMAIN_TID, geodata2_ipv4_domain).
-define(GEODATA2_IPV6_PREFIX_2600_DOMAIN_TID, geodata2_ipv6_prefix_2600_domain).
-define(GEODATA2_IPV6_PREFIX_2601_DOMAIN_TID, geodata2_ipv6_prefix_2601_domain).
-define(GEODATA2_IPV6_REST_DOMAIN_TID, geodata2_ipv6_rest_domain).

-record(state, {}).

-type state() :: #state{}.

-spec lookup(any()) -> {ok, Result :: list()} | not_found | {error, Reason :: term()}.
lookup(IP) ->
    [{data, Data}] = ets:lookup(?GEODATA2_STATE_TID, data),
    [{meta, Meta}] = ets:lookup(?GEODATA2_STATE_TID, meta),
    case geodata2_ip:make_ip(IP) of
        {ok, Bits, IPV} ->
            geodata2_format:lookup(Meta, Data, Bits, IPV);
        {error, Reason} ->
            {error, Reason}
    end.

lookup_iptodomain(IP) ->
    case geodata2_ip:make_ip(IP) of
        {ok, Bits, 4 = IpVersion} ->
            lookup_(?GEODATA2_IPV4_DOMAIN_TID, Bits, IpVersion);
        {ok, Bits, 6 = IpVersion} ->
            EtsTable = choose_ipv6_ets_table(Bits),
            lookup_(EtsTable, Bits, IpVersion);
        {error, Reason} ->
            {error, Reason}
    end.

start() ->
    application:start(geodata2).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

stop() ->
    application:stop(geodata2).

new(ConfigName, Ets) ->
    BinaryName = atom_to_binary(Ets),
    TmpName = binary_to_atom(<<BinaryName/binary, "_swap">>),
    case ets:info(Ets) of
        undefined ->
            %% when this is the first time, create the table even if next the files don't exist
            ets:new(Ets, [set, protected, named_table, {read_concurrency, true}]),
            ets:new(TmpName, [set, protected, named_table, {read_concurrency, true}]);
        _ ->
            ets:new(TmpName, [set, protected, named_table, {read_concurrency, true}])
    end,

    Res = case get_env(geodata2, ConfigName) of
              {ok, Filename} ->
                  case filelib:is_file(Filename) of
                      true ->
                          {ok, RawData} = file:read_file(Filename),
                          Data =
                              case is_compressed(Filename) of
                                  true ->
                                      zlib:gunzip(RawData);
                                  false ->
                                      RawData
                              end,
                          {ok, Meta} = geodata2_format:meta(Data),
                          ets:insert(TmpName, {data, Data}),
                          ets:insert(TmpName, {meta, Meta}),
                          ok;
                      false ->
                          {stop, {dbfile_not_found, Filename}}
                  end;
              undefined ->
                  {stop, {config_not_set, ConfigName}}
          end,
    %% swaps tables to replace the existing table
    ets:delete(Ets),
    ets:rename(TmpName, Ets),
    Res.

-spec init(_) -> {ok, state()} | {stop, tuple()}.
init(_Args) ->
    Res = load_files(),

    case Res of
        ok ->
            {ok, #state{}};
        Error ->
            Error
    end.

handle_call(_What, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(reload, State) ->
    %% TODO: handle in the state if files are not found
    load_files(),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_env(App, Key) ->
    {ConfigModule, ConfigFun} =
        case application:get_env(geodata2, config_interp) of
            {ok, {Cm, Cf}} ->
                {Cm, Cf};
            _ ->
                {?MODULE, id}
        end,
    case application:get_env(App, Key) of
        {ok, Value} ->
            {ok, ConfigModule:ConfigFun(Value)};
        undefined ->
            undefined
    end.

%% this is used to return app env values as-is when config_interp is not set:
id(X) ->
    X.

is_compressed(Filename) ->
    <<".gz">> == iolist_to_binary(filename:extension(Filename)).

load_files() ->
    Res = case new(dbfile, ?GEODATA2_STATE_TID) of
              ok ->
                  % This files are optional
                  new(ipv4_to_domain, ?GEODATA2_IPV4_DOMAIN_TID),
                  new(ipv6_prefix_2600_to_domain, ?GEODATA2_IPV6_PREFIX_2600_DOMAIN_TID),
                  new(ipv6_prefix_2601_to_domain, ?GEODATA2_IPV6_PREFIX_2601_DOMAIN_TID),
                  new(ipv6_prefix_rest_to_domain, ?GEODATA2_IPV6_REST_DOMAIN_TID),
                  ok;
              Error -> %% This config is mandatory
                  error_logger:error_report({error, geodata2_cannot_load_dbfile}),
                  Error
          end,
    maybe_schedule_reload(),
    Res.

maybe_schedule_reload() ->
    case get_env(geodata2, reload_milliseconds) of
        {ok, Time} when is_integer(Time) ->
            erlang:send_after(Time, self(), reload);
        _ ->
            undefined
    end.

lookup_(EtsTable, Bits, IpVersion) ->
    case ets:lookup(EtsTable, data) of
        [{data, Data}] ->
            case ets:lookup(EtsTable, meta) of
                [{meta, Meta}] ->
                    geodata2_format:lookup(Meta, Data, Bits, IpVersion);
                [] ->
                    not_found
            end;
        [] ->
            not_found
    end.

choose_ipv6_ets_table(<<9728:16,
                        _W6:16,
                        _W5:16,
                        _W4:16,
                        _W3:16,
                        _W2:16,
                        _W1:16,
                        _W0:16>>) ->
    %% 9728 in hexadecimal is 2600 in decimal
    ?GEODATA2_IPV6_PREFIX_2600_DOMAIN_TID;
choose_ipv6_ets_table(<<9729:16,
                        _W6:16,
                        _W5:16,
                        _W4:16,
                        _W3:16,
                        _W2:16,
                        _W1:16,
                        _W0:16>>) ->
    %% 9729 in hexadecimal is 2601 in decimal
    ?GEODATA2_IPV6_PREFIX_2601_DOMAIN_TID;
choose_ipv6_ets_table(_) ->
    ?GEODATA2_IPV6_REST_DOMAIN_TID.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

choose_proper_ipv6_ets_table_test() ->
    {ok, IpA, _} = geodata2_ip:make_ip("2600:f8b0:4005:802::1002"),
    ?assertEqual(?GEODATA2_IPV6_PREFIX_2600_DOMAIN_TID, choose_ipv6_ets_table(IpA)),
    {ok, IpB, _} = geodata2_ip:make_ip("2601:f8b0:4005:802::1002"),
    ?assertEqual(?GEODATA2_IPV6_PREFIX_2601_DOMAIN_TID, choose_ipv6_ets_table(IpB)),
    {ok, IpC, _} = geodata2_ip:make_ip("2500:f8b0:4005:802::1002"),
    ?assertEqual(?GEODATA2_IPV6_REST_DOMAIN_TID, choose_ipv6_ets_table(IpC)),
    {ok, IpD, _} = geodata2_ip:make_ip("3500:f8b0:4005:802::1002"),
    ?assertEqual(?GEODATA2_IPV6_REST_DOMAIN_TID, choose_ipv6_ets_table(IpD)),
    {ok, IpE, _} = geodata2_ip:make_ip("2600:0000:4005:802::1002"),
    ?assertEqual(?GEODATA2_IPV6_PREFIX_2600_DOMAIN_TID, choose_ipv6_ets_table(IpE)).

-endif.
