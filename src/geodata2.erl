-module(geodata2).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).
%% API
-export([lookup/1, lookup_iptodomain/1, start/0, start_link/1, stop/0, get_env/2, id/1,
         is_ipv6_mmdb/0]).

-define(GEODATA2_STATE_TID, geodata2_state).
-define(GEODATA2_DOMAIN_TID, geodata2_domain).

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
    case ets:lookup(?GEODATA2_DOMAIN_TID, data) of
        [{data, Data}] ->
            case ets:lookup(?GEODATA2_DOMAIN_TID, meta) of
                [{meta, Meta}] ->
                    case geodata2_ip:make_ip(IP) of
                        {ok, Bits, IPV} ->
                            geodata2_format:lookup(Meta, Data, Bits, IPV);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                [] ->
                    not_found
            end;
        [] ->
            not_found
    end.

start() ->
    application:start(geodata2).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

stop() ->
    application:stop(geodata2).

new(ConfigName, Ets) ->
    ets:new(Ets, [set, protected, named_table, {read_concurrency, true}]),
    case get_env(geodata2, ConfigName) of
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
                    case ConfigName of
                        dbfile ->
                            set_is_ipv6_mmdb(Meta);
                        _ ->
                            ok
                    end,
                    ets:insert(Ets, {data, Data}),
                    ets:insert(Ets, {meta, Meta}),
                    ok;
                false ->
                    {stop, {dbfile_not_found, Filename}}
            end;
        undefined ->
            {stop, {config_not_set, ConfigName}}
    end.

-spec init(_) -> {ok, state()} | {stop, tuple()}.
init(_Args) ->
    case new(dbfile, ?GEODATA2_STATE_TID) of
        ok ->
            case new(ip_to_domain, ?GEODATA2_DOMAIN_TID) of
                ok ->
                    {ok, #state{}};
                _Error -> % Second optional
                    {ok, #state{}}
            end;
        Error -> %% This config is mandatory
            Error
    end.

handle_call(_What, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
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

%% @TODO [RTI-8302] This one could be removed after the IPv4+IPv6 MMDB is definitely used
is_ipv6_mmdb() ->
    application:get_env(geodata2, ipv6, false).

%% @TODO [RTI-8302] This one could be removed after the IPv4+IPv6 MMDB is definitely used
set_is_ipv6_mmdb(Meta) ->
    application:set_env(geodata2, ipv6, geodata2_format:is_ipv6(Meta)).
