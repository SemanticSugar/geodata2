-module(geodata2).

-behavior(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).
%% API
-export([lookup/1, start/0, start_link/1, stop/0, get_env/2, id/1]).

-include("geodata2.hrl").

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

start() ->
    application:start(geodata2).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

stop() ->
    application:stop(geodata2).

new(MagellanFile, DomainFile) ->
    MagellanState = case filelib:is_file(MagellanFile) of
      true ->
          {ok, RawData} = file:read_file(MagellanFile),
          Data = case is_compressed(MagellanFile) of
                   true ->
                       zlib:gunzip(RawData);
                   false ->
                       RawData
                 end,
          {ok, Meta} = geodata2_format:meta(Data),
          ets:new(?GEODATA2_STATE_TID, [set, protected, named_table, {read_concurrency, true}]),
          ets:insert(?GEODATA2_STATE_TID, {data, Data}),
          ets:insert(?GEODATA2_STATE_TID, {meta, Meta}),
          {ok, #state{}};
      _ ->
          {stop, {geodata2_dbfile_not_found, MagellanFile}}
    end,
    DomainState = case filelib:is_file(DomainFile) of
      true ->
          {ok, RawData2} = file:read_file(DomainFile),
          Data2 = case is_compressed(DomainFile) of
                   true ->
                       zlib:gunzip(RawData2);
                   false ->
                       RawData2
                 end,
          {ok, Meta2} = geodata2_format:meta(Data2),
          ets:new(?GEODATA2_DOMAIN_TID, [set, protected, named_table, {read_concurrency, true}]),
          ets:insert(?GEODATA2_DOMAIN_TID, {data, Data2}),
          ets:insert(?GEODATA2_DOMAIN_TID, {meta, Meta2}),
          {ok, #state{}};
      _ ->
          {stop, {ip_to_domain_dbfile_not_found, DomainFile}}
    end,
    {ok, {MagellanState, DomainState}}.

-spec init(_) -> {ok, state()} | {stop, tuple()}.
init(_Args) ->
    MagellanFile = case get_env(geodata2, dbfile) of
      {ok, File} -> File;
      _ ->
          {stop, {geodata2_dbfile_unspecified}}
    end,
    DomainFile = case get_env(ip_to_domain, dbfile) of
      {ok, File2} -> File2;
      _ ->
          {stop, {ip_to_domain_dbfile_unspecified}}
    end,
    new(MagellanFile, DomainFile).


handle_call(_What, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_env(App, Key) ->
    {ConfigModule, ConfigFun} = case application:get_env(geodata2, config_interp) of
                                  {ok, {Cm, Cf}} ->
                                      {Cm, Cf};
                                  _ ->
                                      {?MODULE, id}
                                end,
    case application:get_env(App, Key) of
      {ok, Value} ->
          {ok, ConfigModule:ConfigFun(Value)};
      Other ->
          Other
    end.

%% this is used to return app env values as-is when config_interp is not set:
id(X) ->
    X.

is_compressed(Filename) ->
    <<".gz">> == iolist_to_binary(filename:extension(Filename)).
