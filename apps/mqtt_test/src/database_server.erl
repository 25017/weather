%%%-------------------------------------------------------------------
%%% @author zsolt
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(database_server).
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([
  store_meas/1,
  store_sub/1,
  remove_sub/1,
  get_last_meas/0,
  check_sub/1,
  get_all_subs/0,
  get_meas/1,
  get_all_meas/1]).

-define(SERVER, ?MODULE).



%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  logger:notice("database_server starting"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  logger:notice("database_server initializing state"),
  StateNew = init_state(),
  logger:notice("database_server initializing measurements DETS file"),
  {ok, MeasTable} = dets:open_file("measurements.dat", [{ram_file,true}]),
  logger:notice("database_server initializing telegram subscriptions DETS file"),
  {ok, TelegramTable} = dets:open_file("telegram.dat", []),
  StateTableM = maps:put(meas_table,MeasTable,StateNew),
  StateTableT = maps:put(telegram_table,TelegramTable,StateTableM),
  {ok, StateTableT}.

handle_call({store_meas,Meas}, _From, State ) ->
  MeasTable = maps:get(meas_table,State),
  %% {{Y,M,D},{H,Min,_S}} = calendar:local_time(),
  %% Time = lists:flatten(io_lib:format("~p-~2..0B-~2..0B ~2..0B:~2..0B",[Y,M,D,H,Min])),
  Time = os:system_time(1000),
  MeasRecord = {Time,Meas},
  ok = dets:insert(MeasTable,MeasRecord),
  ok = dets:sync(MeasTable),
  logger:notice("database_server stored measurement into table"),
  logger:notice("database_server ~p",[MeasRecord]),
  NewState = maps:put(last,Meas,State),
  {reply, ok, NewState};

handle_call(get_last_meas, _From, State ) ->
  LastMeas = maps:get(last,State),
  {reply, LastMeas, State};


handle_call({store_sub,SubData}, _From, State ) ->
  TelegramTable = maps:get(telegram_table,State),
  ok = dets:insert(TelegramTable,SubData),
  logger:notice("database_server stored telegram subscription into table"),
  logger:notice("database_server ~p",[SubData]),
  {reply, ok, State};

handle_call({remove_sub,ChatId}, _From, State ) ->
  TelegramTable = maps:get(telegram_table,State),
  ok = dets:delete(TelegramTable,ChatId),
  logger:notice("database_server removed telegram subscription from table"),
  logger:notice("database_server ~p",[ChatId]),
  {reply, ok, State};

handle_call({check_sub,ChatId}, _From, State ) ->
  TelegramTable = maps:get(telegram_table,State),
  Result = dets:lookup(TelegramTable,ChatId),
  {reply, Result, State};

handle_call(get_all_subs, _From, State ) ->
  TelegramTable = maps:get(telegram_table,State),
  Result = dets:traverse(TelegramTable,fun(X) -> {continue, X} end),
  {reply, Result, State};

handle_call({get_meas, N}, _From, State ) ->
  MeasTable = maps:get(meas_table,State),
  Time = os:system_time(1000),
  NewTime = Time - (N*60*1000),
  Match = ets:fun2ms(fun({X,Y}) when X >= NewTime -> {X,Y} end),
  Result = dets:select(MeasTable,Match),
  {reply, Result, State};


handle_call(get_all_meas, _From, State ) ->
  MeasTable = maps:get(meas_table,State),
  Result = dets:traverse(MeasTable,
    fun(X) ->
%%      {TS,MAP} = X,
%%      TS2 = list_to_binary(TS),
%%      MAP2 = maps:merge(MAP,#{<<"timestamp">> => TS2 }),
      {continue, X}
    end),
  {reply, Result, State}.

handle_cast(_Request, State ) ->
  {noreply, State}.

handle_info(_Info, State ) ->
  {noreply, State}.

terminate(_Reason, _State ) ->
  ok.

code_change(_OldVsn, State , _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_state() ->
  StateNew = maps:new(),
  StateLast = maps:put(last,maps:new(),StateNew),
  StateLast.


%%%===================================================================
%%% API functions
%%%===================================================================
store_meas(Meas) ->
  gen_server:call(?SERVER,{store_meas,Meas}).

get_last_meas() ->
  gen_server:call(?SERVER,get_last_meas).

store_sub(SubData) ->
  gen_server:call(?SERVER,{store_sub,SubData}).

remove_sub(ChatId) ->
  gen_server:call(?SERVER,{remove_sub,ChatId}).

check_sub(ChatId) ->
  gen_server:call(?SERVER,{check_sub,ChatId}).

get_all_subs() ->
  gen_server:call(?SERVER,get_all_subs).

get_meas(LastIdU) ->
  LastId = erlang:binary_to_integer(LastIdU),
  SelectedMeas = gen_server:call(?SERVER,{get_meas,LastId}),
  SortedMeas = lists:keysort(1,SelectedMeas),
  Result = lists:map(
    fun(X) ->
      {TS,MAP} = X,
      TS2 = calendar:system_time_to_local_time(TS,1000),
      {{Y,M,D},{H,Min,_S}} = TS2,
      TS3 = lists:flatten(io_lib:format("~p-~2..0B-~2..0B ~2..0B:~2..0B",[Y,M,D,H,Min])),
      MAP2 = maps:merge(MAP,#{<<"timestamp">> => list_to_binary(TS3) }),
      MAP2
    end,
    SortedMeas),
  Result.

get_all_meas(LastIdU) ->
  LastId = erlang:binary_to_integer(LastIdU),
  AllMeas = gen_server:call(?SERVER,get_all_meas),
  SortedMeas = lists:keysort(1,AllMeas),
  Result = lists:map(
    fun(X) ->
      {TS,MAP} = X,
      %%TS2 = list_to_binary(TS),
      TS2 = calendar:system_time_to_local_time(TS,1000),
      {{Y,M,D},{H,Min,_S}} = TS2,
      TS3 = lists:flatten(io_lib:format("~p-~2..0B-~2..0B ~2..0B:~2..0B",[Y,M,D,H,Min])),
      MAP2 = maps:merge(MAP,#{<<"timestamp">> => list_to_binary(TS3) }),
      MAP2
    end,
    SortedMeas),
  ResLen = erlang:length(Result),
  case ResLen < LastId of
    true ->
      Result;
    false ->
      {_FirstPart,SecondPart} = lists:split(ResLen-LastId,Result),
      SecondPart
  end.

