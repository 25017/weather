%%%-------------------------------------------------------------------
%%% @author zsolt
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").

-module(mqtt_access_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  process_flag(trap_exit, true),
  logger:notice("mqtt_access_server: starting"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  logger:notice("mqtt_access_server: initializing state"),
  timer:sleep(1000),
  State = init_state(),
  logger:notice("mqtt_access_server: Starting emqtt"),
  MQTT_options = maps:get(mqtt_options,State),
  {ok, ConnPid} = emqtt:start_link(MQTT_options),
  NewState = maps:put(conn_pid,ConnPid,State),
  Result = conn_sub(NewState),
  logger:notice("mqtt_access_server: Connection: ~p",[Result]),
  {ok, NewState}.

handle_call(_Request, _From, State ) ->
  {reply, ok, State}.

handle_cast(_Request, State ) ->
  {noreply, State}.

handle_info({publish,Publish}, State ) ->
  logger:notice("mqtt_access_server received publish message from broker"),
  Payload = maps:get(payload,Publish),
  Decode = jsone:decode(Payload),
  %%io:format("| T: ~7.2f *C | P: ~7.2f hPa | H: ~7.2f % |~n",[
  %%  maps:get(<<"temp">>,Decode),
  %%  maps:get(<<"pressure">>,Decode),
  %%  maps:get(<<"humidity">>,Decode)
  %%]),
  logger:notice("mqtt_access_server requesting database_server to store measurement"),
  database_server:store_meas(Decode),
  {noreply, State};

handle_info(Msg, State ) ->
  logger:notice("mqtt_access_server: Info catch-all: ~n",[Msg]),
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
  MQTT_options = [
    {owner,self()},
    {host,"172.16.1.7"},
    {port,1883},
    {username,"weather"},
    {password,"weather"}
  ],
  State = maps:put(mqtt_options,MQTT_options,StateNew),
  State.

conn_sub(State) ->
  ConnPid = maps:get(conn_pid,State),
  Result = emqtt:connect(ConnPid),
  case Result of
    {ok,_Props} ->
      SubOpts = [{qos, 1}],
      {ok, _Props2, _ReasonCodes} = emqtt:subscribe(ConnPid, #{}, [{<<"weather">>, SubOpts}]),
      logger:notice("mqtt_access_server: subscription OK!");
    {error,Reason} ->
      logger:notice("mqtt_access_server: Can't connect to broker: ~p",[Reason])
  end,
  {R,_} = Result,
  R.
