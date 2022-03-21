%%%-------------------------------------------------------------------
%%% @author zsolt
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(telegram_bot).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  logger:notice("telegram_bot server starting"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  logger:notice("telegram_bot server initializing"),
  State = init_state(),
  BotName = maps:get(bot_name,State),
  logger:notice("telegram_bot server subscribing for pe4kin events"),
  pe4kin_receiver:subscribe(BotName, self()),
  logger:notice("telegram_bot starts pe4kin http poll"),
  pe4kin_receiver:start_http_poll(BotName, #{limit=>100, timeout=>60}),
  logger:notice("telegram_bot initializing timer for periodic reports"),
  _TimerRef = erlang:send_after(3600000,self(),timer_expired),
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(timer_expired, State) ->
  logger:notice("telegram_bot periodic timer expired, sending reports to subscribers"),
  Subs = database_server:get_all_subs(),
  ResponseText = last_weather_meas_message(),
  BotName = maps:get(bot_name,State),
  lists:map(
    fun(X) ->
      {ChatId,FirstName} = X,
      case pe4kin:send_message(BotName, #{chat_id => ChatId, text => ResponseText}) of
        {ok,_} -> logger:notice("telegram_bot sent periodic message to: ~s",[FirstName]);
        _      -> logger:notice("telegram_bot FAIL periodic message to: ~s",[FirstName])
      end
      end,
    Subs),
  _TimerRef = erlang:send_after(3600000,self(),timer_expired),
  {noreply, State};

handle_info({pe4kin_update,BotName,Update}, State) ->
  logger:notice("telegram_bot received a pe4kin update"),
  PayloadType = pe4kin_types:update_type(Update),
  Message = maps:get(<<"message">>,Update),
  {ok,ChatId} = pe4kin_types:chat_id(PayloadType,Update),
  {ok,From} = pe4kin_types:user(PayloadType,Update),
  FirstName = maps:get(<<"first_name">>,From),
  logger:notice("telegram_bot update PayloadType = ~p ChatId = ~p, FirstName = ~p ",[PayloadType,ChatId,FirstName]),
  C = case catch(pe4kin_types:message_command(BotName, Message)) of
  {Command, _BotName, true, _} -> Command;
  _                            -> not_a_command
  end,
  logger:notice("telegram_bot update, decoded command: ~p",[C]),
  case C of
    <<"/subscribe">> ->
      logger:notice("telegram_bot subscribe"),
      database_server:store_sub({ChatId,FirstName}),
      ResponseText = unicode:characters_to_binary([<<"Hello ">>, FirstName, <<"! You will receive periodic weather updates.">>]),
      {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => ResponseText}),
      FirstMessage = last_weather_meas_message(),
      {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => FirstMessage});

    <<"/unsubscribe">> ->
      logger:notice("telegram_bot unsubscribe"),
      database_server:remove_sub(ChatId),
      ResponseText = unicode:characters_to_binary([FirstName, <<", You will not receive periodic weather updates anymore.">>]),
      {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => ResponseText});

    <<"/now">> ->
      logger:notice("telegram_bot now"),
      ResponseText = last_weather_meas_message(),
      {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => ResponseText});

    <<"/test12">> ->
      logger:notice("telegram_bot: test12"),
      ResponseText = unicode:characters_to_binary([FirstName,", Test 1 2 á é í ó ö? ú"]),

      {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => ResponseText});


    not_a_command ->
      logger:notice("telegram_bot not a command ignoring input");

    _ ->
      logger:notice("telegram_bot unknown command, ignoring input")
  end,

  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State , _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_state() ->
  StateNew = maps:new(),
  StateBotName = maps:put(bot_name,<<"name_bot">>,StateNew),
  StateBotToken = maps:put(bot_token,<<"bot_token">>,StateBotName),
  StateBotToken.

last_weather_meas_message() ->
  LastMeas = database_server:get_last_meas(),
  Temp = maps:get(<<"temp">>,LastMeas), TempStr = io_lib:format("~7.2f",[Temp]),
  Hum = maps:get(<<"humidity">>,LastMeas), HumStr = io_lib:format("~7.2f",[Hum]),
  Pressure = maps:get(<<"pressure">>,LastMeas), PressureStr = io_lib:format("~7.2f",[Pressure]),
  ResponseText = unicode:characters_to_binary([
    16#1f321,<<" Temperature: ">>,TempStr,<<" *C \n">>,
    16#1f32b,<<" Humidity: ">>,HumStr,<<" % \n">>,
    16#2b07,<<" Pressure: ">>,PressureStr,<<" hPa\n">>]),
  ResponseText.


%#{
% <<"message">> =>
%   #{<<"chat">> =>
%       #{<<"first_name">> => <<"Zsolt">>,<<"id">> => 522158447,<<"type">> => <<"private">>},
%     <<"date">> => 1615640538,
%     <<"entities">> => [#{<<"length">> => 9,<<"offset">> => 0,<<"type">> => <<"bot_command">>}],
%     <<"from">> => #{<<"first_name">> => <<"Zsolt">>,<<"id">> => 522158447,<<"is_bot">> => false,<<"language_code">> => <<"de">>},<<"message_id">> => 236,<<"text">> => <<"/kommando">>},<<"update_id">> => 824755375}}
