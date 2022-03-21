%%%-------------------------------------------------------------------
%% @doc mqtt_test top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mqtt_test_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->

    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 10,
                 period => 1},
    ChildSpecs = [
      database_server_child(),
      mqtt_access_server_child(),
      pe4kin_child(),
      telegram_bot_child()
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

mqtt_access_server_child() ->
  New = maps:new(),
  Id = maps:put(id,mqtt_access_server,New),
  Start = maps:put(start,{mqtt_access_server,start_link,[]},Id),
  Shutdown = maps:put(shutdown,5000,Start),
  Shutdown.

database_server_child() ->
  New = maps:new(),
  Id = maps:put(id,database_server,New),
  Start = maps:put(start,{database_server,start_link,[]},Id),
  Start.

telegram_bot_child() ->
  New = maps:new(),
  Id = maps:put(id,telegram_bot,New),
  Start = maps:put(start,{telegram_bot,start_link,[]},Id),
  Start.

pe4kin_child() ->
  BotName = <<"name_bot">>,
  BotToken = <<"bot_token">>,
  set_env({BotName, token}, BotToken),
  New = maps:new(),
  Id = maps:put(id,pe4kin_receiver,New),
  Start = maps:put(start,{pe4kin_receiver,start_link,[BotName, BotToken, #{receiver => true}]},Id),
  Start.

set_env(Key, Value) ->
  Return = application:set_env(pe4kin, Key, Value),
  Return.
