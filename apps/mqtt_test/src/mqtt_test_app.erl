%%%-------------------------------------------------------------------
%% @doc mqtt_test public API
%% @end
%%%-------------------------------------------------------------------

-module(mqtt_test_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok,Started} = application:ensure_all_started(pe4kin),
    logger:notice("started the following dependencies for pe4kin: ~p",[Started]),
    logger:notice("compiling cowboy routes"),
    Dispatch = cowboy_router:compile([
        {'_',
            [
                {"/weather/:last", weather_rest_handler, [{op, get}]},
                {"/[...]", cowboy_static, {priv_dir, mqtt_test, "site",
                    [{mimetypes, cow_mimetypes, all}]}}
            ]}
    ]),

    logger:notice("starting cowboy listener for REST api"),
    {ok, _} = cowboy:start_clear(weather_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    logger:notice("starting the supervisor"),
    mqtt_test_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
