%%%-------------------------------------------------------------------
%%% @author zsolt
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Mar 2021 22:41
%%%-------------------------------------------------------------------
-module(weather_rest_handler).
-author("zsolt").

%% API
-export([]).

%%
%% Cowboy callbacks
-export([
  init/2,
  allowed_methods/2,
 %% content_types_accepted/2,
  content_types_provided/2,
  to_json/2,
  to_json_get/2
]).

%%
%% Additional callbacks
-export([
]).

-record(state, {op, response}).

init(Req, Opts) ->
  [{op, Op} | _] = Opts,
  State = #state{op=Op, response=none},
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Value = [<<"GET">>, <<"HEAD">>, <<"OPTIONS">>],
    {Value, Req, State}.

%%content_types_accepted(Req, State) ->
%%  Value = [
%%    %{{ <<"text">>, <<"json">>, '*'}, from_json},
%%    {<<"application/x-www-form-urlencoded">>, from_json}
%%  ], %test
%%  {Value, Req, State}.

content_types_provided(Req, State) ->
  Value = [
    {{ <<"application">>, <<"json">>, '*'}, to_json}
  ],
  {Value, Req, State}.


to_json(Req, #state{op=Op} = State) ->
  Result = case Op of
             get ->
               to_json_get(Req, State)
           end,
  Result.

to_json_get(Req, State) ->
  LastId=cowboy_req:binding(last,Req),
  M = database_server:get_meas(LastId),
  Body1 = jsone:encode(M,[{float_format, [{decimals, 2}]}]),
  {Body1, Req, State}.

%% test comment for git commit

%%allow_missing_post(Req, State) ->
%%    Value = true,
%%    {Value, Req, State}.

%%charsets_provided(Req, State) ->
%%    % Example:
%%    % Value = [{{ <<"text">>, <<"json">>, '*'}, from_json}],
%%    Value = skip,
%%    {Value, Req, State}.

%%content_types_accepted(Req, State) ->
%%    Value = [{{ <<"text">>, <<"html">>, '*'}, to_html}],
%%    Value = none,
%%    {Value, Req, State}.

%%content_types_provided(Req, State) ->
%%    Value = [{{ <<"text">>, <<"html">>, '*'}, to_html}],
%%    {Value, Req, State}.

%%delete_completed(Req, State) ->
%%    Value = true,
%%    {Value, Req, State}.

%%delete_resource(Req, State) ->
%%    Value = false,
%%    {Value, Req, State}.

%%expires(Req, State) ->
%%    Value = undefined,
%%    {Value, Req, State}.

%%forbidden(Req, State) ->
%%    Value = false,
%%    {Value, Req, State}.

%%generate_etag(Req, State) ->
%%    Value = undefined,
%%    {Value, Req, State}.

%%is_authorized(Req, State) ->
%%    Value = true,
%%    {Value, Req, State}.

%%is_conflict(Req, State) ->
%%    Value = false,
%%    {Value, Req, State}.

%%known_methods(Req, State) ->
%%    Value = [
%%             <<"GET">>,
%%             <<"HEAD">>,
%%             <<"POST">>,
%%             <<"PUT">>,
%%             <<"PATCH">>,
%%             <<"DELETE">>,
%%             <<"OPTIONS">>
%%            ],
%%    {Value, Req, State}.

%%languages_provided(Req, State) ->
%%    Value = skip,
%%    {Value, Req, State}.

%%last_modified(Req, State) ->
%%    Value = undefined,
%%    {Value, Req, State}.

%%malformed_request(Req, State) ->
%%    Value = false,
%%    {Value, Req, State}.

%%moved_permanently(Req, State) ->
%%    Value = false,
%%    {Value, Req, State}.

%%moved_temporarily(Req, State) ->
%%    Value = false,
%%    {Value, Req, State}.

%%multiple_choices(Req, State) ->
%%    Value = false,
%%    {Value, Req, State}.

%%options(Req, State) ->
%%    Value = ok,
%%    {Value, Req, State}.

%%previously_existed(Req, State) ->
%%    Value = false,
%%    {Value, Req, State}.

%%resource_exists(Req, State) ->
%%    Value = true,
%%    {Value, Req, State}.

%%service_available(Req, State) ->
%%    Value = true,
%%    {Value, Req, State}.

%%uri_too_long(Req, State) ->
%%    Value = false,
%%    {Value, Req, State}.

%%valid_content_headers(Req, State) ->
%%    Value = true,
%%    {Value, Req, State}.

%%valid_entity_length(Req, State) ->
%%    Value = true,
%%    {Value, Req, State}.

%%variances(Req, State) ->
%%    Value = [],
%%    {Value, Req, State}.