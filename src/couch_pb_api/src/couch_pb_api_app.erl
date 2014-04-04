-module(couch_pb_api_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_StartType, _StartArgs) ->
    couch_pb_api_sup:start_link().

stop(_State) ->
    ok.
