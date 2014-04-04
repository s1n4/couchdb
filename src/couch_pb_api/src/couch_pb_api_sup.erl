-module(couch_pb_api_sup).
-behaviour(supervisor).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([start_link/0]).
-export([start_socket/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% -----------------------------------------------------------------------------
%% start a socket acceptor
%% -----------------------------------------------------------------------------
start_socket() ->
    supervisor:start_child(?MODULE, []).

%% -----------------------------------------------------------------------------
%% supervisor callback
%% -----------------------------------------------------------------------------
init([]) ->
    Port = couch_pb_api_listener:get_port(),
    IP = couch_pb_api_listener:get_ip(),
    {ok, Listen} = gen_tcp:listen(Port, [binary, {active, once}, {ip, IP}]),

    {ok, {{simple_one_for_one, 60, 3600},
          [{couch_pb_api_listener, {couch_pb_api_listener, start_link, [Listen]},
            temporary, 1000, worker, [couch_pb_api_listener]}
          ]}}.
