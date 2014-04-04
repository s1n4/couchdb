-module(couch_pb_api_listener).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([start_link/1]).
-export([get_ip/0]).
-export([get_port/0]).

-include_lib("couch_pb/include/couch_pb.hrl").

-record(state, {server, authorized = false}).

start_link(Listen) ->
    {ok, spawn_link(fun() ->
                            {ok, Socket} = gen_tcp:accept(Listen),
                            {ok, P} = couch_pb_api_server:start_link(),
                            auth_loop(Socket, #state{server = P})
                    end)}.

%% -----------------------------------------------------------------------------
%% get IP address or return the default IP address
%% -----------------------------------------------------------------------------
get_ip() ->
    Address = couch_config:get("pb_interface", "ip", "127.0.0.1"),
    {ok, IP} = inet:parse_address(Address),
    IP.

%% -----------------------------------------------------------------------------
%% get port number or return the defult port number
%% -----------------------------------------------------------------------------
get_port() ->
    list_to_integer(couch_config:get("pb_interface", "port", "5985")).

%% -----------------------------------------------------------------------------
%% socket listener
%% -----------------------------------------------------------------------------
auth_loop(Socket, State=#state{server = Server}) ->
    receive
        {tcp, Socket, Data} ->
            Data1 = decode(Data),
            AuthInfo = {Data1#cpbrequest.username, Data1#cpbrequest.password},
            {Res, F} = case couch_pb_api_server:authenticate(Server, AuthInfo) of
                           ok ->
                               {#cpbresponse{resp_code = 0},
                                fun() -> command_loop(Socket, State) end};
                           {unauthorized, Msg} ->
                               {#cpbresponse{resp_code = 1, result = #cpbresult{msg = Msg}},
                                fun() -> auth_loop(Socket, State) end}
                       end,
            gen_tcp:send(Socket, encode(Res)),
            inet:setopts(Socket, [{active, once}]),
            F();
        {tcp_closed, Socket} ->
            io:format("socket closed!~n"),
            gen_tcp:close(Socket)
    end.

command_loop(Socket, State=#state{server = Server}) ->
    receive
        {tcp, Socket, Data} ->
            Data1 = decode(Data),
            Res = couch_pb_api_server:exec_command(Server, Data1),
            gen_tcp:send(Socket, encode(Res)),
            inet:setopts(Socket, [{active, once}]),
            command_loop(Socket, State);
        {tcp_closed, Socket} ->
            io:format("socket closed!~n"),
            gen_tcp:close(Socket)
    end.

encode(Resp) ->
    couch_pb_codec:encode_resp(Resp).

decode(Req) ->
    couch_pb_codec:decode_req(Req).
