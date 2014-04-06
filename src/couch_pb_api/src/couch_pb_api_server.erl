-module(couch_pb_api_server).
-behaviour(gen_fsm).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([start_link/1]).
-export([start/1]).
-export([stop/1]).
-export([wait_for_conn/1]).
-export([authenticate/2]).

%% -----------------------------------------------------------------------------
%% gen_fsm
%% -----------------------------------------------------------------------------
-export([init/1]).
-export([handle_event/3]).
-export([wait_for_conn/2]).
-export([wait_for_auth/3]).
-export([wait_for_command/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-include_lib("couchdb/couch_db.hrl").
-include_lib("couch_pb/include/couch_pb.hrl").

-define(TIMEOUT, 20000).
-define(BADARG, {error, badarg}).
-define(UNAUTHORIZED, {unauthorized, <<"Name or password is incorrect.">>}).

-record(state, {socket, user_ctx = #user_ctx{}}).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
start_link(Listen) ->
    gen_fsm:start_link(?MODULE, Listen, [{timeout, infinity}]).

start(Listen) ->
    gen_fsm:start(?MODULE, Listen, [{timeout, infinity}]).

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

wait_for_conn(Pid) ->
    gen_fsm:send_event(Pid, connection).

authenticate(Pid, AuthInfo={_, _}) ->
    gen_fsm:sync_send_event(Pid, AuthInfo, ?TIMEOUT);
authenticate(_, _) ->
    ?BADARG.

%% -----------------------------------------------------------------------------
%% gen_fsm
%% -----------------------------------------------------------------------------
init(Listen) ->
    {ok, wait_for_conn, #state{socket = Listen}, infinity}.

handle_event(stop, _, State) ->
    {stop, shutdown, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

wait_for_conn(connection, State=#state{socket = Listen}) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    {next_state, wait_for_auth, State#state{socket = Socket}};
wait_for_conn(_, State) ->
    {next_state, wait_for_conn, State}.

wait_for_auth({Username, Password}, _From, State) ->
    %% TODO: make it compatible with admin-party
    Reply = case couch_auth_cache:get_user_creds(Username) of
                nil ->
                    ?UNAUTHORIZED;
                UserProps ->
                    case handle_auth(Password, UserProps) of
                        true ->
                            #user_ctx{
                               name=Username,
                               roles=couch_util:get_value(<<"roles">>, UserProps, [])};
                        _Else ->
                            ?UNAUTHORIZED
                    end
            end,
    case Reply of
        ?UNAUTHORIZED ->
            {reply, Reply, wait_for_auth, State};
        UserCtx ->
            {reply, ok, wait_for_command, State#state{user_ctx = UserCtx}}
    end;
wait_for_auth(_, _From, State) ->
    {reply, ?BADARG, wait_for_auth, State}.

wait_for_command({_, _}, _From, State) ->
    {reply, ok, wait_for_command, State};
wait_for_command(_, _From, State) ->
    {reply, ?BADARG, wait_for_command, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info({tcp_closed, Socket}, _, State=#state{socket = Socket}) ->
    {stop, normal, State};
handle_info({tcp_error, Socket}, _, State=#state{socket = Socket}) ->
    {stop, normal, State};
handle_info({tcp, Socket, Data}, StateName, State=#state{socket = Socket}) ->
    Req = decode(Data),
    F = parse_req(Req),
    Res = F(),
    io:format("res: ~p~n", [Res]),
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}]),
    {next_state, StateName, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% -----------------------------------------------------------------------------
%% internal
%% copied from couch_httpd_auth.erl
%% -----------------------------------------------------------------------------
handle_auth(Pass, UserProps) ->
    UserSalt = couch_util:get_value(<<"salt">>, UserProps, <<>>),
    {PasswordHash, ExpectedHash} =
        case couch_util:get_value(<<"password_scheme">>, UserProps, <<"simple">>) of
            <<"simple">> ->
                {couch_passwords:simple(Pass, UserSalt),
                 couch_util:get_value(<<"password_sha">>, UserProps, nil)};
            <<"pbkdf2">> ->
                Iterations = couch_util:get_value(<<"iterations">>, UserProps, 10000),
                {couch_passwords:pbkdf2(Pass, UserSalt, Iterations),
                 couch_util:get_value(<<"derived_key">>, UserProps, nil)}
        end,
    couch_passwords:verify(PasswordHash, ExpectedHash).

%% encode(Resp) ->
%%     couch_pb_codec:encode_resp(Resp).

decode(Req) ->
    couch_pb_codec:decode_req(Req).

parse_req(Req=#cpbrequest{command = CommandID}) ->
    parse_req(couch_pb_codec:get_command_name(CommandID), Req).

parse_req(authenticate, #cpbrequest{username = User, password = Passwd}) ->
    fun() ->
            authenticate(self(), {User, Passwd})
    end;
parse_req(_, _) ->
    fun() ->
            ok
    end.
