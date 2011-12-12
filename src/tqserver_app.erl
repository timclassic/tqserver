%%%-------------------------------------------------------------------------
%%% @author Tim Stewart <tim@stoo.org>
%%%  [http://www.stoo.org]
%%% @copyright 2010 Stoo Research
%%% @end
%%%-------------------------------------------------------------------------

-module(tqserver_app).
-behavior(application).

%% OTP Application callbacks
-export([start/2, stop/1]).

-define(APP, tqserver).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _StartArgs) ->
    ok = validate_env(),
    {ok, Port} = application:get_env(listen_port),
    {ok, LSock} = gen_tcp:listen(Port, []),
    case tqserver_sup:start_link(LSock) of
	{ok, Pid} ->
            tqserver_sup:start_child(),
	    {ok, Pid};
	Other ->
	    {error, Other}
    end.

stop(_State) ->
    ok.


%%%=========================================================================
%%% Internal functions
%%%=========================================================================

validate_env() ->
    ok = default_env(listen_port, 2020),
    ok.

default_env(Key, Default) ->
    case application:get_env(Key) of
	undefined ->
	    ok = application:set_env(?APP, Key, Default),
	    ok;
	{ok, _Val} ->
	    ok
    end.
