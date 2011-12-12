%%%-------------------------------------------------------------------------
%%% @author Tim Stewart <tim@stoo.org>
%%%  [http://www.stoo.org]
%%% @copyright 2010 Stoo Research
%%% @doc Main gen_server for Tim's Queue Server
%%% @end
%%%-------------------------------------------------------------------------

-module(tqserver).
-behavior(gen_server).

%% Public API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(ACCEPT_TIMEOUT, 100).

-record(state, {lsock}).


%%%=========================================================================
%%% Public API
%%%=========================================================================

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).


%%%=========================================================================
%%% gen_server callbacks
%%%=========================================================================

init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.

handle_call(Request, From, State) ->
    io:format("unexpected handle_call:~nRequest: ~p~nFrom: ~p~n",
              [Request, From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    io:format("unexpected handle_cast: Msg: ~p~n", [Msg]),
    {noreply, State}.

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Socket} = gen_tcp:accept(LSock),
    {ok, _Pid} = tqserver_sup:start_child(),
    {noreply, State};
handle_info({tcp, Socket, Data}, State) ->
    ok = handle_line(Socket, Data),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _Socket}, State) ->
    {stop, normal, State};
handle_info(Info, State) ->
    io:format("unexpected handle_info: Info: ~p~n", [Info]),
    {noreply, State, 0}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%=========================================================================
%%% Internal functions
%%%=========================================================================

handle_line(Socket, Data) ->
    io:format("~p: ~p~n", [Socket, Data]),
    ok.
