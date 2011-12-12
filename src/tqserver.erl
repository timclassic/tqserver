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

-record(state, {lsock, queue}).


%%%=========================================================================
%%% Public API
%%%=========================================================================

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).


%%%=========================================================================
%%% gen_server callbacks
%%%=========================================================================

init([LSock]) ->
    Q = tqueue:new(),
    {ok, #state{lsock = LSock, queue = Q}, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Socket} = gen_tcp:accept(LSock),
    {ok, _Pid} = tqserver_sup:start_child(),
    {noreply, State};
handle_info({tcp, Socket, Data}, State) ->
    {ok, NewState} = do_comms(Socket, Data, State),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _Socket}, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%=========================================================================
%%% Internal functions
%%%=========================================================================

do_comms(Socket, Data, State) ->
    {Msg, NewState} =
        try
            {Cmd, Term} = parse_line(Data),
            {ok, OkMsg, OkState} = run_cmd(Cmd, Term, State),
            {OkMsg, OkState}
        catch
            _Class:Err ->
                case Err of
                    invalid_cmd ->
                        ErrMsg = io_lib:fwrite("ERROR: invalid command~n", []);
                    Else ->
                        ErrMsg = io_lib:fwrite("ERROR(unhandled): ~p~n", [Else])
                end,
                {ErrMsg, State}
        end,
    gen_tcp:send(Socket, Msg),
    {ok, NewState}.


parse_line(Data) ->
    Line = re:replace(Data, "\r\n\$", "", [{return, list}]),

    %% Command parsing is naive.  For example, "outt" is considered
    %% valid and will register as the out operation
    case re:run(Line, "^(in|out) *(.*)\$", [{capture, [1, 2], list}]) of
        {match, [Cmd, Term]} ->
            {Cmd, Term};
        _Else ->
            throw(invalid_cmd)
    end.

run_cmd(Cmd, Term, #state{queue = Q} = State) ->
    case Cmd of
        "in" ->
            NewQ = tqueue:in(Q, Term),
            Msg = io_lib:fwrite("OK: data added to queue~n", []),
            NewState = State#state{queue = NewQ};
        "out" ->
            case tqueue:out(Q) of
                {ok, NewQ, OutTerm} ->
                    Msg = io_lib:fwrite("OK: ~p~n", [OutTerm]),
                    NewState = State#state{queue = NewQ};
                {empty, _NewQ} ->
                    Msg = io_lib:fwrite("WARN: Queue is empty~n", []),
                    NewState = State
            end
    end,
    {ok, Msg, NewState}.
