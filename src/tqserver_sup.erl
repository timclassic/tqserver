%%%-------------------------------------------------------------------------
%%% @author Tim Stewart <tim@stoo.org>
%%%  [http://www.stoo.org]
%%% @copyright 2010 Stoo Research
%%% @doc Supervisor for Tim's Queue Server
%%% @end
%%%-------------------------------------------------------------------------

-module(tqserver_sup).
-behavior(supervisor).

%% Public API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%=========================================================================
%%% Public API
%%%=========================================================================

start_link(LSock) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

start_child() ->
    supervisor:start_child(?MODULE, []).


%%%=========================================================================
%%% Supervisor callbacks
%%%=========================================================================

init([LSock]) ->
    Server = {tqserver, {tqserver, start_link, [LSock]},
              temporary, 2000, worker, [tqserver]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
