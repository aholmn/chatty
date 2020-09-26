%%%-------------------------------------------------------------------
%% @doc chatty top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chatty_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init(_) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 60},
    Manager  = {manager,
                {chatty_man, start_link, []},
                permanent, 5000, worker, []},
    Client   = {clients,
                {chatty_clients, start_link, []},
                permanent, 5000, worker, []},
    {ok, {SupFlags, [Manager, Client]}}.
