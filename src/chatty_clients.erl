-module(chatty_clients).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_acceptor/0]).

-define(SERVER, ?MODULE).
-define(PORT, 5000).

%% Interface
start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

start_acceptor() ->
    supervisor:start_child(?MODULE, []).

%% Supervisor
init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT,[{active,true},{reuseaddr,true},{packet,line}]),
    spawn_link(fun start_acceptor/0),
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 60},
    ChildSpecs = [{socket,
                   {chatty_client, start_link, [ListenSocket]},
                   temporary, 1000, worker, []}],
    {ok, {SupFlags, ChildSpecs}}.
