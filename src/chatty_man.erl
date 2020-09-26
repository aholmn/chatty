-module(chatty_man).
-behaviour(gen_server).

-export([start_link/0, broadcast/2, connect/1, get_clients/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% Interface
broadcast(Msg, Pid) ->
    gen_server:cast(?MODULE, {broadcast, Msg, Pid}).

connect(Pid) ->
    chatty_clients:start_acceptor(),
    gen_server:cast(?MODULE, {connect, Pid}).

get_clients() ->
    gen_server:call(?MODULE, get_clients).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{pids => []}}.

handle_call(get_clients, _From, #{pids := Pids} = State) ->
    {reply, Pids, State}.

handle_cast({connect, Pid}, #{pids := Pids} = State) ->
    io:format("Pid: ~p connected~n", [Pid]),
    erlang:monitor(process, Pid),
    NewState = State#{pids := [Pid | Pids]},
    {noreply, NewState};

handle_cast({broadcast, Msg, Pid}, #{pids := Pids} = State) ->
    [chatty_client:send(P, Msg) || P <- Pids, P =/= Pid],
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, #{pids := Pids} = State) ->
    io:format("Pid: ~p down, reason: ~p~n", [Pid, Reason]),
    NewState = State#{pids := [P || P <- Pids, P =/= Pid]},
    {noreply, NewState}.
