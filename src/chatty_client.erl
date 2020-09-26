-module(chatty_client).

-behaviour(gen_server).

-export([start_link/1, send/2]).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2]).

-define(WELCOME_MSG, "Welcome to chat\ntype 'list for commands\n").
-define(COMMANDS, "'quit    leaves chat\n"
                  "'list    show list of commands\n").

%% Interface
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

send(Pid, Msg) ->
    gen_server:cast(Pid, {send, Msg}).

%% Gen server
init(ListenSocket) ->
    gen_server:cast(self(), accept),
    {ok, #{socket => ListenSocket}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(accept, #{socket := ListenSocket} = State) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    chatty_man:connect(self()),
    do_send(Socket, ?WELCOME_MSG),
    {noreply, State#{socket := Socket}};

handle_cast({send, Msg}, #{socket := Socket} = State) ->
    do_send(Socket, Msg),
    {noreply, State}.

handle_info({tcp, Socket, Str}, State) ->
    case Str of
        "'quit\n" ->
            gen_tcp:close(Socket),
            {stop, normal, State};
        "'list\n" ->
            do_send(Socket, ?COMMANDS),
            {noreply, State};
        _ ->
            chatty_man:broadcast(Str, self()),
            {noreply, State}
    end;

handle_info({tcp_closed, _Socket}, #{socket := Socket} = S) ->
    gen_tcp:close(Socket),
    {stop, normal, S};

handle_info({tcp_error, _Socket, _}, S) ->
    {stop, normal, S};

handle_info(Event, State) ->
    io:format("unexpected: ~p~n", [Event]),
    {noreply, State}.

%% Internals
do_send(Socket, Msg) ->
    ok = gen_tcp:send(Socket, Msg),
    ok.
