-module(amoc_registry).

-export([start_link/0,
         register/2,
         get/1]).

-export([init/1, handle_info/2, handle_cast/2,
         handle_call/3, terminate/2, code_change/3]).

-define(TABLE, amoc_registry).
-define(SERVER, {global, ?MODULE}).

-behaviour(gen_server).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

register(ID, Username) ->
    gen_server:cast(?SERVER, {register, {ID, Username}}).

get(ID) ->
    case ets:lookup(?TABLE, ID) of
        [{ID, Username}] -> Username;
        [] -> gen_server:call(?SERVER, {wait, ID})
    end.

init(_) ->
    ets:new(?TABLE, [named_table, protected, {read_concurrency, true}]),
    {ok, dict:new()}.

handle_info(_, State) -> {noreply, State}.

handle_cast({register, {ID, Username}}, State) ->
    ets:insert(?TABLE, {ID, Username}),
    case dict:take(ID, State) of
        error ->
            {noreply, State};
        {Waiters, NewState} ->
            notify_waiters(Waiters, Username),
            {noreply, NewState}
    end;

handle_cast(_, State) -> {noreply, State}.

handle_call({wait, ID}, From, State) ->
    case ets:lookup(?TABLE, ID) of
        [{ID, Username}] ->
            {reply, Username, State};
        [] ->
            add_waiter(ID, From, State),
            {noreply, State}
    end;

handle_call(_, _, State) -> {reply, bad_call, State}.

terminate(_, _) -> ok.

code_change(_, _, State) -> State.



notify_waiters(Username, Waiters) ->
    lists:foreach(
        fun(W) -> gen_server:reply(W, Username) end,
        Waiters).

add_waiter(ID, From, State) ->
    NewWaiters =
    case dict:find(ID, State) of
        error -> [From];
        {ok, Existing} -> [From | Existing]
    end,
    dict:store(ID, NewWaiters, State).
