%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(wocky_login).

-define(HOST, load_util:server()).

-behaviour(amoc_scenario).

-export([start/1]).
-export([init/0]).

-spec init() -> ok.
init() ->
    lager:info("init the scenario"),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(_MyId) ->
    Cfg = load_util:create_user(),

    {ok, Client, _, _} = escalus_connection:start(Cfg),

    %%Allow presence stanza only
    AllowPresence = fun escalus_pred:is_presence/1,

    escalus_connection:set_filter_predicate(Client, AllowPresence),
    %%Drop all stanzas
    %escalus_connection:set_filter_predicate(Client, none),

    send_presence_available(Client),
    lager:info("presence resp ~p", [escalus_client:wait_for_stanza(Client)]),
    timer:sleep(5000),

    timer:sleep(10*1000),
    send_presence_unavailable(Client),
    escalus_connection:stop(Client).

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

-spec send_presence_unavailable(escalus:client()) -> ok.
send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:send(Client, Pres).
