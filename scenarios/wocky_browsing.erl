-module(wocky_browsing).

-include("../deps/wocky_app/apps/wocky_xmpp/include/wocky.hrl").
-include("../deps/wocky_app/apps/wocky_xmpp/test/test_helper.hrl").

-export([setup_db/1, init/0, start/1]).
-import(load_util, [time/2]).

-compile({parse_transform, cut}).

-define(HOST, load_util:server()).
-define(CREATOR_RATIO, 10).
-define(CREATOR_BOTS, 100).
-define(BOT_ITEMS, 20).
-define(HS_ITEMS, 50).

-define(load_helper, 'Elixir.AMOC.LoadHelper').

-define(DB_OPTS, [{timeout, infinity}, {pool_timeout, infinity}]).

-spec setup_db(non_neg_integer()) -> ok.
setup_db(Count) ->
    lager:info("Clearing DB"),
    ?wocky_repo:delete_all(?wocky_user, ?DB_OPTS),
    lager:info("Creating creators"),

    ?wocky_repo:transaction( fun() ->
    CreatorUsers =
    lists:map(
      fun(I) ->
              setup_creator(I)
      end,
      lists:seq(1, Count div ?CREATOR_RATIO)),

    CreatorsForUsers =
    lists:append(lists:duplicate(?CREATOR_RATIO - 1, CreatorUsers)),

    lager:info("Creating users"),
    lists:mapfoldl(
      fun(Creator, ID) ->
              {setup_user(ID, Creator), ID+1}
      end,
      length(CreatorUsers)+1, CreatorsForUsers)
                             end, ?DB_OPTS),
    lager:info("Done").


-spec init() -> ok.
init() ->
    amoc_metrics:new_counter(wocky_browsing_runs),
    amoc_metrics:new_counter(wocky_browsing_errors),

    Histograms  =
        [wocky_browsing_connect_time, wocky_browsing_presence_time,
         wocky_browsing_hs_load_time, wocky_browsing_sub_bot_time,
         wocky_browsing_bot_time, wocky_browsing_bot_items_time],

    lists:foreach(amoc_metrics:new_histogram(_), Histograms),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyID) ->
    try
        lager:info("Starting browsing test"),
        User = ?load_helper:get_user(MyID),
        Cfg = load_util:make_cfg(User),
        Client = time(wocky_browsing_connect_time,
                      fun() -> load_util:connect(Cfg) end),

        time(wocky_browsing_presence_time,
             fun() ->
                     send_presence_available(Client),
                     lager:info("presence resp ~p",
                                [escalus_client:wait_for_stanza(Client)])
             end),

        time(wocky_browsing_hs_load_time,
             fun() -> load_util:load_hs(Client, 50) end),

        [Bot | _Bots] =
            time(wocky_browsing_sub_bot_time,
                 fun() -> load_util:load_subscribed_bots(Client) end),

        lager:info("Loading bot and items"),
        time(wocky_browsing_bot_time,
             fun() -> load_util:load_bot(Client, Bot) end),
        lager:info("Bot loaded"),

        time(wocky_browsing_bot_items_time,
             fun() -> load_util:load_items(Client, Bot) end),
        lager:info("Items loaded"),

        lager:info("Sending unavailable"),
        send_presence_unavailable(Client),
        lager:info("Disconnecting clinet"),
        escalus_connection:stop(Client),
        amoc_metrics:update_counter(wocky_browsing_runs, 1)
    catch
        A:B ->
            lager:info("Scenario failed with: ~p:~p - ~p",
                       [A, B, erlang:get_stacktrace()]),
            amoc_metrics:update_counter(wocky_browsing_errors, 1)
    end.

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

-spec send_presence_unavailable(escalus:client()) -> ok.
send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:send(Client, Pres).

setup_creator(ID) ->
    User = load_util:create_user(ID),
    Bots = ?wocky_factory:insert_list(
              ?CREATOR_BOTS, bot, [{user, User}]),
    lists:foreach(
      fun(Bot) ->
              ?wocky_factory:insert_list(
                 ?BOT_ITEMS, item, [{user, User}, {bot, Bot}])
      end, Bots),
    ?wocky_factory:insert_list(?HS_ITEMS, home_stream_item, [{user, User}]),
    User.

setup_user(ID, Creator) ->
    User = load_util:create_user(ID),
    Bots = ?wocky_user:get_owned_bots(Creator),
    lists:foreach(
      fun(Bot) ->
              ?wocky_factory:insert(share, [{bot, Bot},
                                            {user, User},
                                            {sharer, Creator}]),
              ?wocky_factory:insert(subscription, [{bot, Bot},
                                                   {user, User}])
      end, Bots),
    ?wocky_factory:insert_list(?HS_ITEMS, home_stream_item, [{user, User}]).

make_jid(Proplist) ->
    {username, U} = lists:keyfind(username, 1, Proplist),
    {server, S} = lists:keyfind(server, 1, Proplist),
    {resource, R} = lists:keyfind(resource, 1, Proplist),
    <<U/binary, "@", S/binary, "/", R/binary>>.

