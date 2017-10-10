-module(wocky_browsing).

-include("../deps/wocky_app/apps/wocky_xmpp/include/wocky.hrl").
-include("../deps/wocky_app/apps/wocky_xmpp/test/test_helper.hrl").

-export([setup_db/1, init/0, start/1]).

-compile({parse_transform, cut}).

-define(HOST, load_util:server()).
-define(CREATOR_RATIO, 10).
-define(CREATOR_BOTS, 100).
-define(BOT_ITEMS, 20).
-define(HS_ITEMS, 50).

-define(task, 'Elixir.Task').
-define(load_helper, 'Elixir.AMOC.LoadHelper').

-define(TABLE, wocky_browsing_users).

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
init() -> ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyID) ->
    lager:info("Starting browsing test"),
    User = ?load_helper:get_user(MyID),
    Cfg = load_util:make_cfg(User),
    {ok, Conn, Props, _} = escalus_connection:start(Cfg),

    lager:info("Conn: ~p", [Conn]),
    Jid = make_jid(Props),
    Client = Conn#client{jid = Jid},
    lager:info("client: ~p", [Client]),

    send_presence_available(Client),
    lager:info("presence resp ~p", [escalus_client:wait_for_stanza(Client)]),

    load_util:load_hs(Client, 50),
    load_util:rest(),
    [Bot | _Bots] = load_util:load_subscribed_bots(Client),
    load_util:rest(),
    lager:info("Loading bot and items"),
    load_util:load_bot(Client, Bot),
    lager:info("Bot loaded"),
    load_util:load_items(Client, Bot),
    lager:info("Items loaded"),
    load_util:rest(),

    lager:info("Sending unavailable"),
    send_presence_unavailable(Client),
    lager:info("Disconnecting clinet"),
    escalus_connection:stop(Client).

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

