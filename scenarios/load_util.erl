-module(load_util).

-include("../deps/wocky_app/apps/wocky_xmpp/include/wocky.hrl").
-include("../deps/wocky_app/apps/wocky_xmpp/test/test_helper.hrl").

-export([server/0,
         create_user/1,
         make_cfg/1,
         connect/1,
         rest/0,
         rest/1,
         load_hs/2,
         load_subscribed_bots/1,
         load_bot/2,
         load_items/2,
         time/2
        ]).

-type config() :: proplists:proplist().

server() ->
    list_to_binary(os:getenv("WOCKY_SERVER", "localhost")).

password() -> <<"abc">>.

-spec create_user(non_neg_integer()) -> escalus_users:user_spec().
create_user(ID) ->
    ?wocky_factory:insert(user, [{password, password()},
                                 {handle, handle(ID)}]).

handle(ID) ->
    <<"User", (integer_to_binary(ID))/binary>>.

-spec make_cfg(?wocky_user:t()) -> config().
make_cfg(#{id := ID}) ->
    Port = list_to_integer(os:getenv("WOCKY_PORT", "5223")),
    SSL = os:getenv("WOCKY_SSL", "true") =:= "true",
    [
     {username, ID},
     {server, server()},
     {host, server()},
     {port, Port},
     {ssl, SSL},
     {password, password()},
     {carbons, false},
     {stream_management, true},
     {resource, ?wocky_id:new()}
    ].

-spec connect(config()) -> ejabberd:client().
connect(Config) ->
    {ok, Conn, Props, _} = escalus_connection:start(Cfg),
    Jid = make_jid(Props),
    Conn#client{jid = Jid}.

rest() -> rest(2).
rest(Seconds) ->
    timer:sleep(rand:uniform(timer:seconds(Seconds))).

load_hs(Client, Count) ->
    test_helper:expect_iq_success_u(
      test_helper:get_hs_stanza(#rsm_in{direction = before, max = Count}),
      Client, Client).

load_subscribed_bots(Client) ->
    S = bot_SUITE:subscribed_stanza(#rsm_in{max = 50}),
    Result = test_helper:expect_iq_success(S, Client),
    Elements = wocky_xml:paths_by_attr(Result, [
                                                {element, <<"bots">>},
                                                {element, <<"bot">>},
                                                {element, <<"field">>}],
                                                <<"var">>, <<"id">>),
    [element(2, wocky_xml:get_subel_cdata(<<"value">>, E)) || E <- Elements].

load_bot(Client, BotID) ->
    test_helper:expect_iq_success(bot_SUITE:retrieve_stanza(BotID), Client).

load_items(Client, BotID) ->
    test_helper:expect_iq_success(
      test_helper:iq_get(
        ?NS_BOT, bot_SUITE:item_query_el(#rsm_in{max = 20}, BotID)), Client).

time(Metric, Fun) ->
    {Time, Result} = timer:tc(Fun),
    amoc_metrics:update_hist(Metric, Time),
    Result.
