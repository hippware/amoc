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
         time/2,
         do_geosearch/2,
         do_initial_presence/1
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
    {ok, Conn, Props, _} = escalus_connection:start(Config),
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

make_jid(Proplist) ->
    {username, U} = lists:keyfind(username, 1, Proplist),
    {server, S} = lists:keyfind(server, 1, Proplist),
    {resource, R} = lists:keyfind(resource, 1, Proplist),
    <<U/binary, "@", S/binary, "/", R/binary>>.

do_geosearch(Client, {Lat, Lon}) ->
    QueryEl =
    #xmlel{name = <<"bots">>,
           children = [#xmlel{name = <<"explore-nearby">>,
                              attrs = [{<<"limit">>, integer_to_binary(200)},
                                       {<<"radius">>, float_to_binary(10000.0)},
                                       {<<"lat">>, float_to_binary(Lat)},
                                       {<<"lon">>, float_to_binary(Lon)}]}]},
    Stanza = test_helper:iq_get(?NS_BOT, QueryEl),
    escalus:send(Client, test_helper:add_to_s(Stanza, Client)),
    await_result(Client).

await_result(Client) ->
    Result = escalus:wait_for_stanza(Client, 60000),
    case escalus_pred:is_iq_result(Result) of
        true -> Result;
        false ->
            io:fwrite("Awaiting result but got. ~p", [Result]),
            await_result(Client)
    end.

do_initial_presence(Client) ->
    send_presence_available(Client),
    wait_presence_result(Client).

wait_presence_result(Client) ->
    S = escalus_client:wait_for_stanza(Client, 60000),
    case escalus_pred:is_presence(S) of
        true -> ok;
        false -> wait_presence_result(Client)
    end.

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).
