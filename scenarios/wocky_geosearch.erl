-module(wocky_geosearch).

-include("../deps/wocky_app/apps/wocky_xmpp/include/wocky.hrl").
-include("../deps/wocky_app/apps/wocky_xmpp/test/test_helper.hrl").

-export([setup_db/1, init/0, start/1]).
-import(load_util, [time/2]).

-compile({parse_transform, cut}).

-define(HOST, load_util:server()).
-define(CREATOR_BOTS, 1000).

-define(load_helper, 'Elixir.AMOC.LoadHelper').
-define(wocky_geoutils, 'Elixir.Wocky.GeoUtils').
-define(enum, 'Elixir.Enum').

-define(DB_OPTS, [{timeout, infinity}, {pool_timeout, infinity}]).

-define(HOTSPOTS, [{0.0, 0.0}, {50.0, 50.0}, {-100.0, 50.0}, {200.0, 100.0}]).
-define(MAX_OFFSET, 0.4).

-spec setup_db(non_neg_integer()) -> ok.
setup_db(Count) ->
    lager:info("Clearing DB"),
    ?wocky_repo:delete_all(?wocky_user, ?DB_OPTS),
    lager:info("Creating creators"),

    ?wocky_repo:transaction( fun() ->
                                     lists:foreach(
                                       fun(I) ->
                                               setup_creator(I),
                                               io:fwrite("~p,", [I])
                                       end,
                                       lists:seq(1, Count))
                             end, ?DB_OPTS),

    lager:info("Done").


-spec init() -> ok.
init() ->
    amoc_metrics:new_counter(wocky_geosearch_runs),
    amoc_metrics:new_counter(wocky_geosearch_errors),
    amoc_metrics:new_counter(wocky_geosearch_no_more_bots),
    amoc_metrics:new_counter(wocky_geosearch_bot_limit_reached),

    Histograms  =
        [wocky_geosearch_first_bot_time, wocky_geosearch_bot_interval,
         wocky_geosearch_total_bots, wocky_geosearch_total_time],

    lists:foreach(amoc_metrics:new_histogram(_), Histograms),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyID) ->
    try
        lager:info("Starting browsing test"),
        User = ?load_helper:get_user(MyID),
        Cfg = load_util:make_cfg(User),
        Client = load_util:connect(Cfg),

        send_presence_available(Client),
        escalus_client:wait_for_stanza(Client),

        run_test(Client)
    catch
        A:B ->
            lager:info("Scenario failed with: ~p:~p - ~p",
                       [A, B, erlang:get_stacktrace()]),
            amoc_metrics:update_counter(wocky_geosearch_errors, 1)
    end.

run_test(Client) ->
    time(wocky_geosearch_total_time,
         fun() ->
                 load_util:do_geosearch(Client, rand_point()),
                 get_geosearch_results(Client, wocky_geosearch_first_bot)
         end),
    run_test(Client).

get_geosearch_results(Client, Metric) ->
    amoc_metrics:update_counter(wocky_geosearch_runs, 1),
    R = time(Metric, fun() -> get_geosearch_result(Client) end),
    case R of
        done -> ok;
        more -> get_geosearch_results(Client, wocky_geosearch_bot_interval)
    end.

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

setup_creator(ID) ->
    User = load_util:create_user(ID),
    lists:foreach(fun(_) -> make_bot(User) end, lists:seq(1, ?CREATOR_BOTS)),
    User.

make_bot(User) ->
    {Lat, Lon} = rand_point(),
    ?wocky_factory:insert(bot, [{user, User},
                                {location, ?wocky_geoutils:point(Lat, Lon)},
                                {public, rand:uniform(2) =:= 2},
                                {shortname, <<(?wocky_id:new())/binary>>}
                               ]).

rand_point() ->
    {Lat, Lon} = ?enum:random(?HOTSPOTS),
    {offset(Lat), offset(Lon)}.

offset(Val) ->
    Val + (rand:uniform() * (?MAX_OFFSET * 2.0) - ?MAX_OFFSET).

get_geosearch_result(Client) ->
    Stanza = escalus_client:wait_for_stanza(Client),
    El = xml:get_path_s(Stanza, [{elem, <<"explore-nearby-result">>}]),
    case (hd(El#xmlel.children))#xmlel.name of
        <<"bot">> -> more;
        <<"no-more-bots">> ->
            amoc_metrics:update_counter(wocky_geosearch_no_more_bots, 1),
            done;
        <<"bot-limit-reached">> ->
            amoc_metrics:update_counter(wocky_geosearch_bot_limit_reached, 1),
            done
    end.

