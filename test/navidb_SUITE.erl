%%% Test suite for the navidb module.
-module(navidb_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(LOG(Talue),       ct:pal("CT_LOG: ~p~n", [Value])).
-define(LOG(Title, Args), ct:pal(Title, Args)).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [
    {group, db},
    {group, subs}
].

groups() ->
    [
        {db,   [parallel], [test1, test2]},
        {subs, [parallel], [test3]}
    ].

init_per_suite(Config) ->
    error_logger:tty(false),
    ok = application:load(navidb),

    {ok, _PoolConfig} = application:get_env(navidb, connect_pool),
    % ok = application:set_env(navidb, hostname, "localhost"),
    % ok = application:set_env(navidb, port, 27017),

    % Это единственное значение, которое не имеет умолчания и должно быть задано явно
    ok = application:set_env(navidb, database, <<"navicc_test">>),

    % Можно загрузить модули вручную
    % syntax_tools,compiler,goldrush,lager,mnesia,poolboy,bson,mongodb,navidb
    {ok, Modules} = application:ensure_all_started(navidb),
    [{modules, Modules} | Config].

end_per_suite(Config) ->
    Modules = ?config(modules, Config),
    [application:stop(Module) || Module <- lists:reverse(Modules)],
    application:unload(navidb),
    % meck:unload(),
    error_logger:tty(true),
    ok.

init_per_group(db, Config) ->
    ct:pal("TODO: init_per_group(db, ~p)", [Config]),
    Config;

init_per_group(subs, Config) ->
    ct:pal("TODO: init_per_group(subs, ~p)", [Config]),
    Config.

end_per_group(db, Config) ->
    ct:pal("TODO: end_per_group(db, ~p)", [Config]),
    Config;

end_per_group(subs, Config) ->
    ct:pal("TODO: end_per_group(subs, ~p)", [Config]),
    Config.

%% @doc Test1
%%
test1(_) ->
    ?LOG("*********** Test1 ***********", []),
    % Chech modules id load
    ?assertNotEqual(undefined, whereis(navidb_gpsdb)),
    ?assertNotEqual(undefined, global:whereis_name(navidb_subs)), % Зарегестрирован как глобальный модуль
    ?assertNotEqual(undefined, whereis(navidb_mongo_pool)), % Пул воркеров к базе данных
    ?assertEqual({ready,5,0,0}, poolboy:status(navidb_mongo_pool)),
    timer:sleep(200),  % Ensure indexes was created

    ok.


%% @doc Test2
%%
test2(_) ->
    FakeUsername = <<"baden">>,

    ok = navidb:remove(accounts, {username, FakeUsername}),

    FakeAccount = #{
        username => FakeUsername,
        password => <<"111">>,
        date     => 0,
        premium  => 0,
        groups   => []
    },
    #{id := Id1} = navidb:insert(accounts, FakeAccount),
    ?LOG("      Id1 = ~p", [Id1]),
    Res2 = navidb:get(accounts, {username, FakeUsername}, {filter, ['_id', 'password']}),
    #{id := Id2} = Res2,     % Я не понимаю почему Id отличается от того, который назначен при insert
    ?assertMatch(#{
                    username := FakeUsername,
                    date     := 0,
                    premium  := 0,
                    groups   := []
                }, Res2),
    ?LOG("      Id2 = ~p", [Id2]),
    ?assertException(error, {badmatch, _Reason}, #{'_id' := _} = Res2),
    ?assertException(error, {badmatch, _Reason}, #{password := _} = Res2),

    ok.

test3(_) ->
    ?LOG(" TODO: Test subscribes *********** Test3 ***********", []),
    FakeUsername = <<"baden">>,
    % groups
    ?LOG("      Test subscribes", []),
    Self = self(),
    Listener = spawn(fun() -> listener(Self, FakeUsername) end),
    navidb_subs:watch(Listener),
    Resource = #{<<"resource">> => <<"account">>, <<"id">> => FakeUsername},
    Keys = [tokey(Resource)],
    navidb_subs:subscribe(Listener, Keys),
    timer:sleep(100),
    navidb:set(accounts, {username, FakeUsername}, #{date => 1}),
    ?assertMatch(#{date := 1}, wait_echo(<<"account">>, FakeUsername)),
    navidb:set(accounts, {username, FakeUsername}, #{date => 2}),
    ?assertMatch(#{date := 2}, wait_echo(<<"account">>, FakeUsername)),
    Listener ! stop,

    Res3 = navidb:get(accounts, {username, FakeUsername}, {filter, ['_id', 'password']}),
    ?assertMatch(#{
                    username := FakeUsername,
                    date     := 2,
                    premium  := 0,
                    groups   := []
                }, Res3),
    ok.

tokey(#{<<"resource">> := Resource, <<"id">> := Id}) ->
    <<Resource/binary, ":", Id/binary>>.

wait_echo(Resource, Id) ->
    receive
        {echo, Resource, Id, Doc} ->
            ?LOG("      Echo Doc      = ~p", [Doc]),
            % ?assertMatch(#{resource := <<"account">>, id := Id, data := #{date := 1}}, M),
            Doc
    after 10000 ->
        erlang:error(timeout)
    end.

% Фальшивый обработчик websocket-соединения.
% регистрируется
listener(From, Id) ->
    receive
        stop ->
            ok;
        {json, [{messages, [#{resource := Resource, id := Id, data := Data}]}]} ->
            From ! {echo, Resource, Id, Data},
            listener(From, Id)
        after 5000 ->
            ?LOG("      Listener timeoit", []),
            erlang:error(timeout)
    end.
