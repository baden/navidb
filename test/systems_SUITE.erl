-module(systems_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [insert_get, update, dynamic, command, system_cached].

init_per_suite(Config) ->
    error_logger:tty(false),
    {ok, Modules} = application:ensure_all_started(navidb),
    % meck:new(navidb_mongodb, [non_strict]),
    [{modules, Modules} | Config].

end_per_suite(Config) ->
    % meck:unload(navidb_mongodb),
    Modules = ?config(modules, Config),
    [application:stop(Module) || Module <- lists:reverse(Modules)],
    application:unload(navidb),
    meck:unload(),
    error_logger:tty(true),
    ok.


insert_get(_) ->
    #{'_id' := Skey1} = System1 = helper:fake_system(),
    #{'_id' := Skey2} = System2 = helper:fake_system(),
    #{id := Skey1} = navidb:insert(systems, System1),
    #{id := Skey2} = navidb:insert(systems, System2),
    [Sys1, Sys2] = navidb:get(systems, [Skey1, Skey2]),
    ?assertMatch(#{id := Skey1}, Sys1),
    ?assertMatch(#{id := Skey2}, Sys2),

    % ct:pal(" Res2 = ~p", [Both]),
    All = navidb:get_all_systems(),
    ct:pal(" Res2 = ~p", [All]),
    ?assertMatch([#{error := <<"no_entry">>}], navidb:get(systems, [<<"lost_id">>])),
    navidb:remove(systems, #{'_id' => Skey1}),

    % FCatch = fun(POOL_NAME, Collection, Selector) ->
    %     ct:pal(" catch:: POOL_NAME = ~p, Collection = ~p Selector = ~p", [POOL_NAME, Collection, Selector])
    % end,
    % meck:expect(navidb_mongodb, delete, fun(_C, _S) -> ok end),
    % meck:expect(navidb_mongodb, delete, FCatch),
    % mongo_pool:delete(?POOL_NAME, Coll, map_to_bson(Selector))
    % meck:expect(mongo_pool, delete, FCatch),
    navidb:remove(systems, #{'_id' => Skey2}),
    ok.

update(_) ->
    #{'_id' := Skey} = System = helper:fake_system(),
    navidb:insert(systems, System),
    ?assertException(error, {badmatch, _}, #{foo := _} = navidb:get(systems, {'_id', Skey})),
    % navidb:update(systems, Skey, #{'$set' => #{foo => <<"bar">>}}),
    % navidb:update(systems, {'_id', Skey}, #{'$set' => #{foo => <<"bar">>}}),
    navidb:update(systems, #{'_id' => Skey}, #{'$set' => #{foo => <<"bar">>}}),
    ?assertMatch(#{foo := <<"bar">>}, navidb:get(systems, {'_id', Skey})),
    navidb:remove(systems, #{'_id' => Skey}),
    ok.

dynamic(_) ->
    % System = fake_system(<<"fake-01">>),
    System = helper:fake_system(),
    #{'_id' := Skey} = System,
    navidb:insert(systems, System),
    % No dynamic field before
    ?assertException(error, {badmatch, _}, #{dynamic := _} = navidb:get(systems, {'_id', Skey})),
    % TODO: need test broadcast
    navidb:set(dynamic, Skey, #{ foo => <<"bar">> }),
    % Must contain dynamic field now
    ?assertMatch(#{dynamic := _}, navidb:get(systems, {'_id', Skey})),
    navidb:remove(systems, #{'_id' => Skey}),
    ok.

command(_) ->
    Skey = <<"fake-key-01">>,
    Command = <<"CONFIGUP\r\n">>,
    navidb:set(command, Skey, Command),

    ?assertMatch({ok, Command}, navidb:get(command, Skey)),
    ?assertMatch({error, notfound}, navidb:get(command, <<"fake-key-02">>)),

    ok.

system_cached(_) ->
    #{'_id' := Skey, imei := Imei} = System = helper:fake_system(),
    % Read over cache
    ?assertMatch(#{imei := Imei}, navidb:get(system, Skey, cached)),
    % Read direct database
    ?assertMatch(#{imei := Imei}, navidb:get(systems, Skey)),
    % Remove from database
    navidb:remove(systems, #{'_id' => Skey}),
    % Read over cache. Must be steel present
    ?assertMatch(#{imei := Imei}, navidb:get(system, Skey, cached)),
    % Must not be acceseble on database
    ?assertMatch(#{error := no_entry}, navidb:get(systems, Skey)),
    % Write to DB fake Document
    navidb:insert(systems, #{'_id' => Skey, fake => <<"doc">>}),
    % Update will clean cache
    navidb:update(systems, #{'_id' => Skey}, #{'$set' => #{foo => <<"bar">>}}),
    % Prevent data in database
    % meck:expect(navidb_mongodb, find_one, 2, #{fake => <<"doc">>}),
    ?assertMatch(#{fake := <<"doc">>}, navidb:get(system, Skey, cached)),
    % find_one(Coll, Selector)
    % meck:unload(navidb_mongodb),
    navidb:remove(systems, #{'_id' => Skey}),
    ok.
