-module(systems_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [insert_get, update, dynamic, command, system_cached, logs, config].

init_per_suite(Config) ->
    error_logger:tty(false),
    {ok, Modules} = application:ensure_all_started(navidb),
    [{modules, Modules} | Config].

end_per_suite(Config) ->
    Modules = ?config(modules, Config),
    [application:stop(Module) || Module <- lists:reverse(Modules)],
    application:unload(navidb),
    error_logger:tty(true),
    ok.


insert_get(_) ->
    #{<<"id">> := Skey1} = System1 = helper:fake_system(),
    #{<<"id">> := Skey2} = System2 = helper:fake_system(),
    #{<<"id">> := Skey1} = navidb:insert(systems, System1),
    #{<<"id">> := Skey2} = navidb:insert(systems, System2),

    [Sys1, Sys2] = navidb:get(systems, [Skey1, Skey2]),

    ?assertMatch(#{<<"id">> := Skey1}, Sys1),
    ?assertMatch(#{<<"id">> := Skey2}, Sys2),

    % All = navidb:get_all_systems(),

    ?assertMatch([#{error := <<"no_entry">>}], navidb:get(systems, [<<"lost_id">>])),
    navidb:remove(systems, #{<<"id">> => Skey1}),

    navidb:remove(systems, #{<<"id">> => Skey2}),
    ok.

update(_) ->
    #{<<"id">> := Skey} = System = helper:fake_system(),
    navidb:insert(systems, System),
    ?assertException(error, {badmatch, _}, #{<<"foo">> := _} = navidb:get(systems, {id, Skey})),
    % navidb:update(systems, Skey, #{'$set' => #{foo => <<"bar">>}}),
    % navidb:update(systems, {id, Skey}, #{'$set' => #{foo => <<"bar">>}}),
    navidb:update(systems, #{<<"id">> => Skey}, #{<<"$set">> => #{<<"foo">> => <<"bar">>}}),
    ?assertMatch(#{<<"foo">> := <<"bar">>}, navidb:get(systems, {id, Skey})),

    Record = #{<<"value">> => 10, <<"dt">> => 0},
    navidb:update(systems, Skey, #{<<"$set">> => #{<<"balance">> => Record}}),
    ?assertMatch(#{<<"balance">> := Record}, navidb:get(systems, {id, Skey})),

    navidb:remove(systems, #{id => Skey}),
    ok.

dynamic(_) ->
    % System = fake_system(<<"fake-01">>),
    System = helper:fake_system(),
    #{<<"id">> := Skey} = System,
    navidb:insert(systems, System),
    % No dynamic field before
    ?assertException(error, {badmatch, _}, #{<<"dynamic">> := _} = navidb:get(systems, {id, Skey})),
    % TODO: need test broadcast
    navidb:set(dynamic, Skey, #{ <<"foo">> => <<"bar">> }),
    % Must contain dynamic field now
    ?assertMatch(#{<<"dynamic">> := _}, navidb:get(systems, {id, Skey})),
    navidb:remove(systems, #{<<"id">> => Skey}),
    ok.

command(_) ->
    Skey = <<"fake-key-01">>,
    Command = <<"CONFIGUP\r\n">>,
    navidb:set(command, Skey, Command),

    ?assertMatch({ok, Command}, navidb:get(command, Skey)),
    ?assertMatch({error, notfound}, navidb:get(command, <<"fake-key-02">>)),

    navidb:delete(command, Skey),
    ?assertMatch({error, notfound}, navidb:get(command, <<"fake-key-01">>)),

    ok.

system_cached(_) ->
    Skey1 = <<"eDA2MFM5YmNVUitCQS9reGxBbW1YQT09">>,
    CachedSys = navidb:get(system, Skey1, cached),
    ct:pal("CachedSys = ~p~n", [CachedSys]),
    Doc = navidb:get(systems, Skey1),
    ct:pal("Doc = ~p~n", [Doc]),
    ?assertMatch(#{<<"id">> := Skey1}, Doc),

    #{<<"id">> := Skey, <<"imei">> := Imei} = _System = helper:fake_system(),
    % Read over cache
    ?assertMatch(#{<<"imei">> := Imei}, navidb:get(system, Skey, cached)),
    % Read direct database
    ?assertMatch(#{<<"imei">> := Imei}, navidb:get(systems, Skey)),
    % Remove from database
    navidb:remove(systems, #{id => Skey}),
    % Read over cache. Must be steel present
    ?assertMatch(#{<<"imei">> := Imei}, navidb:get(system, Skey, cached)),
    % Must not be acceseble on database
    ?assertMatch(#{error := no_entry}, navidb:get(systems, Skey)),
    % Write to DB fake Document
    navidb:insert(systems, #{<<"id">> => Skey, <<"fake">> => <<"doc">>}),
    % Update will clean cache
    navidb:update(systems, #{<<"id">> => Skey}, #{<<"$set">> => #{<<"foo">> => <<"bar">>}}),
    % Prevent data in database
    % meck:expect(navidb_mongodb, find_one, 2, #{fake => <<"doc">>}),
    ?assertMatch(#{<<"fake">> := <<"doc">>}, navidb:get(system, Skey, cached)),
    % find_one(Coll, Selector)
    % meck:unload(navidb_mongodb),
    navidb:remove(systems, #{id => Skey}),
    ok.

logs(_) ->
    #{<<"id">> := Skey} = helper:fake_system(),
    Text = <<"Log text">>,
    Document = #{
        <<"system">> => Skey,
        <<"dt">>     => helper:unixtime(),
        <<"text">>   => Text
    },
    navidb:insert(logs, Document),
    Skip  = 100000000000,
    Count = 20,
    [Doc] = navidb:get_logs(Skey, Count, Skip),
    ?assertMatch(#{<<"system">> := Skey, <<"text">> := Text}, Doc),
    ok.

config(_) ->
    #{<<"id">> := Skey} = helper:fake_system(),
    Parced = #{
		<<"gsm#server">> => #{
			<<"value">>   => <<"point.new.navi.cc">>,
			<<"type">>    => <<"STR32">>,
			<<"default">> => <<"map.navi.cc">>
		},
		<<"gps#V0#3">> => #{
			<<"value">>   => <<"20">>,
			<<"type">>    => <<"INT">>,
			<<"default">> => <<"20">>
		},
		<<"akkum#U#3">> => #{
			<<"value">>   => <<"984">>,
			<<"type">>    => <<"INT">>,
			<<"default">> => <<"984">>
		}
	},
    navidb:set(params, Skey, #{<<"data">> => Parced}),

    #{<<"data">> := Data} = navidb:get(params, Skey),
    ?assertMatch(
        #{
            <<"akkum#U#3">> := #{
                <<"default">> := <<"984">>,
                <<"type">>    := <<"INT">>,
                <<"value">>   := <<"984">>
            },
            <<"gps#V0#3">> := #{
                <<"default">> := <<"20">>,
                <<"type">>    := <<"INT">>,
                <<"value">>   := <<"20">>
            },
            <<"gsm#server">> := #{
                <<"default">> := <<"map.navi.cc">>,
                <<"type">>    := <<"STR32">>,
                <<"value">>   := <<"point.new.navi.cc">>
            }
        },
        Data
    ),

    #{<<"id">> := Skey2} = helper:fake_system(),
    #{error := no_entry} = navidb:get(params, {id, Skey2}),
    ok.
