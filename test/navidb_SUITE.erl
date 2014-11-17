%%% Test suite for the navidb module.
-module(navidb_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [test1].

init_per_suite(Config) ->
    error_logger:tty(false),
    {ok, Modules} = application:ensure_all_started(navidb),
    [{modules, Modules} | Config].

end_per_suite(Config) ->
    Modules = ?config(modules, Config),
    [application:stop(Module) || Module <- lists:reverse(Modules)],
    application:unload(navidb),
    % meck:unload(),
    error_logger:tty(true),
    ok.


%% @doc Test1
%%
test1(_) ->
    % Check modules is load
    ?assertNotEqual(undefined, whereis(navidb_gpsdb)),
    ?assertNotEqual(undefined, global:whereis_name(navidb_subs)), % Зарегестрирован как глобальный модуль
    ?assertNotEqual(undefined, whereis(navidb_mongo_pool)), % Пул воркеров к базе данных
    ?assertEqual({ready,5,0,0}, poolboy:status(navidb_mongo_pool)),
    timer:sleep(200),  % Ensure indexes was created
    ok.
