-module(accounts_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [get, system].

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

init_per_testcase(_Case, Config) ->
    #{username := Username} = Account = helper:fake_account(),
    % ct:log("Account=~p", [Account]),
    navidb:insert(accounts, Account),
    [{username, Username} | Config].

end_per_testcase(_Case, Config) ->
    Username = ?config(username, Config),
    navidb:remove(accounts, #{username => Username}),
    ok.

get(Config) ->
    Username = ?config(username, Config),
    Res2 = navidb:get(accounts, {username, Username}, {filter, [id, 'password']}),
    ct:log("Res2=~p", [Res2]),
    ct:log("Username=~p", [Username]),
    ?assertMatch(#{
                    <<"username">> := Username,
                    <<"groups">>   := [],
                    <<"skeys">>    := []
                }, Res2),
    ?assertException(error, {badmatch, _Reason}, #{id := _} = Res2),
    ?assertException(error, {badmatch, _Reason}, #{password := _} = Res2),
    ok.

system(Config) ->
    Username = ?config(username, Config),
    #{id := Skey} = helper:fake_system(),
    #{<<"skeys">> := SkeysBefore} = navidb:get(accounts, {username, Username}),
    ?assertEqual(false, lists:member(Skey, SkeysBefore)),
    ok = navidb:update(accounts, #{<<"username">> => Username}, {<<"$addToSet">>, {skeys, Skey}}),
    #{<<"skeys">> := SkeysAfter} = navidb:get(accounts, {username, Username}),
    ?assertEqual(true, lists:member(Skey, SkeysAfter)),
    ok.
