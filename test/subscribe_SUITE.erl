-module(subscribe_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [account].

init_per_suite(Config) ->
    io:format("init_per_suite(~p)", [Config]),
    % error_logger:tty(false),
    {ok, _Modules} = application:ensure_all_started(navidb),
    Config.

end_per_suite(_Config) ->
    % Modules = ?config(modules, Config),
    % [application:stop(Module) || Module <- lists:reverse(Modules)],
    % application:unload(navidb),
    error_logger:tty(true),
    ok.

account(_) ->
    #{username := Username} = Account = helper:fake_account(),
    navidb:insert(accounts, Account),
    % groups
    Self = self(),
    Listener = spawn(fun() -> listener(Self, Username) end),
    navidb_subs:watch(Listener),
    ?assertEqual({ok, [Listener]}, navidb_subs:pids()),
    Resource = #{<<"resource">> => <<"account">>, <<"id">> => Username},
    Keys = [tokey(Resource)],
    navidb_subs:subscribe(Listener, Keys),
    timer:sleep(100),
    navidb:set(accounts, {username, Username}, #{date => 1}),
    ?assertMatch(#{date := 1}, wait_echo(<<"account">>, Username)),
    navidb:set(accounts, {username, Username}, #{date => 2}),
    ?assertMatch(#{date := 2}, wait_echo(<<"account">>, Username)),
    Listener ! stop,

    Res3 = navidb:get(accounts, {username, Username}, {filter, ['_id', 'password']}),
    ?assertMatch(#{
                    username := Username,
                    date     := 2,
                    groups   := []
                }, Res3),
    navidb:remove(accounts, #{username => Username}),
    ok.

tokey(#{<<"resource">> := Resource, <<"id">> := Id}) ->
    <<Resource/binary, ":", Id/binary>>.

wait_echo(Resource, Id) ->
    receive
        {echo, Resource, Id, Doc} ->
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
            erlang:error(timeout)
    end.
