-module(systems_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [run].

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


run(_) ->
    ct:pal("Test systems"),
    System1 = fake_system(<<"fake-01">>),
    System2 = fake_system(<<"fake-02">>),
    Res1 = navidb:insert(systems, System1),
    Res2 = navidb:insert(systems, System2),
    ct:pal(" Res1 = ~p", [Res1]),
    ct:pal(" Res2 = ~p", [Res2]),
    All = navidb:get_all_systems(),
    ct:pal(" Res2 = ~p", [All]),
    ok.

fake_system(Imei) when is_binary(Imei) ->
    Skey = base64:encode(Imei),
    SImei = binary_to_list(Imei),
    [ImeiOnly | _LastImei] = string:tokens(SImei, "-"),     % Выделим только IMEI, без кода
    LastImei = list_to_binary(string:right(ImeiOnly, 6)),   % Возьмом последние 6 знаков
    #{
        '_id'   => Skey,
        imei    => Imei,                                % IMEI
        date    => unixtime(),                          % Дата/время первого выхода на связь
        phone   => <<>>,                                % Номер SIM-карты
        premium => unixtime() + 60*60*24*31,            % 1 месяц премиум-подписки
        title   => <<"Трекер "/utf8, LastImei/binary>>, % Отображаемое наименование транспортного средства
        icon    => <<"caricon-truck">>,                 % Значек
        car     => {},                                  % Запись о транспортном средстве
        tags    => [],                                  % Ярлыки
        groups  => [],                                  % Принадлежность к группам
        lock    => false,                               % Если установлен в true, то данный трекер запрещено добавлять в список наблюдения
        public  => true,                                % Если установлен в true, то трекер доступен для автоматического добавления членам группы
        params  => #{
            fuel => [
                #{voltage =>  0.0, liters => 0.0},
                #{voltage => 10.0, liters => 100.0}
            ],
            % notranslate, false              % Если установлен в true, то трансляция данных на старый сервер не требуется
            notranslate => true              % Если установлен в true, то трансляция данных на старый сервер не требуется
        }
    }.

unixtime() -> timer:now_diff(now(), {0,0,0}) div 1000000.
