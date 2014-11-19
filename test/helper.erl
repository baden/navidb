-module(helper).

-export([
        fake_account/0,
        fake_account/1,
        fake_system/0,
        fake_system/1,
        random_string/0,
        unixtime/0
]).

fake_account() ->
    fake_account(random_string()).

fake_account(Username) when is_binary(Username) ->
    Salt = random:uniform(trunc(math:pow(2,64))),
    Email = <<(random_string())/binary, "@", (random_string())/binary>>,
    #{
        id       => base64:encode(<<Username/binary, $:, Salt:64>>),
        username => Username,
        password => random_string(),
        title    => Username,
        email    => Email,
        date     => unixtime(),
        skeys    => [],
        groups   => []
    }.

fake_system() ->
    fake_system(random_string()).

fake_system(Imei) when is_binary(Imei) ->
    Skey = base64:encode(Imei),
    SImei = binary_to_list(Imei),
    [ImeiOnly | _LastImei] = string:tokens(SImei, "-"),     % Выделим только IMEI, без кода
    LastImei = list_to_binary(string:right(ImeiOnly, 6)),   % Возьмом последние 6 знаков
    #{
        id      => Skey,
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

random_string() ->
    base64:encode(crypto:rand_bytes(16)).

unixtime() ->
        {A, B, _} = os:timestamp(),
        (A * 1000000) + B.
