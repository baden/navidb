%%% @author Denis Batrak <baden.i.ua@gmail.com>
%%%  [http://batrak.net/]
%%% @doc NaviDB, as a module, provides access to the high-level functionality
%%% contained in the NaviCC application.
%%%
%%% It has functions in two main categories:
%%% <dl>
%%%     <dt>1. Database</dt>
%%%     <dd>{@link insert/2} and {@link get/2} Работа с базой данных.
%%%     </dd>
%%%     <dt>2. Subscribe</dt>
%%%     <dd>Подписка на изменение ресурсов посредством
%%%         {@link set/3} and {@link update/3}.
%%%     </dd>
%%% </dl>
%%% @end
%%% @todo Testing TODO notes.
-module(navidb).

-export([
         insert/2, get/2, get/3, update/3, set/3, remove/2, remove/3, delete/2,
         get_gps_hours/3, get_logs/3, get_geos/3, get_all_systems/0
        %  item/2, filter/2, to_proplist/1, to_map/1
        ]).

% API



%% @doc Insert document
%
-spec insert(Collection :: atom(), Document :: document()) -> document().
insert(Collection, Document) ->
    navidb_mongodb:insert(collection_name(Collection), Document).

% -type collection() :: atom.
-type collection() :: accounts | groups | systems | params | logs | gps | 'system' | 'command' | 'dynamic'.
-type document() :: map().
-type selector() :: binary() | tuple() | list(binary()).

% TODO: Добавить опциональные ключи для запросов через кеш
% get(Collection, Selector, Options) ->
-spec get(collection(), selector()) -> [document()].
get(Collection, Keys) when is_list(Keys) ->
    % TODO: Заменить на один запрос для списка.

    lists:reverse(lists:foldl(
        fun(Key, Acc) ->
            Record = case navidb_mongodb:find_one(collection_name(Collection), #{id => Key}) of
                #{error := no_entry} ->    % Записи о системе еще нет.
                    #{
                        id    => Key,
                        error => <<"no_entry">>
                    };
                Document ->
                    prepare_doc(Collection, Document)
                end,
            [Record | Acc]
        end,
        [],
        Keys
    ));

get(dynamic, Skey) ->
    navidb_cache:get(dynamic, Skey);

get(command, Skey) ->
    navidb_cache:get(command, Skey);

% Protect from use map for Selector
% get(_Collection, Selector) when is_map(Selector) ->
%     erlang:error(badarg);
get(Collection, Key) when is_binary(Key) ->
    get(Collection, {id, Key});

get(Collection, Selector) when is_tuple(Selector); is_map(Selector) ->
    RawDoc = navidb_mongodb:find_one(collection_name(Collection), Selector),
    prepare_doc(Collection, RawDoc).

% По идее это тоже нужно перенести в navidb
% Присобачить к записи системы виртуальное поле dynamic
% prepare_doc(systems, Document = #{<<"id">> := Skey}) ->
prepare_doc(systems, Document = #{id := Skey}) ->
    case navidb:get(dynamic, Skey) of
        {ok, Dynamic} ->
            % [{dynamic, Dynamic}] ++ Document;
            Document#{ dynamic => Dynamic};
        _ -> Document
    end;

prepare_doc(params, Document = #{data := Data}) ->
    % Я лоханулся. Требуется отфильтровать двойные кавычки
    % Это бып сделать на этапе парсинга в navipoint_config
    Filtered = maps:fold(
        fun(Name, #{type := Type, value := Value, default := Default}, Acc) ->
            maps:put(
                % type_to_repr(Name),
                Name,
                #{
                    type    => Type,
                    value   => remquotes(Value),
                    default => remquotes(Default)
                },
                Acc
            )
        end,
        #{},
        Data
    ),
    Document#{data := Filtered};

prepare_doc(_Collection, Document) ->
    Document.

remquotes(In) ->
    binary:replace(In, <<"\"">>, <<"">>, [global]).

% type_to_repr(Label) ->
%     binary:replace(atom_to_binary(Label, utf8), <<$#>>, <<$.>>, [global]).

% Получим запись о трекере и если таковой нет, то создадим ее.

% Проверим есть ли трекер в базе данных.
% Считывать из базы будем один раз и создавать в оперативной памяти запись что такая
% система существует.
% Если не существует, то создадим запись и сохраним в оперативной памяти.

% OPTIONS скорее всего информация о системе не требуется.
% -spec get(collection(), selector(), {filter, _} | cached) -> [document()].
% -spec get('system' | 'command' | 'dynamic', binary() | [1..255], 'cached') -> any().
-spec get(collection(), binary() | [1..255], 'cached') -> any().
get(Collection, Selector, {filter, Fields}) ->
    maps:without(Fields, get(Collection, Selector));

get(system, Skey, cached) ->
    Imei = base64:decode(Skey),
    navidb_cache:get_cached(
        collection_name(systems),
        Skey,
        fun() ->
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
            }
        end
    ).

-spec update(collection(), selector(), {filter, _} | cached) -> [document()].
update(Collection, Selector = {Field, Key}, Document) ->
    Res = navidb_mongodb:update(collection_name(Collection), Selector, Document, true),
    navidb_subs:broadcast(name(Collection), Key, null),
    navidb_cache:delete(collection_name(Collection), Field, Key),
    Res;

% Important! Ignore other fields!
update(Collection, #{id := Key}, Document) ->
    update(Collection, {id, Key}, Document);

update(Collection, Selector, Document) when is_map(Selector)->
    Fields = maps:keys(Selector),
    case length(Fields) of
        1 ->
            update(Collection, {hd(Fields), maps:get(hd(Fields), Selector)}, Document);
        _ ->
            navidb_mongodb:update(collection_name(Collection), Selector, Document, true)
            % erlang:error(badarg)
    end;

update(Collection, Key, Document) ->
    update(Collection, {id, Key}, Document).

% -spec set(collection(), selector(), document()) -> any().
-spec set(collection(), selector(), document()) -> any().
set(Collection, {Field, Key}, Document) ->
    Res = navidb_mongodb:update(collection_name(Collection), {Field, Key}, #{'$set' => Document}, true),
    navidb_subs:broadcast(name(Collection), Key, Document),
    navidb_cache:delete(Collection, Field, Key),
    Res;

set(dynamic, Skey, Dynamic) ->
    navidb_cache:put(dynamic, Skey, Dynamic),
    navidb_subs:broadcast(system, Skey, [{dynamic, Dynamic}]);

set(command, Skey, Data) ->
    navidb_cache:put(command, Skey, Data);

set(Collection, Key, Document) ->
    set(Collection, {id, Key}, Document).

-spec remove(collection(), selector(), {flush, {gps, binary()}}) -> any().
remove(Collection, Selector, {flush, {gps, Skey}}) ->
    navidb_gpsdb:flush(Skey),
    remove(Collection, Selector).

% TODO: Обратить внимание на кеш
-spec remove(collection(), selector()) -> any().
remove(Collection, Selector) ->
    navidb_mongodb:delete(collection_name(Collection), Selector).

-spec delete(collection(), binary()) -> any().
delete(command, Skey) ->
    navidb_cache:delete(command, Skey).

% В перспективе переделать документы на map, и тогда не понадобятся хелперы
% item(Key, Document) ->
%     bson:at(Key, Document).

% filter(Items, Document) ->
%     bson:exclude(Items, Document).

% TODO: разворачивает только верхний уровень!
% to_proplist(Document) ->
%     bson:fields(Document).

% TODO: разворачивает только верхний уровень!
% to_map(Document) ->
%     maps:from_list(bson:fields(Document)).

% TODO: Не самое элегантное решение. Сделано пока абыкак.
-spec get_gps_hours(binary(), non_neg_integer(), non_neg_integer()) -> document().
get_gps_hours(Skey, From, To) ->
    % !!! Этo не может быть map, так как важен порядок сделования полей (недоработка mongoDB)
    Pipeline = [
        {'$match', {
            system, Skey,
            hour, {
                '$gte', From,
                '$lte', To
            }
        }},
        {'$group', {
            '_id', 0, hours, {
                '$addToSet', <<"$hour">>
            }
        }}
    ],
    Hours = case navidb_mongodb:aggregate(collection_name(gps), Pipeline) of
        [] ->
            [];
        [Doc] ->
            % TODO: maps:get(hours) ?
            % bson:at(hours, Doc)
            maps:get(hours, Doc)
    end,

    % Добавим час, который в mnesia
    MemHours = case navidb_gpsdb:get(Skey) of
        {ok, Hour, _} when Hour >= From, Hour =< To ->
            [Hour];
        _ ->
            []
    end,
    Hours ++ MemHours.

% TODO: Не самое элегантное решение. Сделано пока абыкак.
-spec get_logs(binary(), non_neg_integer(), non_neg_integer()) -> document().
get_logs(Skey, Count, Skip) ->
    Pipeline = [
        {'$match', {system, Skey, dt, {'$lt', Skip}}},
        {'$sort', {dt, -1}},
        {'$limit', Count}
    ],
    navidb_mongodb:aggregate(collection_name(logs), Pipeline).

-spec get_geos(binary(), non_neg_integer(), non_neg_integer()) -> document().
get_geos(Skey, From, To) ->
    Pipeline = [
        {'$match', {
            system, Skey,
            hour, {'$gte', From, '$lte', To}
        }},
        {'$sort', {hour, 1}},
        {'$group', {'_id', 0, data, {'$push', <<"$data">>}}}
    ],

    Flat = case navidb_mongodb:aggregate(collection_name(gps), Pipeline) of
        [] ->
            <<"">>;
        [#{data := RawData}] ->
            list_to_binary(RawData)
    end,

    case navidb_gpsdb:get(Skey) of
        {ok, Hour, MemData} when Hour >= From, Hour =< To ->
            {ok, <<Flat/binary, MemData/binary>>};
        _ ->
            {ok, Flat}
    end.

-spec get_all_systems() -> document().
get_all_systems() ->
    Pipeline = [
        {'$project', {
            '_id', 1, imei, 1, date, 1, hwid, 1, swid, 1
        }},
        {'$sort', {date, 1}}
    ],
    navidb_mongodb:aggregate(collection_name(systems), Pipeline).
    % case navidb_mongodb:aggregate(collection_name(systems), Pipeline) of
    %     [] ->
    %         [];
    %     Doc ->
    %         % navidb_mongodb:bson_to_json(Doc)
    %         Doc
    % end.


% Private

collection_name(accounts)   -> navicc_accounts;
collection_name(groups)     -> navicc_groups;
collection_name(systems)    -> navicc_systems;
collection_name(params)     -> navicc_params;
collection_name(logs)       -> navicc_logs;
collection_name(gps)        -> navicc_gps.

% Соответствие коллекции имени ресурса в подписчике
name(accounts) -> account;
name(groups) -> group;
name(systems) -> system;
name(params) -> param;
name(logs) -> log;
name(gps) -> gps.
% name(_) -> unknown.

unixtime() ->
        {A, B, _} = os:timestamp(),
        (A * 1000000) + B.
