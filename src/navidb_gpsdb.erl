%% -*- coding: utf-8 -*-
-module(navidb_gpsdb).

-behaviour(gen_server).

% Info:
% Необходимо оценить необходимость использования пула.
% Но в пуле необходимо быть осторожными с возможным конфликтом.

% Зачем тут вообще gen_server????

-export([
    start_link/0,
    stop/0,
    save/3,
    flush/1,
    get/1,
    info/0
]).


%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    membase
}).

-record(memrecord, {
    skey,   % Ключ
    hour,   % Час
    data    % Пакеты
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


stop() ->
    gen_server:call(?SERVER, {stop}).

% Сохранение пакета (или нескольких) для часа Hour
save(Skey, Hour, Data) ->
    % Проверим что у нас есть в mnesia по данной системе
    case mnesia:dirty_read(memrecord, Skey) of
        [] ->   % Такой записи еще нет. Для этой системы вообще нет зыписей? Новая система.
            Record = #memrecord{
                skey            = Skey,
                hour            = Hour,
                data            = Data
            },
            mnesia:dirty_write(Record);
        [Record] -> % Запись уже есть, проверим, если часы в записе такие-же как у нас, то добавим значение
            % ?INFO("Record = ~p", [Record]),
            case Record#memrecord.hour of
                Hour ->     % Да, запись в тот-же час, просто допишем данные
                    mnesia:dirty_write(Record#memrecord{ data = <<(Record#memrecord.data)/binary, Data/binary>>});

                OldHour ->  % Запись в другой час
                    % Сначала сохраним данные из прошлого часа
                    navidb_mongodb:update(
                        <<"navicc_gps">>, %?DB_GPS,
                        #{<<"system">> => Skey, <<"hour">> => OldHour},
                        #{<<"$push">> => #{<<"data">> => {bin, bin, Record#memrecord.data}}},
                        true
                    ),

                    % И сохраним данные нового часа
                    NewRecord = #memrecord{
                        skey            = Skey,
                        hour            = Hour,
                        data            = Data
                    },
                    mnesia:dirty_write(NewRecord)
            end
    end,

    ok.

% Делайте это при остановке сервера!!!
% Можно это делать с определенным периодом (скажем раз в сутки)
flush(all) ->
    Keys = mnesia:dirty_all_keys(memrecord),
    [flush(Key) || Key <- Keys],
    tbd;

% Сохранение буффера из памяти в базу данных
flush(Skey) ->
    % Проверим что у нас есть в mnesia по данной системе
    case mnesia:dirty_read(memrecord, Skey) of
        [] ->   % Такой записи еще нет. Для этой системы вообще нет зыписей? Сохранение не требуется
            ok;
        [Record] -> % Да, в памяти есть запись уже есть.
            navidb_mongodb:update(
                <<"navicc_gps">>, %?DB_GPS,
                #{<<"system">> => Skey, <<"hour">> => Record#memrecord.hour},
                #{<<"$push">> => #{<<"data">> => {bin, bin, Record#memrecord.data}}},
                true
            ),

            % Удалим запись из памяти
            mnesia:dirty_delete(memrecord, Skey)
    end,

    ok.

% Получение данных из mnesia
get(Skey) ->
    case mnesia:dirty_read(memrecord, Skey) of
        [] -> % Данных нет (система ни разу ничего не слала)
            nodata;
        [Record] ->
            {ok, Record#memrecord.hour, Record#memrecord.data}
    end.

-include_lib("stdlib/include/qlc.hrl").
%% @doc Insert document
%
-spec info() -> map().
info() ->
    % List = mnesia:dirty_all_keys(memrecord),
    % Data = mnesia:dirty_match_object(memrecord, #memrecord{_ = '_'}),
    Q = qlc:q([{X#memrecord.skey, #{hour => X#memrecord.hour, data_length => size(X#memrecord.data)}} || X <- mnesia:table(memrecord)]),
    maps:from_list(do(Q)).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    case mnesia:transaction (F) of
        {atomic, Val} ->
            Val;
        {aborted, _Reason} ->
            []
    end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

% init([Props]) ->
init([]) ->
    MemBase = initmembase(),

    {ok, #state{
        membase = MemBase
    }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

% terminate(_Reason, #state{db_pool = DbPool}) ->
terminate(_Reason, _State) ->
    % resource_pool:close(DbPool),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Internal
%% ===================================================================

initmembase() ->
    % TODO: Установка каталога сохранения через
    % application:set_env(mnesia, dir, Dir).
    Node = node(),

    mnesia:stop(),
    case mnesia:create_schema([Node]) of
        ok ->
            ok;
        {error, {Node, {already_exists, Node}}} ->
            ok
    end,
    ok = mnesia:start(),
    mnesia:create_table(memrecord, [
            {disc_copies, [Node]},
            {type, set},
            {attributes, record_info(fields, memrecord)}
    ]),
    ok.
