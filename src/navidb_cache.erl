%% -*- coding: utf-8 -*-
-module(navidb_cache).

-export([
    start/0, stop/0,
    put/3, get/2, delete/2, delete/3,
    get_cached/3
]).

-define(MEMCACHE_TABLE, navi_resource_cache).

start() ->
    ?MEMCACHE_TABLE = ets:new(?MEMCACHE_TABLE, [set, named_table, public, {write_concurrency, true}]),
    ok.

stop() ->
    true = ets:delete(?MEMCACHE_TABLE),
    ok.

put(Collection, Key, Document) ->
    Id = id(Collection, '_id', Key),
    put_(?MEMCACHE_TABLE, Id, Document).

get(Collection, Key) ->
    Id = id(Collection, '_id', Key),
    get_(?MEMCACHE_TABLE, Id).

delete(Collection, Key) ->
    delete(Collection, '_id', Key).

delete(Collection, Field, Key) ->
    Id = id(Collection, Field, Key),
    delete_(?MEMCACHE_TABLE, Id).

get_cached(Collection, {Field, Key}, Callback) ->
    Id = id(Collection, Field, Key),
    case get_(?MEMCACHE_TABLE, Id) of
        {ok, MemDocument} ->
            MemDocument;
        {error, notfound} ->
            case mongo_worker:find_one(Collection, {Field, Key}) of
                {error, _} ->
                    ct:pal("Key = ~p~n", [Key]),
                    NewDocument = Callback(),
                    ct:pal("NewDocument = ~p~n", [NewDocument]),
                    Res = mongo_worker:insert(Collection, NewDocument),
                    ct:pal("mongo_worker:insert Res = ~p~n", [Res]),
                    % CahceDocument = mongo_worker:bson_to_json(NewDocument),
                    put_(?MEMCACHE_TABLE, Id, NewDocument),
                    NewDocument;
                {ok, DbDocument} ->
                    put_(?MEMCACHE_TABLE, Id, DbDocument),
                    DbDocument
            end
    end;

get_cached(Name, Key, Callback) ->
    get_cached(Name, {id, Key}, Callback).



% Private

id(Collection, Field, Key) ->
    list_to_binary(io_lib:format("~p:~p:~p", [Collection, Field, Key])).

get_(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            {error, notfound};
        [{_Key, Value}] ->
            {ok, Value}
    end.

put_(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}),
    ok.

delete_(Table, Key) ->
    ets:delete(Table, Key).

% Соответствие коллекции имени ресурса
% name(<<"navicc_accounts">>) -> account;
% name(<<"navicc_groups">>) -> group;
% name(<<"navicc_systems">>) -> system;
% name(<<"navicc_params">>) -> param;
% name(<<"navicc_logs">>) -> log;
% name(<<"navicc_gps">>) -> gps;
% name(<<"dynamic">>) -> system_dynamic;
% name(<<"command">>) -> system_command.
% % name(_) -> unknown.   % Make error
