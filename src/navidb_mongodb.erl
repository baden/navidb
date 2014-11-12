%% -*- coding: utf-8 -*-
-module(navidb_mongodb).


-export([
        child_spec/0,
        insert/2,
        update/3,
        update/4,
        delete/2,
        find_one/2,
        aggregate/2,
        ensure_index/2
        % id/1,
        % tokey/1,
        % bson_to_map/1,
        % map_to_bson/1,
        % proplist_to_doc/1
        ]).

-define(POOL_NAME, navidb_mongo_pool).

child_spec() ->
    {ok, PoolConfig} = application:get_env(navidb, connect_pool),
    PoolSize         = proplists:get_value(size, PoolConfig),
    MaxOverflow      = proplists:get_value(max_overflow, PoolConfig),
    Server     = application:get_env(navidb, hostname, "localhost"),
    Port       = application:get_env(navidb, port, 27017),
    {ok, Database}   = application:get_env(navidb, database),

    mongo_pool:child_spec(?POOL_NAME, PoolSize, Server, Port, Database, MaxOverflow).

insert(Coll, Doc) ->
    bson_to_map(mongo_pool:insert(?POOL_NAME, Coll, map_to_bson(Doc))).

update(Coll, Selector, Doc) ->
    mongo_pool:update(?POOL_NAME, Coll, map_to_bson(Selector), map_to_bson(Doc)).

update(Coll, Selector, Doc, Upsert) ->
    mongo_pool:update(?POOL_NAME, Coll, map_to_bson(Selector), map_to_bson(Doc), Upsert).

delete(Coll, Selector) ->
    mongo_pool:delete(?POOL_NAME, Coll, map_to_bson(Selector)).

find_one(Coll, Selector) ->
    case mongo_pool:find_one(?POOL_NAME, Coll, map_to_bson(Selector)) of
        {} -> #{error => no_entry};
        {Res} -> bson_to_map(Res)
    end.

aggregate(Coll, Pipeline) ->
    Cmd = {
        aggregate, atom_to_binary(Coll, latin1),
        pipeline, map_to_bson(Pipeline)
    },

    % Original (not implemented yet)
    % Res = mongo_pool:command(?POOL_NAME, Cmd),
    {Res} = mongo_pool:find_one(?POOL_NAME, '$cmd', Cmd),
    _Ok = bson:at(ok, Res),  % 1.0 если выполнение успешно
    bson_to_map(bson:at(result, Res)).

ensure_index(Coll, IndexSpec) ->
    mongo_pool:ensure_index(?POOL_NAME, Coll, IndexSpec).

% % Преобразование bson:document() в значение, готовое к jsxn:encode

bson_to_map({}) ->
    #{};

bson_to_map([]) ->
    [];

bson_to_map({bin, bin, Value}) ->
    Value;

bson_to_map(Document) when is_list(Document) ->
    [bson_to_map(Value) || Value <- Document];

bson_to_map(Document) when is_tuple(Document) ->
    bson:doc_foldl(fun
        ('_id', {Value}, Acc) ->   % автоматически oid
            Acc#{id => base64:encode(Value)};
        ('_id', Value, Acc) ->     % ручной id
            Acc#{id => Value};
        (Label, Value, Acc) ->
            % maps:put(id(Label), bson_to_map(Value), Acc)
            maps:put(Label, bson_to_map(Value), Acc)
        end,
        #{},
        Document
    );

bson_to_map(undefined)->
    null;

bson_to_map(Value)->
    Value.

% Преобразование значения, полученного из jsxn:decode в bson:document()
map_to_bson([]) ->
    [];

% Для совместимости с прямым использованием bson в качестве Selector
% TODO: Не хватает is_tuple, если полей больше одного.
map_to_bson({Key, Value}) ->
    {Key, map_to_bson(Value)};

map_to_bson(Value) when is_list(Value) ->
    [map_to_bson(Each) || Each <- Value];

map_to_bson(Value) when is_map(Value) ->
    erlang:list_to_tuple(maps:fold(
        fun (Key, Item, Acc) ->
            % [tokey(Key)] ++ [map_to_bson(Item)] ++ Acc
            [Key, map_to_bson(Item)] ++ Acc
        end,
        [],
        Value
    ));

map_to_bson(Value) ->
    Value.

% id(Label) when is_binary(Label) ->
%     binary:replace(Label, <<$#>>, <<$.>>, [global]);
%
% id(Label) when is_atom(Label) ->
%     Label.
    % id(erlang:atom_to_binary(Label, utf8)).

% proplist_to_doc([{}]) -> {};
%
% proplist_to_doc(In) when is_list(In) ->
%     erlang:list_to_tuple(lists:flatten(lists:foldl(fun
%         ({Key, Value}, Acc) ->
%             [[tokey(Key), Value] | Acc]
%         end,
%         [],
%         In
%     ))).

% tokey(Label) when is_binary(Label) ->
%     binary:replace(Label, <<$.>>, <<$#>>, [global]);
%
% tokey(Label) when is_atom(Label) ->
%     tokey(erlang:atom_to_binary(Label, utf8)).


% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

bson_to_map_test() ->
    ?debugFmt("*********** Test bson_to_map", []),
    ?assertEqual(#{}, bson_to_map({})),
    ?assertEqual([], bson_to_map([])),
    ?assertEqual(#{a => 0}, bson_to_map({a, 0})),
    ?assertEqual(#{a => 0, b => 1}, bson_to_map({a, 0, b, 1})),
    ?assertEqual([#{a => 0, b => 1}], bson_to_map([{a, 0, b, 1}])),
    ?assertEqual(#{a => 0, b => #{c => 1}}, bson_to_map({a, 0, b, {c, 1}})),
    ?assertEqual(#{a => 0, b => <<"binary">>}, bson_to_map({a, 0, b, {bin, bin, <<"binary">>}})),
    ?assertEqual(#{a => 0, id => <<"some_id">>}, bson_to_map({a, 0, '_id', <<"some_id">>})),
    ?assertEqual(#{a => 0, id => base64:encode(<<"some_id">>)}, bson_to_map({a, 0, '_id', {<<"some_id">>}})),
    done.

map_to_bson_test() ->
    ?debugFmt("*********** Test map_to_bson", []),
    ?assertEqual({}, map_to_bson(#{})),
    ?assertEqual([], map_to_bson([])),
    ?assertEqual({a, 0}, map_to_bson(#{a => 0})),
    ?assertEqual(
        true,
        case map_to_bson(#{a => 0, b => 1}) of
            {a, 0, b, 1} -> true;
            {b, 1, a, 0} -> true;
            _            -> false
        end
    ),
    ?assertEqual(
        true,
        case map_to_bson(#{a => 0, b => #{c => 1}}) of
            {a, 0, b, {c, 1}} -> true;
            {b, {c, 1}, a, 0} -> true;
            _                 -> false
        end
    ),
    done.

toket_test() ->
    % ?assertEqual(<<"test#me">>, tokey(<<"test.me">>)),
    done.

-endif.
