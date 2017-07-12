%% -*- coding: utf-8 -*-
-module(navidb_mongodb).


-export([
        % child_spec/0,
        init_pool/0,
        insert/2,
        % update/3,
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

-define(POOL_NAME, navidb_mongo_api).

init_pool() ->
    % {Mongo, Db} = app_ctl:get_cfg(mongo),
    {ok, PoolConfig} = application:get_env(navidb, connect_pool),
    PoolSize         = proplists:get_value(size, PoolConfig),
    MaxOverflow      = proplists:get_value(max_overflow, PoolConfig),
    Server     = application:get_env(navidb, hostname, "localhost:27017"),
    % Port       = application:get_env(navidb, port, 27017),
    {ok, Database}   = application:get_env(navidb, database),
    Options = [
        {name, navidb_pool},
        {pool_size, PoolSize},
        {max_overflow, MaxOverflow},
        {register, ?POOL_NAME}
        ],
    WorkOpts = [{database, Database}],
    {ok, _Pid} = mongo_api:connect(single, Server, Options, WorkOpts),
    ok.

% child_spec() ->
%     {ok, PoolConfig} = application:get_env(navidb, connect_pool),
%     PoolSize         = proplists:get_value(size, PoolConfig),
%     MaxOverflow      = proplists:get_value(max_overflow, PoolConfig),
%     Server     = application:get_env(navidb, hostname, "localhost"),
%     Port       = application:get_env(navidb, port, 27017),
%     {ok, Database}   = application:get_env(navidb, database),
%
%     mongo_api:child_spec(?POOL_NAME, PoolSize, Server, Port, Database, MaxOverflow).

insert(Coll, Doc) ->
    % Ans = mongo_api:insert(?POOL_NAME, Coll, map_to_bson(Doc)),
    Ans = mongo_api:insert(?POOL_NAME, Coll, prepare_doc(Doc)),
    {{true, #{<<"n">> := 1}}, Answer } =
        Ans,
    repair_doc(Answer).

% update(Coll, Selector, Doc) ->
%     mongo_api:update(?POOL_NAME, Coll, map_to_bson(Selector), map_to_bson(Doc)).

update(Coll, Selector, Doc, Upsert) ->
    {true, #{<<"n">> := 1}} =
        mongo_api:update(?POOL_NAME, Coll, prepare_doc(Selector), prepare_doc(Doc), #{upsert => Upsert}),
    ok.

delete(Coll, Selector) ->
    mongo_api:delete(?POOL_NAME, Coll, prepare_doc(Selector)).

find_one(Coll, Selector) ->
    Ans = mongo_api:find_one(?POOL_NAME, Coll, prepare_doc(Selector), {}),
    % Ans.
    case Ans of
        undefined -> #{error => no_entry};
        #{} -> repair_doc(Ans)
    end.
    % case Ans of
    %     {} -> #{error => no_entry};
    %     {Res} -> bson_to_map(Res)
    % end.

aggregate(Coll, Pipeline) ->
    Cmd = {
        % aggregate, atom_to_binary(Coll, latin1),
        aggregate, Coll,
        % pipeline, map_to_bson(Pipeline)
        pipeline, Pipeline
    },

    % Original (not implemented yet)
    % Res = mongo_api:command(?POOL_NAME, Cmd),
    Res = mongo_api:find_one(?POOL_NAME, <<"$cmd">>, Cmd, {}),
    case Res of
        undefined -> #{error => no_entry};
        #{<<"ok">> := 1.0, <<"result">> := Result} ->
            repair_doc(Result)
    end.

ensure_index(Coll, IndexSpec) ->
    mongo_api:ensure_index(?POOL_NAME, Coll, IndexSpec).

% private

% Prepare doc for save in database
prepare_doc(Doc) when is_map(Doc) ->
    Fun = fun
        (K, V, Acc) ->
            maps:put(key_to_db(K), prepare_doc(V), Acc)
    end,
    maps:fold(Fun, #{}, Doc);

prepare_doc({K, V}) -> #{key_to_db(K) => prepare_doc(V)};

prepare_doc(Doc) -> Doc.


key_to_db(id) ->
    <<"_id">>;

key_to_db(<<"id">>) ->
    <<"_id">>;

key_to_db(Key) when is_atom(Key) ->
    key_to_db(atom_to_binary(Key, utf8));

% Nested requests, like: data.{key}.value
key_to_db(Keys) when is_list(Keys) ->
    <<$., Rest/binary>> = lists:foldl(
        fun
            % (Key, Acc) when is_atom(Key) ->
            %     <<Acc/binary, $., (atom_to_binary(Key, latin1))/binary>>;
            (Key, Acc) ->
                <<Acc/binary, $., (key_to_db(Key))/binary>>
        end,
        <<>>,
        Keys),
    Rest;

key_to_db(Key) when is_binary(Key) ->
    binary:replace(Key, <<$.>>, <<$#>>, [global]).



% Restore doc from database
repair_doc(Doc) when is_map(Doc) ->
    Fun = fun
        ('_id', {Value}, Acc) ->   % автоматически oid
            Acc#{id => base64:encode(Value)};
        (<<"_id">>, {Value}, Acc) ->   % автоматически oid
            Acc#{id => base64:encode(Value)};
        (<<"_id">>, Value, Acc) ->     % ручной id
            Acc#{id => Value};
        (K, V, Acc) ->
            maps:put(key_from_db(K), repair_doc(V), Acc)
    end,
    maps:fold(Fun, #{}, Doc);

repair_doc(Document) when is_list(Document) ->
    [repair_doc(Value) || Value <- Document];

repair_doc(Doc) -> Doc.

% % Преобразование bson:document() в значение, готовое к jsxn:encode

% bson_to_map({}) ->
%     #{};
%
% bson_to_map([]) ->
%     [];
%
% bson_to_map({bin, bin, Value}) ->
%     Value;
%
% bson_to_map(Document) when is_list(Document) ->
%     [bson_to_map(Value) || Value <- Document];
%
% bson_to_map(Document) when is_tuple(Document) ->
%     bson:doc_foldl(fun
%         ('_id', {Value}, Acc) ->   % автоматически oid
%             Acc#{id => base64:encode(Value)};
%         (<<"_id">>, {Value}, Acc) ->   % автоматически oid
%             Acc#{id => base64:encode(Value)};
%         (<<"_id">>, Value, Acc) ->     % ручной id
%             Acc#{id => Value};
%         (Label, Value, Acc) ->
%             % maps:put(id(Label), bson_to_map(Value), Acc)
%             maps:put(key_from_db(Label), bson_to_map(Value), Acc)
%         end,
%         #{},
%         Document
%     );
%
% bson_to_map(undefined) ->
%     null;
%
% bson_to_map(Value) ->
%     Value.

key_from_db(<<"_id">>) ->  % Подозреваю что это никогда не вызывается
    id;

key_from_db(Key) when is_atom(Key) ->
    key_from_db(atom_to_binary(Key, utf8));

key_from_db(Key) when is_binary(Key) ->
    binary:replace(Key, <<$#>>, <<$.>>, [global]).

% Преобразование значения, полученного из jsxn:decode в bson:document()
% map_to_bson([]) ->
%     [];
%
% % Для совместимости с прямым использованием bson в качестве Selector
% % TODO: Не хватает is_tuple, если полей больше одного.
%
% map_to_bson({bin, Value}) ->
%     {bin, bin, Value};
%
% map_to_bson({Key, Value}) ->
%     {key_to_db(Key), map_to_bson(Value)};
%
% map_to_bson({bin, bin, Value}) ->
%     {bin, bin, Value};
%
% % map_to_bson(Value) when is_tuple(Value) ->
% %     List = tuple_to_list(Value),
% %     Parts
% %     Value.
%
% map_to_bson(Value) when is_list(Value) ->
%     [map_to_bson(Each) || Each <- Value];
%
% map_to_bson(Value) when is_map(Value) ->
%     erlang:list_to_tuple(maps:fold(
%         fun (Key, Item, Acc) ->
%             % [tokey(Key)] ++ [map_to_bson(Item)] ++ Acc
%             [key_to_db(Key), map_to_bson(Item)] ++ Acc
%         end,
%         [],
%         Value
%     ));
%
% map_to_bson(Value) ->
%     Value.


% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

prepare_doc_test() ->
    ?assertEqual(#{}, prepare_doc(#{})),
    ?assertEqual(#{<<"_id">> => <<"bebebe">>}, prepare_doc(#{id => <<"bebebe">>})),
    ?assertEqual(#{<<"a#b">> => 0}, prepare_doc(#{'a.b' => 0})),
    ?assertEqual(#{<<"a#b">> => 0}, prepare_doc(#{<<"a.b">> => 0})),
    ?assertEqual(#{<<"data.a#b.value">> => 0}, prepare_doc(#{[data, <<"a.b">>, value] => 0})),

    done.

repair_doc_test() ->
    ?assertEqual(#{}, repair_doc(#{})),
    ?assertEqual([#{}], repair_doc([#{}])),
    ?assertEqual(#{<<"a">> => 0, id => <<"some_id">>}, repair_doc(#{<<"a">> => 0, <<"_id">> => <<"some_id">>})),
    ?assertEqual(#{<<"a">> => 0, id => base64:encode(<<"some_id">>)}, repair_doc(#{<<"a">> => 0, <<"_id">> => {<<"some_id">>}})),
    done.


% bson_to_map_test() ->
%     ?assertEqual(#{a => 0}, bson_to_map({a, 0})),
%     ?assertEqual(#{a => null}, bson_to_map({a, undefined})),
%     ?assertEqual(#{a => 0, b => 1}, bson_to_map({a, 0, b, 1})),
%     ?assertEqual([#{a => 0, b => 1}], bson_to_map([{a, 0, b, 1}])),
%     ?assertEqual(#{a => 0, b => #{c => 1}}, bson_to_map({a, 0, b, {c, 1}})),
%     ?assertEqual(#{a => 0, b => <<"binary">>}, bson_to_map({a, 0, b, {bin, bin, <<"binary">>}})),
%     ?assertEqual(#{a => 0, id => <<"some_id">>}, bson_to_map({a, 0, '_id', <<"some_id">>})),
%     ?assertEqual(#{a => 0, id => base64:encode(<<"some_id">>)}, bson_to_map({a, 0, '_id', {<<"some_id">>}})),
%     done.
%
% map_to_bson_test() ->
%     ?assertEqual({}, map_to_bson(#{})),
%     ?assertEqual([], map_to_bson([])),
%     ?assertEqual({<<"a">>, 0}, map_to_bson(#{a => 0})),
%     ?assertEqual({<<"a#b">>, 0}, map_to_bson(#{'a.b' => 0})),
%     ?assertEqual({<<"a#b">>, 0}, map_to_bson(#{<<"a.b">> => 0})),
%     ?assertEqual(
%         true,
%         case map_to_bson(#{a => 0, b => 1}) of
%             {<<"a">>, 0, <<"b">>, 1} -> true;
%             {<<"b">>, 1, <<"a">>, 0} -> true;
%             _            -> false
%         end
%     ),
%     ?assertEqual(
%         true,
%         case map_to_bson(#{a => 0, b => #{c => 1}}) of
%             {<<"a">>, 0, <<"b">>, {<<"c">>, 1}} -> true;
%             {<<"b">>, {<<"c">>, 1}, <<"a">>, 0} -> true;
%             _                 -> false
%         end
%     ),
%     ?assertEqual({<<"data.a#b.value">>, 0}, map_to_bson(#{[data, <<"a.b">>, value] => 0})),
%
%   done.

toket_test() ->
    % ?assertEqual(<<"test#me">>, tokey(<<"test.me">>)),
    done.

-endif.
