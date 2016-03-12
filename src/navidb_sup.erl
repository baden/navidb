
-module(navidb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type),       {I, {I, start_link, []},   permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%-include("deps/mongodb/include/mongo_protocol.hrl").

init([]) ->
    ok = navidb_cache:start(),

    % ok = mongo_worker:connect(),

    % Worker      = ?CHILD(navidb_worker, worker),
    % Cache       = ?CHILD(cache, worker),
    GPSDB      = ?CHILD(navidb_gpsdb, worker),
    Subscriber = ?CHILD(navidb_subs, worker),

    % Pool-воркеры для подключения к mongoDB
    % Пока не удаляю, скорее всего тут будет пулл для mc_worker
    % https://github.com/comtihon/mongodb-erlang
    % {ok, Pools} = application:get_env(navidb, pools),

    % PoolSpecs = lists:map(fun({Name, Args, WorkerArgs}) ->
    %     PoolArgs = [{name, {local, Name}}] ++ Args,
    %     poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    % end, Pools),
    % MondoDB     = ?CHILD(navidb_mongodb, worker),

    % MongoPool = mongo_worker:child_spec(),

    % PoolArgs = [{name, {local, mc_worker}}, {worker_module, mc_worker}, {size, 5}, {max_overflow, 10}],
    % WorkerArgs = [{"localhost", 27017, #conn_state{database = <<"erlnavicc">>}}, []],
    % MongoPool = poolboy:child_spec(navidb_mongo_pool, PoolArgs, WorkerArgs),

    % PoolArgs = [
    %     {name, {local, pool1}},
    %     {worker_module, navidb_worker},
    %     {size, 5}, {max_overflow, 10}
    % ],
    % WorkerArgs = [{"localhost", 27017, <<"erlnavicc">>}, []],
    % MongoPool = poolboy:child_spec(pool1, PoolArgs, WorkerArgs),

    %% our mongo pool
    %% get configs
    % {ok, [{PoolName, SizeArgs, WorkerArgs}]} = application:get_env(campus, pools),
    PoolName = navidb_mongo_pool,
    Worker = mc_worker,
    PoolArgs = [
        {name, {local, PoolName}},
        {worker_module, Worker},
        {size, 10},
        {max_overflow, 30}
    ],

    Server     = application:get_env(navidb, hostname, "localhost"),
    Port       = application:get_env(navidb, port, 27017),
    {ok, Database}   = application:get_env(navidb, database),

    WorkerArgs = [ % [{"localhost", 27017, <<"erlnavicc">>}, []],
        {host, Server},
        {port, Port},
        {database, Database},
        {w_mode, safe}
    ],
    PoolSpecs = poolboy:child_spec(PoolName, PoolArgs, WorkerArgs),

    Mongo = ?CHILD(mongo_worker, worker, [PoolName]),

    % {ok, { {one_for_one, 5, 10}, [MongoDBPool, Worker, GPSDB, Websocket]} }.
    % {ok, { {one_for_one, 5, 10}, [Worker, GPSDB, Websocket, MongoPool]} }.
    % {ok, { {one_for_one, 5, 10}, [GPSDB, Subscriber, MongoPool]} }.
    {ok, { {one_for_one, 5, 10}, [GPSDB, Subscriber, Mongo, PoolSpecs]} }.
    % {ok, { {one_for_one, 5, 10}, [MondoDB, Worker, GPSDB, Websocket] ++ PoolSpecs} }.
