-module(navidb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, start_phase/3, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    navidb_sup:start_link().

start_phase(init, _Type, _Args) ->
    % mongo_pool:start_link(navidb_mongo_pool, 5, <<"localhost">>, 27017, <<"erlnavicc">>, 10),
    ok = createindexes(),
    ok.

stop(_State) ->
    ok.

createindexes() ->
    {ok, ok} = mongo_worker:ensure_index(navicc_groups, #{<<"key">> => #{<<"groupname">> => 1}, <<"unique">> => true, <<"dropDups">> => true}),
    {ok, ok} = mongo_worker:ensure_index(navicc_accounts, #{<<"key">> => #{<<"username">> => 1}, <<"unique">> => true, <<"dropDups">> => true}),
    {ok, ok} = mongo_worker:ensure_index(navicc_logs, #{<<"key">> => #{<<"system">> => 1, <<"dt">> => 1}}),
    {ok, ok} = mongo_worker:ensure_index(navicc_balance, #{<<"key">> => #{<<"system">> => 1, <<"dt">> => 1}}),
    % {ok, ok} = mongo_worker:ensure_index(?SYSTEMS, #{<<"key">> => #{<<"imei">> => 1}, <<"unique">> => true, <<"dropDups">> => true}),
    {ok, ok} = mongo_worker:ensure_index(navicc_gps, #{<<"key">> => #{<<"system">> => 1, <<"hour">> => 1}}),
    ok.
