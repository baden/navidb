%% -*- coding: utf-8 -*-

% TODO: название модуля вводит в заблуждение. Это скорее модуль подписки на обновления.
% В теории, его можно заменить на gproc (см. edice).

-module(navidb_subs).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-export([
    watch/1,
    subscribe/2,
    pids/0,
    listeners/1,
    resources/1,
    broadcast/3
    % unwatch/2
]).

-define(SERVER, global:whereis_name(?MODULE)).
% Данные помещаются в очередь и отправляются через POOLTIMEOUT миллисекунд
% -define(POOLTIMEOUT, 1000).
% -define(POOLTIMEOUT, 500).
-define(POOLTIMEOUT, 50).

-record(state, {
    pids,           % Список всех websocket-клиентов
    pool,           % Очередь сообщений
    subscribes :: ets:tid(),    % Подписчики на ресурсы,    связь  ресурс -> pid
    resources :: ets:tid()      % Список ресурсов,          связь     pid -> ресурс
}).

% TODO!
%
%  Не хватает защиты от ситуации внеплановой рассинхронизации
%  Это если почему-то очередь будет расти, а событие send по таймауту потеряется.
%  В этом случае очередь будет расти бесконечно.
%  Можно конечно было добавить проверку длины очереди, но процедура length(list) весьма затратная
%  и не хотелось бы ее вызывать на каждый broadcast

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    register(?MODULE, self()),    % set readable name for debugging in observer:start()
    process_flag(trap_exit, true),  % set this so we can catch death of watchers in pids:
    {ok, #state{
                pids = [],
                pool = [],
                subscribes = ets:new(?MODULE, [bag]),
                resources = ets:new(?MODULE, [bag])
               }
    }.

stop() ->
    gen_server:call(?SERVER, {stop}).

% API calls

% При подключении Websocket-клиент регистрируется для совсем Широковещательных сообщений
watch(Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {watch, Pid}).

% Список ресурсов, за которыми необходимо следить
subscribe(Pid, Resources) when is_pid(Pid) ->
    gen_server:call(?SERVER, {subscribe, Pid, Resources}).

pids() ->
    gen_server:call(?SERVER, {pids}).

listeners(Key) ->
    gen_server:call(?SERVER, {listeners, Key}).

resources(Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {resources, Pid}).

broadcast(Collection, Key, Document) ->
    Message = #{
        resource => atom_to_binary(Collection, latin1),
        id       => Key,
        data     => Document
    },
    gen_server:cast(?SERVER, {broadcast, Message}).

% Sync

handle_call({watch, Pid}, _From, #state{pool=_Pool, pids=Pids} = State) when is_pid(Pid) ->
    link(Pid), % tell us if they exit, so we can unwatch them
    {reply, {ok, "Logedin"}, State#state{pids= [Pid] ++ Pids}};

handle_call({subscribe, Pid, Keys}, _From, #state{subscribes = Subscribes, resources = Resources} = State) when is_pid(Pid) ->
    [ets:insert(Subscribes, {Key, Pid}) || Key <- Keys],
    [ets:insert(Resources, {Pid, Key}) || Key <- Keys],
    {reply, {ok, "Subscribed"}, State};

handle_call({pids}, _From, #state{pids=Pids} = State) ->
    {reply, {ok, Pids}, State};

handle_call({listeners, Key}, _From, #state{subscribes = Subscribes} = State) ->
    Pids = [Pid || {_, Pid} <- ets:lookup(Subscribes, Key)],
    {reply, {ok, Pids}, State};

handle_call({resources, Pid}, _From, #state{resources = Resources} = State) ->
    Keys = [Key || {_, Key} <- ets:lookup(Resources, Pid)],
    {reply, {ok, Keys}, State};

handle_call(Request, _From, State) ->
    error_logger:warning_msg("Unimplemented navidb_subs:handle_call (~p)~n", [Request]),
    {reply, ignored, State}.

% Async

% Никто не смотрит, можно не собирать пул и даже дополнительно очистить его
handle_cast({broadcast, _Message}, #state{pool=_Pool, pids=Pids} = State) when Pids == [] ->
    {noreply, State#state{pool = []}};

% TODO: Вот этот момент мне не нравится.
handle_cast({broadcast, Message}, #state{pool=Pool, pids=_Pids} = State) when Pool == [] ->
    erlang:send_after(?POOLTIMEOUT, self(), pool),
    {noreply, State#state{pool = [Message]}};

handle_cast({broadcast, Message}, #state{pool=Pool, pids=_Pids} = State) ->
    % Необходимо обратить внимание на обратный порядок списка!!!
    {noreply, State#state{pool = [Message] ++ Pool}};

handle_cast(Msg, State) ->
    error_logger:warning_msg("Unimplemented navidb_subs:handle_cast(~p)~n", [Msg]),
    {noreply, State}.

% Messages

tokey(_Rec = #{resource := Resource, id := Id}) ->
    % Resource = proplists:get_value(resource, Rec),
    % Id = proplists:get_value(id, Rec),
    <<Resource/binary, ":", Id/binary>>.

handle_info(pool, #state{pool=Pool, subscribes = Subscribes} = State) ->
    % Сформируем список сообщений для каждого клиента
    % Пройдемся по всем сообщениям пула
    Messages = dict:to_list(lists:foldl(
        fun (M, Acc) ->
            % Добавим сообщение M всем слушателям
            Pids = [Pid || {_, Pid} <- ets:lookup(Subscribes, tokey(M))],
            lists:foldl(
                fun (Pid, Acc2) ->
                    dict:append(Pid, M, Acc2)
                end,
                Acc,
                Pids
            )
        end,
        dict:new(),
        lists:reverse(Pool)
    )),

    [ Pid ! {json, [{messages, M}]} || {Pid, M} <- Messages ],

    {noreply, State#state{pool = []}};

handle_info({'EXIT', Pid, _Why}, #state{pids = Pids, subscribes = Subscribes, resources = Resources} = State) ->
    % force logout:
    % handle_call({unwatch, Pid}, blah, State);

    % Получим все связки {Keys, Pid}
    KeyRows = [{K, P} || {P, K} <- ets:lookup(Resources, Pid)],

    % Удалим все записи Pid -> Key
    ets:delete(Resources, Pid),
    [ets:delete_object(Subscribes, Obj) || Obj <- KeyRows],

    % Удалим из общего списка
    {noreply, State#state{pids = lists:delete(Pid, Pids)}};

handle_info(Info, State) ->
    error_logger:warning_msg("Caught unhandled navidb_subs:handle_info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
