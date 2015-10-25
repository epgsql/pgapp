%%%-------------------------------------------------------------------
%%% @author David N. Welton <davidw@dedasys.com>
%%% @copyright (C) 2015, David N. Welton
%%% @doc
%%%
%%% @end
%%% Created : 20 Feb 2015 by David N. Welton <davidw@dedasys.com>
%%%-------------------------------------------------------------------
-module(pgapp).

%% API
-export([connect/1, connect/2]).
-export([equery/2, equery/3, equery/4]).
-export([squery/1, squery/2, squery/3]).
-export([with_transaction/1, with_transaction/2, with_transaction/3]).
-export([connected_workers/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec connect(Settings :: list()) -> {ok, WorkerPid} when
              WorkerPid :: pid().
connect(Settings) ->
    connect(epgsql_pool, Settings).

-spec connect(PoolName :: atom(),
              Settings :: list()) -> {ok, WorkerPid} when
              WorkerPid :: pid().
connect(PoolName, Settings) ->
    PoolSize    = proplists:get_value(size, Settings, 5),
    MaxOverflow = proplists:get_value(max_overflow, Settings, 5),
    pgapp_sup:add_pool(PoolName, [{name, {local, PoolName}},
                                  {worker_module, pgapp_worker},
                                  {size, PoolSize},
                                  {max_overflow, MaxOverflow}], Settings).

-spec equery(Sql    :: epgsql:sql_query(),
             Params :: list(epgsql:bind_param()))
            -> epgsql:reply(epgsql:equery_row()).
equery(Sql, Params) ->
    pgapp_worker:equery(Sql, Params).

-spec equery(Sql     :: epgsql:sql_query(),
             Params  :: list(epgsql:bind_param()),
             Timeout :: atom() | integer())
            -> epgsql:reply(epgsql:equery_row()).
equery(Sql, Params, Timeout) ->
    pgapp_worker:equery(Sql, Params, Timeout).


-spec equery(PoolName :: atom(), Sql::epgsql:sql_query(),
             Params   :: list(epgsql:bind_param()),
             Timeout  :: atom() | integer())
            -> epgsql:reply(epgsql:equery_row()).
equery(PoolName, Sql, Params, Timeout) ->
    pgapp_worker:equery(PoolName, Sql, Params, Timeout).

-spec squery(Sql :: epgsql:sql_query())
            -> epgsql:reply(epgsql:squery_row()) |
               [epgsql:reply(epgsql:squery_row())].
squery(Sql) ->
    pgapp_worker:squery(Sql).

-spec squery(Sql::epgsql:sql_query(),
             Timeout :: atom() | integer())
            -> epgsql:reply(epgsql:squery_row()) |
               [epgsql:reply(epgsql:squery_row())].
squery(Sql, Timeout) ->
    pgapp_worker:squery(Sql, Timeout).

-spec squery(PoolName :: atom(),
             Sql      :: epgsql:sql_query(),
             Timeout  :: atom() | integer())
            -> epgsql:reply(epgsql:squery_row()) |
               [epgsql:reply(epgsql:squery_row())].
squery(PoolName, Sql, Timeout) ->
    pgapp_worker:squery(PoolName, Sql, Timeout).

-spec with_transaction(Function :: fun(() -> Reply))
                      -> Reply | {rollback, any()} when Reply :: any().
with_transaction(Fun) when is_function(Fun, 0) ->
    with_transaction(epgsql_pool, Fun).

-spec with_transaction(PoolName :: atom(),
                       Function :: fun(() -> Reply))
                      -> Reply | {rollback, any()} when Reply :: any().
with_transaction(PoolName, Fun) when is_function(Fun, 0) ->
    pgapp_worker:with_transaction(PoolName, Fun).

-spec with_transaction(PoolName :: atom(),
                       Function :: fun(() -> Reply),
                       Timeout  :: atom() | non_neg_integer())
                      -> Reply | {rollback, any()} when Reply :: any().
with_transaction(PoolName, Fun, Timeout) when is_function(Fun, 0) ->
    pgapp_worker:with_transaction(PoolName, Fun, Timeout).

-spec connected_workers(PoolName :: atom()) -> {ok, ConnectedWorkerPids} when
                        ConnectedWorkerPids :: list(pid()).
connected_workers(PoolName) ->
    WorkerPids = gen_server:call(PoolName, get_avail_workers),
    {ok, [WorkerPid || WorkerPid <- WorkerPids,
          pgapp_worker:is_connected(WorkerPid)]}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
