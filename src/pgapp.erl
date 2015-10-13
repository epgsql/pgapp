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
-export([connect/1, connect/2, equery/2, equery/3, squery/1, squery/2,
         with_transaction/1, with_transaction/2]).

%%%===================================================================
%%% API
%%%===================================================================

connect(Settings) ->
    connect(epgsql_pool, Settings).

connect(PoolName, Settings) ->
    PoolSize = proplists:get_value(size, Settings, 5),
    MaxOverflow = proplists:get_value(max_overflow, Settings, 5),
    pgapp_sup:add_pool(PoolName,
                       [{name, {local, PoolName}},
                        {worker_module, pgapp_worker},
                        {size, PoolSize},
                        {max_overflow, MaxOverflow}],
                       Settings).

-spec equery(Sql::epgsql:sql_query(),
             Params :: list(epgsql:bind_param())) -> epgsql:reply(epgsql:equery_row()).
equery(Sql, Params) ->
    pgapp_worker:equery(Sql, Params).

-spec equery(PoolName::atom(), Sql::epgsql:sql_query(),
             Params :: list(epgsql:bind_param())) -> epgsql:reply(epgsql:equery_row()).
equery(PoolName, Sql, Params) ->
    pgapp_worker:equery(PoolName, Sql, Params).

-spec squery(Sql::epgsql:sql_query()) -> epgsql:reply(epgsql:squery_row()) |
                                         [epgsql:reply(epgsql:squery_row())].
squery(Sql) ->
    pgapp_worker:squery(Sql).

-spec squery(PoolName::atom(), Sql::epgsql:sql_query()) -> epgsql:reply(epgsql:squery_row()) |
                                                           [epgsql:reply(epgsql:squery_row())].
squery(PoolName, Sql) ->
    pgapp_worker:squery(PoolName, Sql).

with_transaction(PoolName, Fun) when is_function(Fun, 0) ->
    pgapp_worker:with_transaction(PoolName, Fun).

with_transaction(Fun) when is_function(Fun, 0) ->
    with_transaction(epgsql_pool, Fun).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
