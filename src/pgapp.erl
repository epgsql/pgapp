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
         prepared_query/3, prepared_query/2]).

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
    equery(epgsql_pool, Sql, Params).

-spec equery(PoolName::atom(), Sql::epgsql:sql_query(),
             Params :: list(epgsql:bind_param())) -> epgsql:reply(epgsql:equery_row()).
equery(PoolName, Sql, Params) ->
    poolboy:transaction(PoolName,
                        fun(Worker) ->
                                gen_server:call(Worker, {equery, Sql, Params})
                        end).

-spec prepared_query(Name::string,Params :: list(epgsql:bind_param())) ->
                                          epgsql:reply(epgsql:equery_row()).
prepared_query(Name, Params) ->
  prepared_query(epgsql_pool, Name, Params).

-spec prepared_query(PoolName::atom(), Name::string,
    Params :: list(epgsql:bind_param())) -> epgsql:reply(epgsql:equery_row()).
prepared_query(PoolName, Name, Params) ->
  poolboy:transaction(PoolName,
    fun(Worker) ->
      gen_server:call(Worker, {prepared_query, Name, Params})
    end).


-spec squery(Sql::epgsql:sql_query()) -> epgsql:reply(epgsql:squery_row()) |
                                         [epgsql:reply(epgsql:squery_row())].
squery(Sql) ->
    squery(epgsql_pool, Sql).

-spec squery(PoolName::atom(), Sql::epgsql:sql_query()) -> epgsql:reply(epgsql:squery_row()) |
                                                           [epgsql:reply(epgsql:squery_row())].
squery(PoolName, Sql) ->
    poolboy:transaction(PoolName,
                        fun(Worker) ->
                                gen_server:call(Worker, {squery, Sql})
                        end).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
