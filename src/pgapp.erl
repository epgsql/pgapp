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
-export([connect/1, equery/2]).

%%%===================================================================
%%% API
%%%===================================================================

connect(Settings) ->
    PoolSize = proplists:get_value(size, Settings, 5),
    MaxOverflow = proplists:get_value(max_overflow, Settings, 5),
    pgapp_sup:add_pool(epgsql_pool,
                       [{name, {local, epgsql_pool}},
                        {worker_module, pgapp_worker},
                        {size, PoolSize},
                        {max_overflow, MaxOverflow}],
                       Settings).

equery(Sql, Params) ->
    poolboy:transaction(epgsql_pool,
                        fun(Worker) ->
                                gen_server:call(Worker, {equery, Sql, Params})
                        end).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
