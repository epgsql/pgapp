-module(pgapp_sup).

-behaviour(supervisor).

-include("worker_args.hrl").

%% API
-export([start_link/0, add_pool/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Pools} = application:get_env(pgapp, pools),
    PoolSpec = lists:map(fun ({PoolName, SizeArgs, ConnectionArgs}) ->
                                make_child_spec(PoolName, SizeArgs, ConnectionArgs, undefined);
                             ({PoolName, SizeArgs, ConnectionArgs, SQLFile}) ->
                                make_child_spec(PoolName, SizeArgs, ConnectionArgs, SQLFile)
                         end, Pools),
    {ok, { {one_for_one, 10, 10}, PoolSpec} }.

make_child_spec(PoolName, SizeArgs, ConnectionArgs, SQLFile) ->
  PoolArgs = [{name, {local, PoolName}},
    {worker_module, pgapp_worker}] ++ SizeArgs,
  poolboy:child_spec(PoolName, PoolArgs, make_worker_args(ConnectionArgs, SQLFile)).

make_worker_args(ConnectionArgs, SQLFileName) ->
  SQL = case SQLFileName of
    undefined -> [];
    File ->
      {ok, PreparedSQL} = file:consult(File),
      PreparedSQL
  end,
  #worker_args{connection_args = ConnectionArgs, prepared_sql = SQL}.

add_pool(Name, PoolArgs, ConnectionArgs) ->
  add_pool(Name, PoolArgs, ConnectionArgs, undefined).

add_pool(Name, PoolArgs, ConnectionArgs, SQLFile) ->
  ChildSpec = poolboy:child_spec(Name, PoolArgs, make_worker_args(ConnectionArgs, SQLFile)),
  supervisor:start_child(?MODULE, ChildSpec).
