%% Worker for poolboy.  Initial code from
%% https://github.com/devinus/poolboy
%%
%% Copyright 2015 DedaSys LLC <davidw@dedasys.com>

-module(pgapp_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([squery/1, squery/2, squery/3,
         equery/2, equery/3, equery/4,
         with_transaction/2, with_transaction/3]).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {conn::pid(),
                delay::pos_integer(),
                timer::timer:tref(),
                start_args::proplists:proplist()}).

-define(INITIAL_DELAY, 500). % Half a second
-define(MAXIMUM_DELAY, 5 * 60 * 1000). % Five minutes
-define(TIMEOUT, 5 * 1000).

-define(STATE_VAR, '$pgapp_state').

squery(Sql) ->
    case get(?STATE_VAR) of
        undefined ->
            squery(epgsql_pool, Sql);
        Conn ->
            epgsql:squery(Conn, Sql)
    end.

squery(PoolName, Sql) when is_atom(PoolName) ->
    squery(PoolName, Sql, ?TIMEOUT);
squery(Sql, Timeout) ->
    squery(epgsql_pool, Sql, Timeout).

squery(PoolName, Sql, Timeout) ->
    middle_man_transaction(PoolName,
                           fun (W) ->
                                   gen_server:call(W, {squery, Sql}, Timeout)
                           end, Timeout).

equery(Sql, Params) ->
    case get(?STATE_VAR) of
        undefined ->
            equery(epgsql_pool, Sql, Params);
        Conn ->
            epgsql:equery(Conn, Sql, Params)
    end.

equery(PoolName, Sql, Params) when is_atom(PoolName) ->
    equery(PoolName, Sql, Params, ?TIMEOUT);
equery(Sql, Params, Timeout) ->
    equery(epgsql_pool, Sql, Params, Timeout).

equery(PoolName, Sql, Params, Timeout) ->
    middle_man_transaction(PoolName,
                           fun (W) ->
                                   gen_server:call(W, {equery, Sql, Params},
                                                   Timeout)
                           end, Timeout).

with_transaction(PoolName, Fun) ->
    with_transaction(PoolName, Fun, ?TIMEOUT).

with_transaction(PoolName, Fun, Timeout) ->
    middle_man_transaction(PoolName,
                           fun (W) ->
                                   gen_server:call(W, {transaction, Fun},
                                                   Timeout)
                           end, Timeout).

middle_man_transaction(Pool, Fun, Timeout) ->
    Tag = make_ref(),
    {Receiver, Ref} = erlang:spawn_monitor(
                        fun() ->
                                process_flag(trap_exit, true),
                                Result = poolboy:transaction(Pool, Fun,
                                                             Timeout),
                                exit({self(),Tag,Result})
                        end),
    receive
        {'DOWN', Ref, _, _, {Receiver, Tag, Result}} ->
            Result;
        {'DOWN', Ref, _, _, {timeout, _}} ->
            {error, timeout};
        {'DOWN', Ref, _, _, Reason} ->
            {error, Reason}
    end.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    {ok, connect(#state{start_args = Args, delay = ?INITIAL_DELAY})}.

handle_call(_Query, _From, #state{conn = undefined} = State) ->
    {reply, {error, disconnected}, State};
handle_call({squery, Sql}, _From,
            #state{conn=Conn} = State) ->
    {reply, epgsql:squery(Conn, Sql), State};
handle_call({equery, Sql, Params}, _From,
            #state{conn = Conn} = State) ->
    {reply, epgsql:equery(Conn, Sql, Params), State};
handle_call({transaction, Fun}, _From,
            #state{conn = Conn} = State) ->
    put(?STATE_VAR, Conn),
    Result = epgsql:with_transaction(Conn, fun(_) -> Fun() end),
    erase(?STATE_VAR),
    {reply, Result, State}.

handle_cast(reconnect, State) ->
    {noreply, connect(State)}.

handle_info({'EXIT', From, Reason}, State) ->
    {NewDelay, Tref} =
        case State#state.timer of
            undefined ->
                %% We add a timer here only if there's not one that's
                %% already active.
                Delay = calculate_delay(State#state.delay),
                {ok, T} =
                    timer:apply_after(
                      State#state.delay,
                      gen_server, cast, [self(), reconnect]),
                {Delay, T};
            Timer ->
                {State#state.delay, Timer}
        end,

    error_logger:warning_msg(
      "~p EXIT from ~p: ~p - attempting to reconnect in ~p ms~n",
      [self(), From, Reason, NewDelay]),
    {noreply, State#state{conn = undefined, delay = NewDelay, timer = Tref}}.

terminate(_Reason, #state{conn = undefined}) ->
    ok;
terminate(_Reason, #state{conn = Conn}) ->
    ok = epgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connect(State) ->
    Args = State#state.start_args,
    Hostname = proplists:get_value(host, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),

    case epgsql:connect(Args) of
        {ok, Conn} ->
            error_logger:info_msg(
              "~p Connected to ~s at ~s with user ~s: ~p~n",
              [self(), Database, Hostname, Username, Conn]),
            timer:cancel(State#state.timer),
            State#state{conn=Conn, delay=?INITIAL_DELAY, timer = undefined};
        Error ->
            NewDelay = calculate_delay(State#state.delay),
            error_logger:warning_msg(
              "~p Unable to connect to ~s at ~s with user ~s (~p) "
              "- attempting reconnect in ~p ms~n",
              [self(), Database, Hostname, Username, Error, NewDelay]),
            {ok, Tref} =
                timer:apply_after(
                  State#state.delay, gen_server, cast, [self(), reconnect]),
            State#state{conn=undefined, delay = NewDelay, timer = Tref}
    end.

calculate_delay(Delay) when (Delay * 2) >= ?MAXIMUM_DELAY ->
    ?MAXIMUM_DELAY;
calculate_delay(Delay) ->
    Delay * 2.
