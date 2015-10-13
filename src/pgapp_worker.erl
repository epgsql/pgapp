%% Worker for poolboy.  Initial code from
%% https://github.com/devinus/poolboy
%%
%% Copyright 2015 DedaSys LLC <davidw@dedasys.com>

-module(pgapp_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([squery/1, squery/2, equery/2, equery/3, with_transaction/2]).
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {conn::pid(),
                delay::pos_integer(),
                timer::timer:tref(),
                start_args::proplists:proplist()}).

-define(INITIAL_DELAY, 500). % Half a second
-define(MAXIMUM_DELAY, 5 * 60 * 1000). % Five minutes

-define(STATE_VAR, '$pgapp_state').

squery(Sql) ->
    case get(?STATE_VAR) of
        undefined ->
            squery(epgsql_pool, Sql);
        Conn ->
            epgsql:squery(Conn, Sql)
    end.

squery(PoolName, Sql) ->
    poolboy:transaction(PoolName,
                        fun (Worker) ->
                                gen_server:call(Worker, {squery, Sql})
                        end).


equery(Sql, Params) ->
    case get(?STATE_VAR) of
        undefined ->
            equery(epgsql_pool, Sql, Params);
        Conn ->
            epgsql:equery(Conn, Sql, Params)
    end.

equery(PoolName, Sql, Params) ->
    poolboy:transaction(PoolName,
                        fun (Worker) ->
                                gen_server:call(Worker, {equery, Sql, Params})
                        end).

with_transaction(PoolName, Fun) ->
    poolboy:transaction(PoolName,
                        fun (Worker) ->
                                gen_server:call(Worker, {transaction, Fun})
                        end).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    {ok, connect(#state{start_args = Args, delay = ?INITIAL_DELAY})}.

handle_call({squery, Sql}, _From, #state{conn=Conn} = State) when Conn /= undefined ->
    {reply, epgsql:squery(Conn, Sql), State};

handle_call({equery, Sql, Params}, _From, #state{conn = Conn} = State) when Conn /= undefined ->
    {reply, epgsql:equery(Conn, Sql, Params), State};

handle_call({transaction, Fun}, _From, #state{conn = Conn} = State) when Conn /= undefined ->
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

terminate(_Reason, #state{conn=Conn}) ->
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
