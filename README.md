pgapp
=====

This code is a front end for epgsql that adds a pool (poolboy, for the
moment), and code to isolate crashes in the epgsql library: if your
database goes down, you probably don't want the rest of the
application to fail as well.

Build and start the application with Make:

    - Copy and/or rename the file `pgapp.config.sample` into `pgapp.config`
    - Enter your configuration data in `pgapp.config`
    - Run:

        ~:$ make
        ~:$ make run

    - To test it, just run:

        > pgapp:equery(a_pool_name, "select current_date", []).

      where `a_pool_name` is the name of one of the pools in your `pgapp.config` file.

API use:
    - Simple pool:

        application:ensure_all_started(pgapp).
        pgapp:connect([{size, 10}, {database, "mydb"}, {username, "foo"}, {password, "bar"}]).
        pgapp:equery("select current_date", []),
        pgapp:with_transaction(fun() ->
                                     pgapp:squery("update ..."),
                                     pgapp:squery("delete from ..."),
                                     pgapp:equery("select ? from ?", ["*", Table])
                               end).

    - Multi pool:

        application:ensure_all_started(pgapp).
        pgapp:connect(a_pool_name, [{size, 10}, {database, "mydb"}, {username, "foo"}, {password, "bar"}]).
        pgapp:equery(a_pool_name, "select current_date", []).

The equery and squery API's are the same as those of epgsql: https://github.com/epgsql/epgsql

**Note**: It is still experimental, and is likely to break.  The API
is not stable!

See [Changelog].
