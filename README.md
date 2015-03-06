pgapp
=====

This code is a front end for epgsql that adds a pool (poolboy, for the
moment), and code to isolate crashes in the epgsql library: if your
database goes down, you probably don't want the rest of the
application to fail as well.

Use:

    application:ensure_all_started(pgapp).
    pgapp:connect([{size, 10}, {database, "mydb"}, {username, "foo"}, {password, "bar"}]).
    pgapp:equery("select current_date", []).

The equery and squery API's are the same as those of epgsql: https://github.com/epgsql/epgsql

**Note**: It is still experimental, and is likely to break.  The API
is not stable!
