pgapp
=====

This code is a front end for epgsql that adds fuse and poolboy in
front of the actual Postgres connections.

Use:

    application:ensure_all_started(pgapp).
    pgapp:connect([{size, 10}, {database, "mydb"}, {username, "foo"}, {password, "bar"}]).
    pgapp:equery("select current_date", []).

**Note**: It is still experimental, and is likely to break.  The API is not stable.
