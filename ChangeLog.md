0.2.0
=====

  * Allow killing queries via `CALL mysql.mywatch_kill(id)`. You should have
    this routine on your database server and grant MyWatch privilege to
    execute it. Provided a safe example of this function.

  * Minor improvements in UI


0.1.2
=====

  * Use location hash for server name
  * Compact server list (`display: inline-block`)


0.1.1
=====

  * Request `HEAD /server/:server/processlist.json` before
    showing the server to user. This is to hide servers which
    are not allowed by Sproxy to this user.

  * Added a workaround for buggy haskell mysql package
    causing a heisenbug that random sections of the
    configuration files were not found by libmysqlclient.

  * Added a workaround for the way MariaDB's libmysqlclient
    processes SSL options. SSL now works with MariaDB's
    libmysqlclient.

  * Fixed parsing of `GRANT` queries (they have `NULL` states)


0.1.0
=====

  * Initial version
  * Only view queries

