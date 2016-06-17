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

  * Fixed parsing of `GRANT` queries (they have `NULL` states).

0.1.0
=====

  * Initial version
  * Only view queries

