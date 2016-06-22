My Watch
========

HTTP server for viewing MySQL queries on multiple servers. Designed to work
behind [Sproxy](https://github.com/zalora/sproxy).


Requirements
============

MyWatch is written in Haskell with [GHC](http://www.haskell.org/ghc/).
All required Haskell libraries are listed in [mywatch.cabal](mywatch.cabal).
Use [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install)
to fetch and build all pre-requisites automatically.


Installation
============

    $ git clone https://github.com/zalora/mywatch.git
    $ cd mywatch
    $ cabal install


Usage
=====

Type `mywatch --help` to see usage summary:

    Usage:
      mywatch [options] MYCNF

    Options:

      -d, --datadir=DIR        Data directory including static files [default: {cabal data dir}]

      -s, --socket=SOCK        Listen on this UNIX-socket [default: /tmp/mywatch.sock]
      -p, --port=PORT          Instead of UNIX-socket, listen on this TCP port (localhost)

      -h, --help               Show this message



Configuration
=============

The `MYCNF` argument denotes a MySQL client config file.  Each section in this
file describes a MySQL server where you can view processes. As usually, the
"client" section applies internally to all other sections, but is ignored by
MyWatch. Remember to make this file secret if it includes passwords, or use the
`!include` directive. However, MyWatch does not parse included files for
more sections.

```
[foo]
host = example.com
user = user1
...

[bar]
host = example.net
user = user2
...

!include /run/keys/my.secret.cnf
```


Database Privileges
===================

MyWatch needs the `PROCESS` privilege.


Screenshots
===========
![MyWatch1](./screenshots/mywatch-1.png)
![MyWatch2](./screenshots/mywatch-2.png)

