# gargoyle-postgresql

This package provides tools for managing PostgreSQL servers that live in local folders and communicate via a Unix domain socket. It uses the <https://hackage.haskell.org/package/gargoyle gargoyle> package in order to automatically initialize, spin up, and spin down such servers according to client demand.

The `gargoyle-psql` executable is such a client which will try to connect to a PostgreSQL server at a given location:

```
gargoyle-psql db

psql (9.5.6)
Type "help" for help.

postgres=#
```

Note that `gargoyle-psql` assumes that PostgreSQL executables such as `psql` are available on the PATH. A custom 'Gargoyle' is required to use non-standard PostgreSQL installations.

The following is an example of using this package to run [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple) actions using a local DB:


```haskell
import Database.PostgreSQL.Simple
import Gargoyle
import Gargoyle.PostgreSQL

withDb :: String -> (Connection -> IO a) -> IO a
withDb dbPath a = withGargoyle defaultPostgres dbPath $ \dbUri -> a =<< connectPostgreSQL dbUri
```
