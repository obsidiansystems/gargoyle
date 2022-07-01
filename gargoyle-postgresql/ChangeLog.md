# Changelog for gargoyle-postgresql
 
## 0.2.0.2

* Add support for ghc 9.0.2

## 0.2.0.1

* Disable test suite

## 0.2

* Bugfix: Replace use of file handle with /dev/null to fix a crash due to lack of referential transparency in GHCs behavior with respect to file Handles
* Loosen version bounds
* Add `gargoyle-pg-run`, which makes it easier to run gargoyle-database-using programs (e.g., psql, pg_dump).

## 0.1

* Initial release
