gargoyle-postgresql-connect
===========================

This module provides a unified interface, `withDb`, which, given a filepath will:

1. Determine whether the filepath points to a file that contains a database url and, if so, attempt to connect.
2. If the filepath points to a directory, assume that this directory is a local database and attempt to connect.
3. If no file or folder exists at the filepath, create a folder and launch a local database within.
