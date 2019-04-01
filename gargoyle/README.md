# gargoyle
  
Gargoyle is a framework for managing daemons from Haskell. Currently, the only requirement is that the daemon be able to communicate over a Unix domain socket. See [gargoyle-postgresql](https://hackage.haskell.org/package/gargoyle-postgresql) for an example that uses gargoyle to manage postgresql.
  
To use Gargoyle the client must:
* Define a value of the 'Gargoyle' type which specifies how to administer the daemon.
* Create an executable whose `main` is `gargoyleMain`. The name of this executable should match the executable name specified in the `_gargoyle_exec` field of the `Gargoyle`.
* The client will run their code with `withGargoyle` to gain access to the daemon.
