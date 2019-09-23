# gargoyle

Gargoyle is a framework for managing daemons from Haskell. Currently, the only requirement is that the daemon be able to communicate over a Unix domain socket. See [gargoyle-postgresql](https://hackage.haskell.org/package/gargoyle-postgresql) for an example that uses gargoyle to manage postgresql.

To use Gargoyle the client must:
* Define a value of the 'Gargoyle' type which specifies how to administer the daemon.
* Create an executable whose `main` is `gargoyleMain`. The name of this executable should match the executable name specified in the `_gargoyle_exec` field of the `Gargoyle`.
* The client will run their code with `withGargoyle` to gain access to the daemon.

# Importing into nixpkgs:

```nix
haskellPackages.override {
  overrides = self: super:
    let gargoylePkgs = import ./path/to/gargoyle self;
    in {
      inherit (gargoylePkgs) gargoyle gargoyle-postgresql gargoyle-postgresql-nix gargoyle-postgresql-connect;
    };
}
```

# Hacking

Do something like this:

```shell
nix-shell -A gargoyle-postgresql.env --run 'cd gargoyle-postgresql && cabal new-repl'
```
