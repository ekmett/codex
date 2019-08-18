codex
=====

This repo acts as a monorepo holding a bunch of related libraries for graphics and font manipulation in Haskell.

The end goal is to assemble all of this into a nice GUI/editor layer for coda, but you can enjoy the incremental
fruits of our labors for their own merits.

Using Nix
---------

There is a `shell.nix` that will build a development environment for these packages. It includes the various C dependencies that we use and defaults to use `ghc881` and `cabal-3.0`.

_*YOU WILL NEED TO USE THE NIX SPECIFIC PROJECT FILE:*_

```sh
$ cabal --project=nix-shell-cabal.project v2-configure --enable-tests
```


Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the ##coda or #haskell IRC channels on irc.freenode.net.

-Edward Kmett
