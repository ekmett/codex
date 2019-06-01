primitive-statevar
==================

[![Hackage](https://img.shields.io/hackage/v/primitive-statevar.svg)](https://hackage.haskell.org/package/primitive-statevar)

This package provides a backwards compatible improvement on the 'StateVar' library, which extends
the concept of a `StateVar` to allow it to work in other `PrimMonad`s from `primitive`.

To use import this module instead of `Data.StateVar`, and you can use the combinators in this library
with existing `StateVar`s from packages like `gl` or `sdl2`. (You may have to hide any re-exports
those libraries make of the combinators from `Data.StateVar`, however!)

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

