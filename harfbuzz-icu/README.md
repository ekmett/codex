harfbuzz-icu
============

[![Hackage](https://img.shields.io/hackage/v/harfbuzz-icu.svg)](https://hackage.haskell.org/package/harfbuzz-icu)

This package lets ICU supply the unicode tables needed by harfbuzz.

On a mac:

```
$ brew install icu4c harfbuzz
```

The cabal file uses `pkg-config` to find these on all platforms except for a Mac.

Because Apple ships a crippled subset of ICU, the install for `icu4c` is keg-only. This means that `pkg-config`
can't find it without assistance.

We try to be clever and find it ourselves, but in the event that we screw up, you can tell `pkg-config` about
icu4c yourself and then tell us to trust `pkg-config`.

```
$ export PKG_CONFIG_PATH="/usr/local/opt/icu4c/lib/pkgconfig:$PKG_CONFIG_PATH"
$ cabal new-install harfbuzz-icu -ftrust-darwin-pkg-config
```

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

