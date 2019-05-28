harfbuzz-icu
============

[![Hackage](https://img.shields.io/hackage/v/harfbuzz-icu.svg)](https://hackage.haskell.org/package/harfbuzz-icu)

This package lets ICU supply the unicode tables needed by harfbuzz.

In addition to installing icu4c and harfbuzz, 

```
$ brew install icu4c harfbuzz
```

to get this to run on a mac you may need to supply some additional help. In my case this consisted of
the statement:

```
$ export PKG_CONFIG_PATH="/usr/local/opt/icu4c/lib/pkgconfig:$PKG_CONFIG_PATH"
```

before `pkg-config` could locate everything needed to link against the `harfbuzz-icu` library.

This statement is actually suggested by the output of `brew info icu4c`.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

