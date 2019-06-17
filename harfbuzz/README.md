harfbuzz
========

[![Hackage](https://img.shields.io/hackage/v/harfbuzz.svg)](https://hackage.haskell.org/package/harfbuzz)

Harfbuzz is a text shaping library that takes a sequence of unicode codepoints and a font, and selects
a sequence of glyphs and placements for those glyphs to represent the desired text. This presupposes
things like breaking text into manageable fragments, and the something like the unicode bidirectional
algorithm has already processed the data.

This package is intended to provide Haskell bindings to that library.

The standard Harfbuzz library supplies a bunch of optional extras exposed here as well.

To take full advantage of this functionality you'll need `Cabal` 3.

Then you can incur dependencies on subsets of functionality by e.g. depending on `harfbuzz:harfbuzz-subset` to just get font subsetting or `harfbuzz:harfbuzz-core` to just get the core harfbuzz functionality with no additional library dependencies. You can depend on the following sub-libraries:

harfbuzz-raw
-------------

This is used internally by the library to expose implementation details that hopefully you won't need unless you are extending `harfbuzz`.

harfbuzz-core
-------------

This supplies just the baseline functionality for `harfbuzz`, which is managing fonts, buffers, and shaping to turn buffers full of codepoints into buffers full of shaped glyphs.

harfbuzz-freetype
-------------

This supplies additional interoperability with the `freetype` library, letting you cache construction of harfbuzz typefaces from freetype fonts, and access any underlying `freetype` typeface from a harfbuzz typeface.

harfbuzz-icu
------------

This library lets ICU supply the unicode tables needed by harfbuzz.

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

harfbuzz-subset
---------------

Bindings to the harfbuzz font subsetting library. This library can be used to extract smaller
subsets of fonts for specialized purposes, leaving unused features, hints, etc. on the cutting room floor.

harfbuzz-opentype
-----------------

Harfbuzz supplies access to many extended forms of tables contained in OpenType fonts for working with variable
fonts, color fonts (such as emojis), exposing metrics for OpenType math fonts. This is packaged as a sub-library separate
from harfbuzz-core, as it is a lot of stuff, and most users won't need any of it.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

