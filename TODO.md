# TODO

## Font and text rendering stuff:

We need a few things to do text "right":

Assume we know the locale we're in and the 'style' of font we want to use when we draw.

1.) [X] We need to figure out where to get the fonts that supply the "best" glyphs for the requested style and language in a given locale. (e.g. `fontconfig`)

2.) [_] We need to break the text up into unidirectional runs using the current local (e.g. `fribidi`, or just the bidi algorithm in general)

3.) [X] We need a tool for positioning sequences of glyphs given a short run of text. (e.g. `harfbuzz`)

4.) [_] Given short runs of text, we may need to be able to best "guess" at the language it is in given the surrounding context.

5.) [_] Using the language info we may be able to use something like `hyphenation` to help carve up those chunks into smaller chunks that will fit.

6.) [_] We need a way to figure out where all of this goes. This may be something like Knuth & Plass's line-wrap algorithm or a smarter one based on SMAWK
    for laying out paragraphs and/or something like a pretty-printer for fairly greedy layout for things like source code. It can leverage the soft
    breaks given by `hyphenation` if needed.

7.) [_] We need a way to extract shapes or bitmaps from fonts. (e.g. stb_truetype, freetype, the rendering parts of msdfgen, external generator tools, whatever.)

8.) [_] We need to manage the resources used to render glyphs. (e.g. `atlas`/`stb_rect_pack.h`, `freetype-gl`, `stb_truetype.h`)

9.) [_] Finally given those resources, we need to get the stuff on screen. (e.g. the rendering parts of freetype-gl, glyphy, the shader from msdfgen, blitting)

Goal coverage by library:

```
 123456789
+---------+
|1        | fontconfig
| 2       | fribidi
|  3      | harfbuzz
|    5    | hyphenation
|  3   789| freetype-gl
|      7  | freetype
|       8 | atlas/stb_rect_pack
|      999| stb_truetype
+---------+
```

Current plan:

* [_] Wrap stb_truetype.h. This provides some nicely self-contained tooling for reading .ttf and .ttc files,
  this includes walking the shape of a glyph, producing bitmaps, SDFs, etc. It can also yield a really
  nice "Haskelly" API around the guts of fonts.

* [_] Wrap `glyphy`. This would provide a much higher quality SDF render in exchange for a funny shader. 

* [_] Wrap `fribidi` or wrap or build an implementation of the bidi algorithm.

* [X] Wrap `harfbuzz`. This provides layout of glyphs given a sequence of codepoints.

* [_] Actually put a shader in UI for drawing one of freetype-gl, stb_truetype or glyphy
