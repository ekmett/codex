{ mkDerivation, adjunctions, atlas, base, bytestring, const
, containers, data-default, distributive, file-embed, fontconfig
, freetype, gl, glow, harfbuzz, harfbuzz-icu, hkd, hspec
, lens, linear, mtl, primitive, sdl2, StateVar, stdenv
, text, transformers, vector, weak
}:
mkDerivation {
  pname = "ui";
  version = "0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    adjunctions atlas base bytestring const containers data-default
    distributive file-embed fontconfig freetype gl glow harfbuzz
    harfbuzz-icu hkd lens linear mtl primitive sdl2
    StateVar text transformers vector weak
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    adjunctions atlas base bytestring const containers data-default
    distributive file-embed fontconfig freetype gl glow harfbuzz
    harfbuzz-icu hkd hspec lens linear mtl primitive sdl2
    StateVar text transformers vector weak
  ];
  homepage = "https://github.com/ekmett/ui#readme";
  description = "UI toolbox to complement Coda";
  license = [ stdenv.lib.licenses.bsd2 stdenv.lib.licenses.asl20 ]; 
}
