{ mkDerivation, adjunctions, base, bindings-freetype-gl, bytestring
, containers, data-default, distributive, file-embed, gl, glow
, hedgehog, hspec, JuicyPixels, lens, linear, mtl, packing
, primitive, sdl2, StateVar, stdenv, text, transformers, vector
}:
mkDerivation {
  pname = "ui";
  version = "0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    adjunctions base bindings-freetype-gl bytestring containers
    data-default distributive file-embed gl glow JuicyPixels lens
    linear mtl packing primitive sdl2 StateVar text transformers vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    adjunctions base bindings-freetype-gl bytestring containers
    data-default distributive file-embed gl glow hedgehog hspec
    JuicyPixels lens linear mtl packing primitive sdl2 StateVar text
    transformers vector
  ];
  homepage = "https://github.com/ekmett/ui#readme";
  description = "UI toolbox to complement Coda";
  license = [ stdenv.lib.licenses.bsd2 stdenv.lib.licenses.asl20 ]; 
}
