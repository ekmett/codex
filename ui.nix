{ mkDerivation, adjunctions, base, bytestring, containers, data-default, distributive, gl, hedgehog, JuicyPixels, lens
, linear, primitive, sdl2, StateVar, stdenv, text, vector, bindings-freetype-gl
}:
let deps = 
  [ adjunctions base bytestring containers data-default distributive gl 
    JuicyPixels lens linear primitive sdl2 StateVar text vector bindings-freetype-gl
  ];
in mkDerivation {
  pname = "ui";
  version = "0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = deps;
  executableHaskellDepends = deps;
  testHaskellDepends = [ hedgehog ] ++ deps;
  homepage = "https://github.com/ekmett/ui#readme";
  description = "UI toolbox to complement Coda";
  license = [ stdenv.lib.licenses.bsd2 stdenv.lib.licenses.asl20 ];
}
