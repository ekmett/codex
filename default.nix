{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let
  pkgs = import nixpkgs {};

  sources = {
    freetype2            = import ./nix/freetype2.nix;
    bindings-freetype-gl = import ./nix/bindings-freetype-gl.nix;
  };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  modHaskPkgs = haskellPackages.override {
    overrides = hself: hsuper: {
      freetype2 = hsuper.callCabal2nix "freetype2" sources.freetype2 {};
      bindings-freetype-gl = 
        let
          drvArgs = if pkgs.buildPlatform.isLinux 
                      then { GLEW = pkgs.glew; }
                      else {};

          drv = hself.callCabal2nix "bindings-freetype-gl" 
                  sources.bindings-freetype-gl 
                  drvArgs;

          drvPatched = pkgs.haskell.lib.appendPatch drv 
                        ./nix/bindings-freetype-gl-fix-setuphs.patch;
        in
          drvPatched;
    };
  };

  drv = modHaskPkgs.callPackage ./codex.nix {};

in
  pkgs.haskell.lib.shellAware drv
