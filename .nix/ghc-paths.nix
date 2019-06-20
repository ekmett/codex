
let
  hostNix = import <nixpkgs> {};
  ghc-pathsPin = hostNix.pkgs.lib.importJSON ./ghc-paths.json;

  ghc-paths = hostNix.pkgs.fetchFromGitHub {
    owner = "simonmar";
    repo  = "ghc-paths";
    inherit (ghc-pathsPin) rev sha256;
  };
in
  ghc-paths
