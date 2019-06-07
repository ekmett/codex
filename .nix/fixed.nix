
let
  hostNix = import <nixpkgs> {};
  fixedPin = hostNix.pkgs.lib.importJSON ./fixed.json;

  fixed = hostNix.pkgs.fetchFromGitHub {
    owner = "ekmett";
    repo  = "fixed";
    inherit (fixedPin) rev sha256;
  };
in
  fixed
