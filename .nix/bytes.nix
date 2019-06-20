
let
  hostNix = import <nixpkgs> {};
  bytesPin = hostNix.pkgs.lib.importJSON ./bytes.json;

  bytes = hostNix.pkgs.fetchFromGitHub {
    owner = "ekmett";
    repo  = "bytes";
    inherit (bytesPin) rev sha256;
  };
in
  bytes
