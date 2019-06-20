let
  hostNix = import <nixpkgs> {};
  shellyPin = hostNix.pkgs.lib.importJSON ./shelly.json;

  shelly = hostNix.pkgs.fetchFromGitHub {
    owner = "yesodweb";
    repo  = "Shelly.hs";
    inherit (shellyPin) rev sha256;
  };
in
  shelly
