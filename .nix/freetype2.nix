let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    freetype2-pinned = initialNixpkgs.pkgs.lib.importJSON ./freetype2.json;
    freetype2 = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "lamdu";
      repo = "freetype2";
      inherit (freetype2-pinned) rev sha256;
    };
  };
in
  sources.freetype2