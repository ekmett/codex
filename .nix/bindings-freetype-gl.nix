let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    bindings-freetype-gl-pinned = initialNixpkgs.pkgs.lib.importJSON ./bindings-freetype-gl.json;
    bindings-freetype-gl = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "lamdu";
      repo = "bindings-freetype-gl";
      fetchSubmodules = true;
      inherit (bindings-freetype-gl-pinned) rev sha256;
    };
  };
in
  sources.bindings-freetype-gl
