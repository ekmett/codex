{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let

  overlay = self: super: {
    # We require a minimum version of fontconfig lib for specific functionality.
    # This is a version that should be equialent for some macs, so pin it.
    fontconfig = super.fontconfig.overrideAttrs (oldAttrs: rec {
      version = "2.13.1";
      name    = "fontconfig-${version}";
      src     = pkgs.fetchurl {
        url = "https://www.freedesktop.org/software/fontconfig/release/fontconfig-2.13.1.tar.gz";
        sha256 = "0zzspdnydj9g5fxl9804c7rr8lsm6bmm0f7mz5awcpyp74mqa3cz";
      };
      buildInputs = oldAttrs.buildInputs ++
        [ super.libuuid # new build dependency
        ];
    });

    # Upgrade the harf of buzz, we require the Unicode 12 updates.
    harfbuzz = super.harfbuzz.overrideAttrs (oldAttrs: rec {
      version = "2.5.0";
      name = "harfbuzz-${version}";
      src = pkgs.fetchurl {
        url = "https://www.freedesktop.org/software/harfbuzz/release/harfbuzz-${version}.tar.xz";
        sha256 = "1vqnqkzz7ws29g5djf31jj6a9dbid8a27a8y4balmy5lipwp774m";
      };
    });

    harfbuzz-icu = self.harfbuzz.override { withIcu = true; };
  };

  pkgs = import nixpkgs { overlays = [overlay]; };

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  # Codex packages                    
  sources = {
    atlas        = ./lib/atlas;
    const        = ./lib/const;
    freetype     = ./lib/freetype;
    glow         = ./lib/glow;
    harfbuzz     = ./lib/harfbuzz;
    harfbuzz-icu = ./lib/harfbuzz-icu;
    weak         = ./lib/weak;
    fontconfig   = ./lib/fontconfig;
    hkd          = ./lib/hkd;
  };

  nihs = {
    stb = ./nih/stb;
  };

  c2nix = p: n: args:
    # Why type when you can function?
    p.callCabal2nix n sources.${n} args;

  # Basic overrides to include our packages
  modHaskPkgs = haskellPackages.override {
    overrides = hself: hsuper: {

      atlas        = (c2nix hsuper "atlas" {}).overrideAttrs (_: {
        # nix builds don't like symlinks, move the real thing in.
        postUnpack = ''
          rm atlas/cbits/stb_rect_pack.h
          cp ${nihs.stb}/stb_rect_pack.h atlas/cbits/
        '';
      });

      hkd          = c2nix hsuper "hkd" {};
      const        = c2nix hsuper "const" {};
      weak         = c2nix hsuper "weak" {};
      # Provide the external lib dependency to match the cabal file
      freetype     = c2nix hself "freetype" { freetype2 = pkgs.freetype; };
    };
  };

  # Adding any of these into the modHaskPkgs overrides will result in an
  # infinite recursion error. :)
  fontconfig = c2nix modHaskPkgs "fontconfig" {};
  harfbuzz   = c2nix modHaskPkgs "harfbuzz" {};
  harfbuzz-icu = c2nix modHaskPkgs "harfbuzz-icu" { harfbuzz = harfbuzz; };
  glow       = c2nix modHaskPkgs "glow" {};

  # Build the UI derivation and include our specific dependencies.
  ui = modHaskPkgs.callPackage ./ui.nix { 
    fontconfig = fontconfig;
    harfbuzz = harfbuzz;
    harfbuzz-icu = harfbuzz-icu;
    glow = glow;
  };

in
  # Working this way seems to clash with the cabal.project file? Need a better
  # way to build an env from the combined buildInputs from ALL THE THINGS. Then
  # I could work on all of it with impunity instead of having to 'mv
  # cabal.project FOO' before doing anything ;<
  pkgs.haskell.lib.shellAware ui
