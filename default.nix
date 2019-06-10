{ nixpkgs ? import ./.nix/nixpkgs.nix
, compiler ? "default"
}:
let
  overlay = self: super: {
    # Minimum version of freetype2 is required
    freetype = super.callPackage ./.nix/freetype {};
    freetype2 = self.freetype;

    # We require a minimum version of fontconfig lib for specific functionality.
    # This is a version that should be equivalent for some macs, so pin it.
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

    # Flip the ICU bit.
    harfbuzz-icu = self.harfbuzz.override { withIcu = true; };
    harfbuzz-subset = self.harfbuzz;

    # some renames to keep cabal & pkg-config happy
    icu-uc = super.icu;
  };

  pkgs = import nixpkgs { overlays = [overlay]; };

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  # Codex packages, add source location here first. If you hit
  # infinite recursion errors when trying to 'nix-shell' then you
  # might have to move the package to 'recursiveSrcs'.
  sources = {
    atlas              = ./atlas;
    bidi-icu           = ./bidi-icu;
    const              = ./const;
    freetype           = ./freetype;
    harfbuzz-opentype  = ./harfbuzz-opentype;
    hkd                = ./hkd;
    primitive-statevar = ./primitive-statevar;
    primitive-ffi      = ./primitive-ffi;
    primitive-unlift   = ./primitive-unlift;
    ptrdiff            = ./ptrdiff;
    weak               = ./weak;
    smawk              = ./smawk;
    watch              = ./watch;
    watch-directory    = ./watch-directory;
  };

  # Basic overrides to include our packages
  modHaskPkgs = haskellPackages.override {
    overrides = hself: hsuper: pkgs.lib.mapAttrs (n: p: hself.callCabal2nix n p {}) sources // {
      # no-caching hitting sadness :<
      gl    = pkgs.haskell.lib.doJailbreak hsuper.gl;
      fixed = hself.callCabal2nix "fixed" (import ./.nix/fixed.nix) {};
    };
  };

  # Move sources to here if we find they're triggering 'infinite
  # recursion' errors when building the environment.
  #
  # Sean: I'm sure that I'm missing something as having mutually
  # recursive packages is supposed to be handled by `shellFor`??
  #
  recursiveSrcs = rec {
    fontconfig          = modHaskPkgs.callCabal2nix "fontconfig" ./fontconfig {};
    fontconfig-freetype = modHaskPkgs.callCabal2nix "fontconfig-freetype" ./fontconfig-freetype {};
    glow                = modHaskPkgs.callCabal2nix "glow" ./glow {};

    harfbuzz            = modHaskPkgs.callCabal2nix "harfbuzz" ./harfbuzz {};
    harfbuzz-icu        = modHaskPkgs.callCabal2nix "harfbuzz-icu" ./harfbuzz-icu { harfbuzz = harfbuzz; };
    harfbuzz-subset     = modHaskPkgs.callCabal2nix "harfbuzz-subset" ./harfbuzz-subset { harfbuzz = harfbuzz; };
    harfbuzz-freetype   = modHaskPkgs.callCabal2nix "harfbuzz-freetype" ./harfbuzz-freetype { harfbuzz = harfbuzz; };
    engine              = modHaskPkgs.callCabal2nix "engine" ./engine { glow = glow; };
    ui                  = modHaskPkgs.callCabal2nix "ui" ./ui {
      fontconfig   = fontconfig;
      glow         = glow;
    };
  };

in
modHaskPkgs.shellFor {
  packages = p:
  # Add the normal packages to the environment
  pkgs.lib.attrsets.attrVals (pkgs.lib.attrNames sources) p ++
  # Add our recursive snowflake packages to the environment
  pkgs.lib.attrValues recursiveSrcs;
}
