{ nixpkgs ? import ./.nix/nixpkgs.nix
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
    atlas             = ./atlas;
    const             = ./const;
    fontconfig        = ./fontconfig;
    freetype          = ./freetype;
    glow              = ./glow;
    hkd               = ./hkd;
    harfbuzz          = ./harfbuzz;
    harfbuzz-freetype = ./harfbuzz-freetype;
    harfbuzz-opentype = ./harfbuzz-opentype;
    harfbuzz-icu      = ./harfbuzz-icu;
    ptrdiff           = ./ptrdiff;
    weak              = ./weak;
  };

  c2nix = p: n: args:
    # Why type when you can function?
    p.callCabal2nix n sources.${n} args;

  # Basic overrides to include our packages
  modHaskPkgs = haskellPackages.override {
    overrides = hself: hsuper: {

      ptrdiff      = c2nix hsuper "ptrdiff" {};
      atlas        = c2nix hsuper "atlas" {};
      hkd          = c2nix hself "hkd" {};
      const        = c2nix hsuper "const" {};
      weak         = c2nix hsuper "weak" {};

      # Provide the external lib dependency to match the cabal file
      freetype     = c2nix hself "freetype" { freetype2 = pkgs.freetype; };
    };
  };

  # Adding any of these into the modHaskPkgs overrides will result in an
  # infinite recursion error.
  fontconfig        = c2nix modHaskPkgs "fontconfig" {};
  harfbuzz          = c2nix modHaskPkgs "harfbuzz" {};
  harfbuzz-icu      = c2nix modHaskPkgs "harfbuzz-icu" { harfbuzz = harfbuzz; };
  glow              = c2nix modHaskPkgs "glow" {};

  # Build the UI derivation and include our specific dependencies.
  ui = modHaskPkgs.callPackage ./ui/ui.nix { 
    fontconfig   = fontconfig;
    harfbuzz     = harfbuzz;
    harfbuzz-icu = harfbuzz-icu;
    glow         = glow;
  };

in
modHaskPkgs.shellFor {
  packages = p: [
    p.hkd
    p.const
    p.weak
    p.atlas
    p.freetype

    fontconfig
    harfbuzz
    harfbuzz-icu
    glow
    ui
  ];
}
