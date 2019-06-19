{ nixpkgs ? import ./.nix/nixpkgs.nix
, compiler ? "ghc881"
}:
let
  bootNixpkgs = import nixpkgs {};

  # Codex packages, add source location here first. If you hit
  # infinite recursion errors when trying to 'nix-shell' then you
  # might have to move the package to 'recursiveSrcs'.
  localSources = {
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
    parsnip            = ./parsnip;
  };

  overlay = self: super: {
    haskellPackages =
      let
        ghcVersion = hself: hsuper: {
          ghc = super.haskell.packages.ghc881.ghc.overrideAttrs (old: rec {
            ghcTargetPrefix = super.stdenv.lib.optionalString
              (super.stdenv.targetPlatform != super.stdenv.hostPlatform)
              "${super.stdenv.targetPlatform.config}-";

            version = "8.8.0.20190613";
            name = "${ghcTargetPrefix}ghc-${version}";
            src = super.fetchurl {
              url = "https://downloads.haskell.org/ghc/8.8.1-alpha2/ghc-${version}-src.tar.xz";
              sha256 = "17531jabkdmlhj57mkshjfwlri2g3jgal8fw9zpkl1kbplnrivyr";
            };
          });
        };

        # no-caching hitting sadness :<
        hackageFixes = hself: hsuper: with pkgs.haskell.lib; rec {
          # cabal-doctest doesn't build yet, so we need to slice it
          # out from packages that use it.
          dontCabalDoctest = drv: overrideCabal drv (drv: {
            doCheck = false;
            setupHaskellDepends = builtins.filter
              (dep: dep != null && dep.pname != "cabal-doctest")
              drv.setupHaskellDepends;
          });

          # There's something I don't understand here: cabal-doctest
          # is listed in setupHaskellDepends for these pacakges, but
          # isn't being found.
          #bytes         = dontCabalDoctest hsuper.bytes;
          comonad       = dontCabalDoctest hsuper.comonad;
          distributive  = dontCabalDoctest hsuper.distributive;
          #ft-properties = dontCabalDoctest hsuper.ft-properties;
          lens = dontCabalDoctest hsuper.lens;
          #linear        = dontCabalDoctest hsuper.linear;
          semigroupoids = dontCabalDoctest hsuper.semigroupoids;

          #fixed         = hsuper.callCabal2nix "fixed" (import ./.nix/fixed.nix) {};

          basement = appendPatches hsuper.basement [
              # https://github.com/haskell-foundation/foundation/pull/520 but edited
              ./.nix/basement/0001-prevent-pattern-case-fail-calls.patch
              ./.nix/basement/0002-basement-fails.patch
            ];
          foundation = appendPatch (dontCheck hsuper.foundation)
            ./.nix/foundation/0001-foundation-fails.patch;
          yaml = appendPatch hsuper.yaml
            ./.nix/yaml/0001-yaml-fails.patch;

          ghc-paths = appendPatch hsuper.ghc-paths
            ./.nix/ghc-paths/0001-cabal-3-fixes.patch;
          hackage-db = appendPatch hsuper.hackage-db
            ./.nix/hackage-db/0001-cabal-3-fixes.patch;
          language-nix = appendPatch hsuper.language-nix
            ./.nix/language-nix/0001-cabal-3-fixes.patch;

          QuickCheck = doJailbreak hsuper.QuickCheck;

          # configuration-8.8.x.nix appends a patch. Maybe not needed
          # for alpha2?
          happy = overrideCabal hsuper.happy (drv: {
            patches = [];
          });
        };

        localPackages = hself: hsuper:
          pkgs.lib.mapAttrs (n: p: hsuper.callCabal2nix n p {}) localSources;

        baseHaskellPackages =
          if compiler == "default"
            then super.haskellPackages
            else super.haskell.packages.${compiler};
      in
        baseHaskellPackages.override (old: {
          overrides = builtins.foldl' super.lib.composeExtensions
            (old.overrides or (_: _: {})) [
              # ghcVersion
              hackageFixes
              localPackages
            ];
        });

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
  inherit (pkgs) haskellPackages;

  # Move sources to here if we find they're triggering 'infinite
  # recursion' errors when building the environment.
  #
  # Sean: I'm sure that I'm missing something as having mutually
  # recursive packages is supposed to be handled by `shellFor`??
  #
  recursiveSrcs = rec {
    fontconfig          = haskellPackages.callCabal2nix "fontconfig" ./fontconfig {};
    fontconfig-freetype = haskellPackages.callCabal2nix "fontconfig-freetype" ./fontconfig-freetype {};
    glow                = haskellPackages.callCabal2nix "glow" ./glow {};

    harfbuzz            = haskellPackages.callCabal2nix "harfbuzz" ./harfbuzz {};
    harfbuzz-icu        = haskellPackages.callCabal2nix "harfbuzz-icu" ./harfbuzz-icu { harfbuzz = harfbuzz; };
    harfbuzz-subset     = haskellPackages.callCabal2nix "harfbuzz-subset" ./harfbuzz-subset { harfbuzz = harfbuzz; };
    harfbuzz-freetype   = haskellPackages.callCabal2nix "harfbuzz-freetype" ./harfbuzz-freetype { harfbuzz = harfbuzz; };
    engine              = haskellPackages.callCabal2nix "engine" ./engine { glow = glow; };
    ui                  = haskellPackages.callCabal2nix "ui" ./ui {
      fontconfig   = fontconfig;
      glow         = glow;
    };
  };

in
#haskellPackages.shellFor {
#  packages = p:
#  # Add the normal packages to the environment
#  pkgs.lib.attrsets.attrVals (builtins.attrNames localSources) p ++
#
#  # Add our recursive snowflake packages to the environment
#  pkgs.lib.attrValues recursiveSrcs;
#}
haskellPackages
