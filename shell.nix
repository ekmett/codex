{ nixpkgs ? import ./.nix/nixpkgs.nix
, compiler ? "ghc881"
}: let
  overlay = self: super:
    let
      # Codex packages, add new source locations here first. If you
      # hit infinite recursion errors when trying to 'nix-shell' you
      # might need to pass in some other packages explicitly like
      # the harfbuzz stuff below.
      codexSources = {
        atlas               = { path = ./atlas;               args = {}; };
        bidi-icu            = { path = ./bidi-icu;            args = {}; };
        const               = { path = ./const;               args = {}; };
        engine              = { path = ./engine;              args = {}; };
        glow                = { path = ./glow;                args = {}; };
        hkd                 = { path = ./hkd;                 args = {}; };
        parsnip             = { path = ./parsnip;             args = {}; };
        primitive-ffi       = { path = ./primitive-ffi;       args = {}; };
        primitive-statevar  = { path = ./primitive-statevar;  args = {}; };
        primitive-unlift    = { path = ./primitive-unlift;    args = {}; };
        ptrdiff             = { path = ./ptrdiff;             args = {}; };
        smawk               = { path = ./smawk;               args = {}; };
        ui                  = { path = ./ui;                  args = {}; };
        watch               = { path = ./watch;               args = {}; };
        watch-directory     = { path = ./watch-directory;     args = {}; };
        weak                = { path = ./weak;                args = {}; };

        # Snowflakes who need things passed in explicitly, usually
        # because they depend on a system library of their own name.
        # Feed them their deps manually to prevent infinite recursion.
        fontconfig = {
          path = ./fontconfig;
          args = { inherit (self) fontconfig; };
        };

        freetype = {
          path = ./freetype;
          args = { freetype2 = self.freetype; };
        };

        harfbuzz = {
          path = ./harfbuzz;
          args = { inherit (self) harfbuzz; };
        };
      };
    in {
      haskellPackages =
        let
          # Use old versions of tools we never link against.
          oldTools = hself: hsuper: {
            cabal2nix = super.haskellPackages.cabal2nix;
            happy = super.haskellPackages.happy;
          };

          hackageFixes = hself: hsuper: with pkgs.haskell.lib;
            let
              dontCabalDoctest = drv: overrideCabal drv (drv: {
                doCheck = false;
                setupHaskellDepends = builtins.filter
                  (dep: dep != null && dep.pname != "cabal-doctest")
                  drv.setupHaskellDepends;
              });
            in {
              # Blocks of fixes are in increasing order of invasiveness.

              # Jailbreaks.
              QuickCheck = doJailbreak hsuper.QuickCheck;
              system-fileio = doJailbreak hsuper.system-fileio;
              tasty-hspec = doJailbreak hsuper.tasty-hspec;
              vector-binary-instances =
                doJailbreak hsuper.vector-binary-instances;

              # cabal-doctest doesn't build yet, so we slice it out
              # from packages that use it.
              comonad       = dontCabalDoctest hsuper.comonad;
              distributive  = dontCabalDoctest hsuper.distributive;
              lens          = dontCabalDoctest hsuper.lens;
              linear        = dontCabalDoctest hsuper.linear;
              semigroupoids = dontCabalDoctest hsuper.semigroupoids;

              # Newer versions from within the package set.
              binary-orphans = doJailbreak hsuper.binary-orphans_1_0_1;
              gl = hsuper.gl_0_9;
              inline-c = hsuper.inline-c_0_8_0_1;
              sdl2 = dontCheck hsuper.sdl2_2_5_0_0;

              # On hackage, but not in the package set or all-cabal-hashes.
              JuicyPixels = hsuper.callHackageDirect {
                pkg = "JuicyPixels";
                ver = "3.3.3.1";
                sha256 = "0kal9l2c8frnm8kvdql5n4947n38jv3q3jd2vw8lix5ha21j6w91";
              } {};

              # Too new for hackage.
              bytes = dontCabalDoctest
                (hsuper.callCabal2nix "bytes" (import ./.nix/bytes.nix) {});
              fixed = hsuper.callCabal2nix "fixed" (import ./.nix/fixed.nix) {};
              ghc-paths = hsuper.callCabal2nix "ghc-paths"
                (import ./.nix/ghc-paths.nix) {};

              # Patched packages.
              shelly = appendPatches
                (hsuper.callCabal2nix "shelly" (import ./.nix/shelly.nix) {})
                [ (super.fetchpatch {
                    url = "https://github.com/yesodweb/Shelly.hs/commit/5afa9c3f99cc34fbdc2505cdd5b58e6e98713540.diff";
                    sha256 = "1hgr3k71rl6napw5ps258fkywqk9jdpbm7hgns1m97ybsj6k04p1";
                  })

                  (super.fetchpatch {
                    url = "https://github.com/yesodweb/Shelly.hs/commit/5093c266200ce4ee61075af310b12f5c9cc7faa6.diff";
                    sha256 = "0073l42nvzrq2mmv75h4xvn159c8m6vbm0j46qqi07xsmvf2adgr";
                  })
                ];
            };

          codexPackages = hself: hsuper: pkgs.lib.mapAttrs
            (n: { path, args }: hsuper.callCabal2nix n path args)
            codexSources;

          codexPackageFixes = hself: hsuper: with pkgs.haskell.lib; {
            harfbuzz = addBuildDepends hsuper.harfbuzz [ self.freetype ];
          };

          baseHaskellPackages = if compiler == "default"
            then super.haskellPackages
            else super.haskell.packages.${compiler};

        in
          baseHaskellPackages.override (old: {
            overrides = builtins.foldl' super.lib.composeExtensions
              (old.overrides or (_: _: {})) [
                oldTools
                hackageFixes
                codexPackages
                codexPackageFixes
              ];
          });

      # Minimum version of freetype2 is required
      freetype = super.callPackage ./.nix/freetype {};

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
      harfbuzz-icu = super.harfbuzz.override { withIcu = true; };
      harfbuzz-subset = super.harfbuzz;

      # some renames to keep cabal & pkg-config happy
      icu-uc = super.icu;

      codexShell = self.haskellPackages.shellFor {
        packages = p:
          super.lib.attrsets.attrVals (builtins.attrNames codexSources) p;
      };
    };

  pkgs = import nixpkgs { overlays = [overlay]; };
in
  pkgs.codexShell
