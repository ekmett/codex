{ nixpkgs ? import ./.nix/nixpkgs.nix
, compiler ? "ghc881"
, withHoogle ? false
}: let
  overlay = self: super:
    let
      # Codex packages, add new source locations here first. If you
      # hit infinite recursion errors when trying to 'nix-shell' you
      # might need to pass in some other packages explicitly like
      # the harfbuzz stuff below.
      codexSources = {
        atlas            = { path = ./atlas;            args = {}; };
        bidi-icu         = { path = ./bidi-icu;         args = {}; };
        const            = { path = ./const;            args = {}; };
        engine           = { path = ./engine;           args = {}; };
        glow             = { path = ./glow;             args = {}; };
        hkd              = { path = ./hkd;              args = {}; };
        parsnip          = { path = ./parsnip;          args = {}; };
        primitive-ffi    = { path = ./primitive-ffi;    args = {}; };
        primitive-extras = { path = ./primitive-extras; args = {}; };
        ptrdiff          = { path = ./ptrdiff;          args = {}; };
        smawk            = { path = ./smawk;            args = {}; };
        ui               = { path = ./ui;               args = {}; };
        watch            = { path = ./watch;            args = {}; };
        watch-directory  = { path = ./watch-directory;  args = {}; };
        weak             = { path = ./weak;             args = {}; };
        tabulation-hash  = { path = ./tabulation-hash;  args = {}; };
        language-server  = { path = ./language-server;  args = {}; };

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
          hackageFixes = hself: hsuper: {
            bytes = hsuper.callHackage "bytes" "0.16" {};
            inline-c = hsuper.callHackageDirect {
              pkg = "inline-c";
              ver = "0.9.0.0";
              sha256 = "07i75g55ffggj9n7f5y6cqb0n17da53f1v03m9by7s4fnipxny5m";
            } {};
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
                hackageFixes
                codexPackages
                codexPackageFixes
              ];
          });

      # We require a minimum version of fontconfig lib for specific functionality.
      # This is a version that should be equivalent for some macs, so pin it.
      fontconfig = super.fontconfig.overrideAttrs (oldAttrs: rec {
        version = "2.13.92";
        name    = "fontconfig-${version}";
        src     = pkgs.fetchurl {
          url = "https://www.freedesktop.org/software/fontconfig/release/fontconfig-${version}.tar.gz";
          sha256 = "05h02dgsyz96cbg8w77ix3rb6iygl302pl38vzik28m4hdds01il";
        };
        patches = [];
        postInstall = null;
        buildInputs = oldAttrs.buildInputs ++
          [ super.libuuid # new build dependency
          ];
      });

      # Flip the ICU bit.
      harfbuzz-icu = super.harfbuzz.override { withIcu = true; };
      harfbuzz-subset = super.harfbuzz;

      # some renames to keep cabal & pkg-config happy
      icu-uc = super.icu;

      codexShell = self.haskellPackages.shellFor {
        nativeBuildInputs = [
          self.haskellPackages.ghcid
          self.haskellPackages.cabal-install
        ];

        packages = p: super.lib.attrsets.attrVals (builtins.attrNames codexSources) p;
        inherit withHoogle;
      };
    };

  pkgs = import nixpkgs { overlays = [overlay]; };
in
  pkgs.codexShell
