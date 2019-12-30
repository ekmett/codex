{ nixpkgs ? import ./.nix/nixpkgs.nix
, compiler ? "ghc881"
, withHoogle ? false
}: let

  overlay = self: super:
  
    let
      cherrypick-stb-lib = name: file: ver: desc: super.stdenv.mkDerivation {
        pname = name;
        version = ver;
        src = super.fetchFromGitHub {
          owner = "nothings";
          repo = "stb";
          rev = "f67165c2bb2af3060ecae7d20d6f731173485ad0";
          sha256 = "1b8r55q4siqfvm1r4wdqgj4vwzw0bfhypgm60dk4b5c7i7rgxx3g";
        };
        phases = [ "unpackPhase" "installPhase" ];
        outputs = [ "out" "dev" ];
        nativeBuildInputs = [ self.pkgconfig ];
        installPhase = ''
          mkdir -p $out/include/${name}
          # We only care about a single library/header
          cp ${file} $out/include/${name}/

          mkdir -p $dev/lib/pkgconfig

          # These should exist in the stb repo, probably...
          cat >> $dev/lib/pkgconfig/${name}.pc << EOF
          prefix=$out
          exec_prefix=$out
          includedir=$out/include/${name}

          Name: ${name}
          Description: ${desc}
          Version: ${ver}
          Cflags: -I$out/include/${name}
          EOF
        '';
      };

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
        stb-truetype     = { path = ./stb-truetype;     args = {}; };

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
            stb-truetype = addPkgconfigDepends hsuper.stb-truetype
              [ self.stb_rect_pack
                self.stb_truetype
              ];
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

      # pin stb to recent version
      stb_rect_pack = cherrypick-stb-lib
        "stb_rect_pack"
        "stb_rect_pack.h"
        "1.00"
        "simple 2D rectangle packer with decent quality";

      stb_truetype = cherrypick-stb-lib
        "stb_truetype"
        "stb_truetype.h"
        "1.22"
        "Font text rasteriser";

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
