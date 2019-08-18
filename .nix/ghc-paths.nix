let
  ghc-pathsPin = builtins.fromJSON (builtins.readFile ./ghc-paths.json);

  ghc-paths = builtins.fetchGit {
    inherit (ghc-pathsPin) url rev;
    ref = "master";
  };
in
  ghc-paths
