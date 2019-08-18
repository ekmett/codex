let
  cabal-doctestPin = builtins.fromJSON (builtins.readFile ./cabal-doctest.json);

  cabal-doctest = builtins.fetchGit {
    inherit (cabal-doctestPin) url;
    ref = "master";
  };
in
  cabal-doctest
