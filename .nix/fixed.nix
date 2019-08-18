let
  fixedPin = builtins.fromJSON (builtins.readFile ./fixed.json);

  fixed = builtins.fetchGit {
    inherit (fixedPin) url rev;
    ref = "master";
  };
in
  fixed
