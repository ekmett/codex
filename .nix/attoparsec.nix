let
  attoparsecPin = builtins.fromJSON (builtins.readFile ./attoparsec.json);

  attoparsec = builtins.fetchGit {
    inherit (attoparsecPin) url;
    ref = "master";
  };
in
  attoparsec
