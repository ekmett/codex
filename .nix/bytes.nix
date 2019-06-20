let
  bytesPin = builtins.fromJSON (builtins.readFile ./bytes.json);

  bytes = builtins.fetchGit {
    inherit (bytesPin) url rev;
    ref = "master";
  };
in
  bytes
