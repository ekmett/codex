let
  shellyPin = builtins.fromJSON  (builtins.readFile ./shelly.json);

  shelly = builtins.fetchGit {
    inherit (shellyPin) url rev;
    ref = "master";
  };
in
  shelly
