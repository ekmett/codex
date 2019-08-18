let
  dlistPin = builtins.fromJSON (builtins.readFile ./dlist.json);

  dlist = builtins.fetchGit {
    inherit (dlistPin) url;
    ref = "master";
  };
in
  dlist
