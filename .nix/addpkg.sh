#! /usr/bin/env nix-shell
#! nix-shell --pure -i bash -p nix-prefetch-git
OWNER=$1
PKG=$2

NUX="let
  ${PKG}Pin = builtins.fromJSON (builtins.readFile ./$PKG.json);

  $PKG = builtins.fetchGit {
    inherit (${PKG}Pin) url ref;
    ref = \"master\";
  };
in
  $PKG"

nix-prefetch-git "https://github.com/$OWNER/$PKG" > "$PKG.json"
echo "$NUX" > "$PKG.nix"

