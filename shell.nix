let
  bootstrap = import <nixpkgs> { };
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };
  pkgs = import src { };
  moon = pkgs.haskellPackages.callCabal2nix "moon" ./moon.cabal {};
  devTools = [ pkgs.haskell-language-server pkgs.ghcid ];
in
  pkgs.lib.overrideDerivation moon.env (old: {
    buildInputs = old.buildInputs ++ devTools;
  })
