let
  sources = import ./sources.nix { inherit pkgs; };
  plutus = import sources.plutus-apps {};
  pkgs = plutus.pkgs;
  pre-commit-hooks-nix = import sources.pre-commit-hooks-nix;
  haskell-nix = pkgs.haskell-nix;
  plutus-starter = import ./pkgs {
    inherit pkgs haskell-nix sources plutus pre-commit-hooks-nix;
  };
  project = plutus-starter.haskell.project;
in
  {
    inherit pkgs plutus plutus-starter project;
  }
