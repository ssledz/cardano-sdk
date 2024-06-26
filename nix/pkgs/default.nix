{ pkgs
, sources
, plutus
, haskell-nix
, pre-commit-hooks-nix
}:

let
  gitignore-nix = pkgs.callPackage plutus."gitignore.nix" { };
  compiler-nix-name = plutus.plutus-apps.haskell.compiler-nix-name;
  haskell = pkgs.callPackage ./haskell {
    inherit gitignore-nix sources haskell-nix;
    inherit compiler-nix-name; # Use the same GHC version as plutus
    inherit (pkgs) libsodium-vrf;
  };
  hlint = plutus.plutus-apps.hlint;
  cabal-install = plutus.plutus-apps.cabal-install;
  stylish-haskell = plutus.plutus-apps.stylish-haskell;
  haskell-language-server = plutus.plutus-apps.haskell-language-server;
  cardano-repo-tool = plutus.plutus-apps.cardano-repo-tool;
in
  {
    inherit haskell hlint cabal-install stylish-haskell cardano-repo-tool;
    inherit haskell-language-server pre-commit-hooks-nix;
  }
