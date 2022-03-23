{ haskell-nix
, gitignore-nix
, compiler-nix-name
, lib
, libsodium-vrf
}:

let
  project = haskell-nix.project {
    src = haskell-nix.haskellLib.cleanGit {
      name = "byron";
      src = ../../../.;
    };
    inherit compiler-nix-name;
    sha256map = { };
    modules = [
      {
        packages = {
          plutus-contract.doHaddock = false;
          plutus-contract.flags.defer-plugin-errors = false;
          plutus-use-cases.doHaddock = false;
          plutus-use-cases.flags.defer-plugin-errors = false;
          plutus-ledger.doHaddock = false;
          plutus-ledger.flags.defer-plugin-errors = false;
          plutus-tx-plugin.doHaddock = false;
          cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
          cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
        };
      }
    ];
  };
in
project
