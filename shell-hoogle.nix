let
  packages = import ./.;
  inherit (packages) plutus-starter;
  inherit (plutus-starter) haskell;

in
haskell.project.shellFor {
  packages = ps: [ ps.cardano-tx ];
  withHoogle = true;
}
