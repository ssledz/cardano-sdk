let
  packages = import ./nix;

  inherit (packages) pkgs plutus plutus-starter project;

  inherit (plutus-starter) haskell stylish-haskell devcontainer;

in
{
  inherit pkgs plutus sdk;

  inherit project;

  devcontainer =
    import ./nix/devcontainer/dex-devcontainer.nix { inherit pkgs sdk; };
}
