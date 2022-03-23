let
  packages = import ./.;
  inherit (packages) pkgs plutus-starter;
  inherit (pkgs) lib stdenv nixpkgs-fmt;
  inherit (plutus-starter) haskell pre-commit-hooks-nix stylish-haskell;

  # Configure project pre-commit hooks
  pre-commit-check = pre-commit-hooks-nix.run {
    src = ./.;
    tools = {
      stylish-haskell = stylish-haskell;
      nixpkgs-fmt = nixpkgs-fmt;
      shellcheck = pkgs.shellcheck;
    };
    hooks = {
      stylish-haskell.enable = true;
      hlint.enable = true;
      nixpkgs-fmt = {
        enable = true;
        # While nixpkgs-fmt does exclude patterns specified in `.ignore` this
        # does not appear to work inside the hook. For now we have to thus
        # maintain excludes here *and* in `./.ignore` and *keep them in sync*.
        excludes = [ ".*nix/sources.nix$" ".*/packages.nix$" ];
      };
      shellcheck.enable = true;
    };
  };

  nixpkgsInputs = (with pkgs;
    [
      libsodium-vrf
      ghcid
      niv
      nixpkgs-fmt
    ] ++ lib.optionals stdenv.isDarwin [ clang ]);

  localInputs = (with plutus-starter; [
    hlint
    cabal-install
    haskell-language-server
    stylish-haskell
    cardano-repo-tool
    # HACK: This shouldn't need to be here.
    pkgs.lzma.dev
  ]);

  devInputs = (with pkgs; [
    haskellPackages.hasktags
    haskellPackages.hindent
    haskellPackages.hpc
    haskellPackages.happy
  ]);

in
  haskell.project.shellFor {
    withHoogle = false;
    nativeBuildInputs = nixpkgsInputs ++ localInputs ++ devInputs;
    shellHook = ''
      ${pre-commit-check.shellHook}
      export PATH=$(pwd)/.ghcup/bin:$PATH
    '';
  }
