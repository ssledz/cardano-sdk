# common-nix-config

### install in new project

Set a named remote pointing to this repository:

```
git remote add -f nix git@gitlab.binarapps.com:plutus/common-nix-config.git
```

Add a subtree:

```
git subtree add --prefix nix nix master --squash
```

Add `shell.nix` to project root, like:

```nix
import ./nix/shell.nix
```

And finally add a `release.nix` with docker images:

```nix
let
  packages = import ./nix;
  inherit (packages) pkgs plutus-starter;
  project = plutus-starter.haskell.project;
  name = "mobile-wallet-backend";
  package = project.mobile-wallet-backend.components.exes.mobile-wallet-backend;
  appConfig = ./app.config;
  runAsRoot = ''
    mkdir -p /etc/mobile-wallet-backend/
    cp ${appConfig} /etc/mobile-wallet-backend/app.config
  '';
  config = {
    Cmd = [ "/bin/mobile-wallet-backend" "start-app" "--config" "/etc/mobile-wallet-backend/app.config" ];
  };
in
  {
    mobile-wallet-backend = import ./nix/image.nix {
      inherit pkgs package name config runAsRoot;
    };
  }
```
