{ pkgs
, package
, name
, tag ? "latest"
, config
, runAsRoot
}:

pkgs.dockerTools.buildImage {
  inherit name tag config runAsRoot;
  contents = [ package pkgs.bash pkgs.bashInteractive pkgs.coreutils ];
}
