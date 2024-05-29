{
  inputs,
  lib,
  config,
  pkgs,
  outputs,
  secrets,
  ...
}: {
  imports = [
    ./server.nix
    ./redshift.nix
  ];

  home.packages = with pkgs; [
  ];
}
