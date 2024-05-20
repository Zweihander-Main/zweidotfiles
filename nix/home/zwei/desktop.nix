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
  ];

  home.packages = with pkgs; [
  ];

}
