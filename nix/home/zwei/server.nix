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
    ./default.nix
    ./coreutils
  ];

  home.packages = with pkgs; [
  ];
}
