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
    ./tmux
  ];

  home.packages = with pkgs; [
  ];
}
