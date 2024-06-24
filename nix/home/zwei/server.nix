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
    ./vim
  ];

  home.packages = with pkgs; [
  ];
}
