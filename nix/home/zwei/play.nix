{
  inputs,
  lib,
  config,
  pkgs,
  outputs,
  secrets,
  ...
}:
with lib; {
  imports = [
    ./default.nix
    ./alacritty
    ./coreutils
    ./copyq
    ./dunst
    ./emacs
    ./flameshot
    ./mouseless
    ./pcmanfm
    ./pipe-viewer
    ./redshift
    ./sound
    ./stalonetray
    ./sxhkd
    ./tmux
    ./time
    ./udiskie
    ./vim
  ];

  home.packages = with pkgs; [
  ];
}
