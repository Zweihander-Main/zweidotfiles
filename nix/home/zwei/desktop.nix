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
    ./alacritty
    ./coreutils
    ./copyq
    ./dunst
    ./emacs
    ./flameshot
    ./mouseless
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
