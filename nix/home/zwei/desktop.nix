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
    #./emacs
    ./flameshot
    ./mouseless
    ./redshift
    ./stalonetray
    ./sxhkd
    ./tmux
    ./udiskie
  ];

  home.packages = with pkgs; [
  ];
}
