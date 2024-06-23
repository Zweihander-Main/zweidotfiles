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
    ./chezmoi
    ./chromium
    ./copyq
    ./coreutils
    ./dev-c
    ./dev-python
    ./dunst
    ./emacs
    ./flameshot
    ./lf
    ./mouseless
    ./redshift
    ./shellscripts
    ./stalonetray
    ./sxhkd
    ./syncthing
    ./time
    ./tmux
    ./udiskie
    ./x11
  ];

  home.packages = with pkgs; [
    # user programs
    firefox
    zathura
  ];
}
