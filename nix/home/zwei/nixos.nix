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
    ./sound
    ./stalonetray
    ./sxhkd
    ./syncthing
    ./time
    ./tmux
    ./udiskie
    ./vim
    ./x11
  ];

  home.packages = with pkgs; [
    # user programs
    firefox
    zathura
    xdg-ninja
  ];
}
