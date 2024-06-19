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
    ./x11
  ];

  home.packages = with pkgs; [
    # user programs
    firefox
    zathura
    unzip
    sxhkd
    # dev
    gcc
    python3
  ];
}
