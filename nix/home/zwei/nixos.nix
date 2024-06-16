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
    alacritty
    firefox
    sysz
    zathura
    unzip
    sxhkd
    # dev
    gcc
    python3
  ];
}
