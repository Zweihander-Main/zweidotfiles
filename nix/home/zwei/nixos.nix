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
    ./copyq
    ./coreutils
    ./dunst
    ./emacs
    ./flameshot
    ./lf
    ./mouseless
    ./redshift
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
    ungoogled-chromium
    sysz
    zathura
    unzip
    sxhkd
    # dev
    gcc
    python3
  ];
}
