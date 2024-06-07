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
    ./emacs
    ./lf
    ./redshift
    ./stalonetray
    ./syncthing
    ./time
    ./x11
  ];

  home.packages = with pkgs; [
    # user programs
    alacritty
    tmux
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
