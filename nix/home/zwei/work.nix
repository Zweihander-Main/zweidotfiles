{
  inputs,
  lib,
  config,
  pkgs,
  outputs,
  secrets,
  ...
}: with lib; {
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

## TODO: DRY, possibly get rid of 'type'
