{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: let 
  homeDir = config.home.homeDirectory;
in {
  home.packages = with pkgs; [
    bat
    broot
    delta
    fd
    fzf
    lsd
    mcfly
    navi
    ripgrep
    viddy
    zsh-fzf-tab
    moar
    neofetch
    autojump
    mcfly-fzf
  ];
}




