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
    dwm
    dmenu
    xorg.xinit
    xorg.xmodmap
    xss-lock
    gnupg
    i3lock-color
  ];
}
