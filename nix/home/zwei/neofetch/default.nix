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
    neofetch
  ];

  xdg.configFile."neofetch/config.conf".source = ./config.conf;
}

# TODO: replace with fork





