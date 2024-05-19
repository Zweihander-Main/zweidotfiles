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
  ];

  xdg.configFile."bat/config".source = ./config;
  xdg.configFile."bat/syntaxes".source = ./syntaxes;
  xdg.configFile."bat/themes".source = ./themes;



}





