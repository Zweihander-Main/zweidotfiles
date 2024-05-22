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
    navi
  ];

  xdg.configFile."navi/config.yaml".source = ./config.yaml;
  xdg.dataFile."navi/cheats".source = ./cheats;
}
