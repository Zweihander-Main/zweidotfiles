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
    viddy
  ];

  xdg.configFile."viddy.toml".source = ./viddy.toml;
}
