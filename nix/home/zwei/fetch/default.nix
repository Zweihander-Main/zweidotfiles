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
    fastfetch
  ];

  xdg.configFile."fastfetch/config.jsonc".source = ./config.jsonc;
}
