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
    flameshot
  ];

  xdg.configFile."autostart/Flameshot.desktop".source = ./Flameshot.desktop;
  xdg.configFile."systemd/user/app-Flameshot@autostart.service.d/override.conf".source = ./override.conf;
}
