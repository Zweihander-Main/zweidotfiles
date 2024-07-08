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
    syncthing
  ];

  services.syncthing = {
    enable = true;
    tray.enable = true;
  };
}
