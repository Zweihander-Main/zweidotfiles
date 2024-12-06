{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: {
  home.packages = with pkgs; [
    anki
    gtk3
  ];
}
