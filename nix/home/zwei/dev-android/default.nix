{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: {
  #TODO: build out
  home.packages = with pkgs; [
    android-studio
  ];
}
