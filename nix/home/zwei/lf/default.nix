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
    lf
    # for ctpv in lf
    ctpv
    exiftool
    atool
    ffmpegthumbnailer
    ueberzugpp
    chafa
    jq
    glow
  ];

  xdg.configFile."lf/lfrc".source = ./lfrc;
}




