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
    glibcLocales # for exiftool
    atool
    ffmpegthumbnailer
    # ueberzugpp # not working atm, install locally
    chafa
    jq
    glow
  ];

  xdg.configFile."lf/lfrc".source = ./lfrc;
  xdg.configFile."ctpv/config".source = ./ctpv.config;
}
