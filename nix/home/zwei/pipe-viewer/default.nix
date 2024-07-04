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
    pipe-viewer
    mplayer
  ];

  systemd.user.tmpfiles.rules = ["d ${homeDir}/vids/towatch"];

  # TODO: ffmpeg, wget, ytdlp, players needs templating
  # TODO: music only option for config

  xdg.configFile."pipe-viewer/pipe-viewer.conf".source = ./pipe-viewer.conf;

}
