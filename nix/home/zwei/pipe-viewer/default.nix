{
  pkgs,
  config,
  lib,
  overlays,
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

  # TODO: port to desktop with optional audioonly option

  xdg.configFile."pipe-viewer/pipe-viewer.conf".source = overlays.mkTemplate ./pipe-viewer.conf.j2 {
    player = "audioonly";
  };
}
