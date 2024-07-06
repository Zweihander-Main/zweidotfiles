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
    j2cli
  ];

  systemd.user.tmpfiles.rules = ["d ${homeDir}/vids/towatch"];

  # TODO: port to desktop with optional audioonly option
  # TODO: explore creating lib of jinja and removing pkg from here

  xdg.configFile."pipe-viewer/pipe-viewer.conf".source =
    pkgs.runCommand
    "template-pipe-viewer"
    {
      passAsFile = [
        "paramsJson"
      ];
      paramsJson = builtins.toJSON {
        player = "audioonly";
      };
    }
    ''
      ${pkgs.j2cli}/bin/j2 -f json ${./pipe-viewer.conf.j2} "$paramsJsonPath" > "$out"
    '';

  # xdg.configFile."pipe-viewer/pipe-viewer.conf".source = ./pipe-viewer.conf;
}
