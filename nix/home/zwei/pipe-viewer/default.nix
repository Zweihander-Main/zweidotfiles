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
    # j2cli
  ];

  systemd.user.tmpfiles.rules = ["d ${homeDir}/vids/towatch"];

  # TODO: remove old config, port to desktop
  # TODO: music only option for config

  # Templating using jinja2?

  # pkgs.runCommand
  # "myscript"
  # {
  #   passAsFile = [
  #     "paramsJson"
  #   ];
  #   paramsJson = builtins.toJSON {
  #     items = [
  #       "socks"
  #       "towel"
  #     ];
  #   };
  # }
  # '''
  #   ${pkgs.j2cli}/bin/j2 -f json ${./my-script.j2} "$paramsJsonPath" > "$out"
  # ''

  xdg.configFile."pipe-viewer/pipe-viewer.conf".source = ./pipe-viewer.conf;
}
