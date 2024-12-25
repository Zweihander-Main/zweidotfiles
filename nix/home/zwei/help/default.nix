{
  pkgs,
  config,
  ...
}: let
  homeDir = config.home.homeDirectory;
in {
  home.packages = with pkgs; [
    navi # cheat sheets
    tldr # man pages
  ];

  xdg.configFile."navi/config.yaml".source = ./config.yaml;
  xdg.dataFile."navi/cheats".source = ./cheats;

  systemd.user.tmpfiles.rules = ["d ${homeDir}/.cache/tldr"];
}
