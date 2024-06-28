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
    (lib.mkIf (!config.hostAttr.preinstalled.vim) vim)
  ];

  xdg.configFile."vim/vimrc".source = ./vimrc;
  xdg.configFile."vim/gvimrc".source = ./gvimrc;
  systemd.user.tmpfiles.rules = ["d ${homeDir}/.config/vim/pack/plugins"];
}
