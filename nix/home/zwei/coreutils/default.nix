{
  pkgs,
  config,
  lib,
  secrets,
  ...
}:
with lib; let
  homeDir = config.home.homeDirectory;
in {
  imports = [
    ../lf # file manager
    ../bat # cat++
    ../help # cheat sheets
    ../fetch # rice
    ../viddy # watch replacement
  ];

  home.packages = with pkgs; [
    autojump # cd enhancement
    broot # ls --tree
    delta # diff viewer
    fd # find replacement
    fzf # fuzzy finder
    lsd # ls replacement
    mcfly # history enhancement
    mcfly-fzf # mcfly fzf integration
    moar # pager replacement
    ripgrep # grep replacement
    sd # sed replacement
    (mkIf config.hostAttr.preinstalled.systemd sysz) # systemd assistant
    thefuck # correct prev command
  ];
}
