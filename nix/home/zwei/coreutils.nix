{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: let 
  homeDir = config.home.homeDirectory;
in {
  imports = [
    ./lf        # file manager
    ./bat       # cat++
  ];

  home.packages = with pkgs; [
    autojump    # cd enhancement
    broot       # ls --tree
    delta       # diff viewer
    fd          # find replacement
    fzf         # fuzzy finder
    lsd         # ls replacement
    mcfly       # history enhancement
    mcfly-fzf   # mcfly fzf integration
    moar        # pager replacement
    navi        # cheat sheets
    neofetch    # rice
    ripgrep     # grep replacement
    sd          # sed replacement
    thefuck     # correct prev command
    viddy       # watch replacement
  ];
}

  # TODO navi port
  # TODO neofetch port and replacement
  # TODO viddy port
