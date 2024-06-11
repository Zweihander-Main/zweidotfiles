{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: {
  home.packages = with pkgs; [
    tmux
  ];

  xdg.configFile."tmux/tmux.conf".source = ./tmux.conf;
  xdg.configFile."tmux/theme-tomorrow-night.conf".source = ./theme-tomorrow-night.conf;
}
