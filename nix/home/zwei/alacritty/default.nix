{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: {
  home.packages = with pkgs; [
    (lib.mkIf (!config.hostAttr.preinstalled.alacritty) alacritty)
  ];

  xdg.configFile."alacritty/alacritty.toml".source = ./alacritty.toml;
  xdg.configFile."alacritty/colors.toml".source = ./colors.toml;
}
