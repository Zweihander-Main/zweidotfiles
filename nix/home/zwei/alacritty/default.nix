{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: {
  home.packages = with pkgs; [
    alacritty
  ];

  xdg.configFile."alacritty/alacritty.toml".source = ./alacritty.toml;
  xdg.configFile."alacritty/colors.toml".source = ./colors.toml;
}
