{
  inputs,
  lib,
  config,
  pkgs,
  outputs,
  secrets,
  ...
}: {
  imports = [
    ./default.nix
    ./coreutils
    ./copyq
    ./dunst
    #./emacs
    ./redshift
    ./stalonetray
    ./sxhkd
  ];

  home.packages = with pkgs; [
  ];
}
