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
    ./mouseless
    ./redshift
    ./stalonetray
    ./sxhkd
  ];

  home.packages = with pkgs; [
  ];
}
