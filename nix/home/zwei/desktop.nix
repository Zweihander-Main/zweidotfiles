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
    #./emacs
    ./redshift
    ./stalonetray
    ./sxhkd
  ];

  home.packages = with pkgs; [
  ];
}
