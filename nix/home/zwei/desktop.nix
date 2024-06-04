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
    ./redshift
    #./emacs
    ./stalonetray
  ];

  home.packages = with pkgs; [
  ];
}
