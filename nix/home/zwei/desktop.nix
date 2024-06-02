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
    ./server.nix
    ./redshift.nix
    #./emacs.nix
    #./stalonetray.nix
  ];

  home.packages = with pkgs; [
  ];
}
