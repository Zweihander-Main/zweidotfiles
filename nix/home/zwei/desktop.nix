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
  ];
 
  redshift = {
    enable = true;
  };

  home.packages = with pkgs; [
  ];
}
