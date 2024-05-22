{
  pkgs,
  config,
  lib,
  secrets,
  ...
}: let
  homeDir = config.home.homeDirectory;
in {
  home.packages = with pkgs; [
    redshift
    acpilight
  ];

  xdg.configFile."redshift/redshift.conf".source = ./redshift.conf;
  xdg.configFile."redshift/hooks/brightness.sh".source = ./brightness.sh;

  systemd.user.services.nix_redshift = {
    Unit = {
      Description = "Redshift display color temperature adjustment";
      Documentation = "man:redshift(1)";
      PartOf = ["graphical-session.target"];
    };

    Service = {
      Type = "exec";
      ExecStart = "${pkgs.redshift}/bin/redshift";
      RestartSec = 5;
      Restart = "on-failure";
    };

    Install = {WantedBy = ["wm.target"];};
  };
}
