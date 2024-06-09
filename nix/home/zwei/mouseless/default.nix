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
    keynav           # fork in overlay
    xmouseless       # fork in pkgs
    unclutter-xfixes # unmodified
  ];

  xdg.configFile."keynav/keynavrc".source = ./keynavrc;

  systemd.user.services.keynav = {
    Unit = {
      Description = "Keynav mouse-free application";
      PartOf = ["graphical-session.target"];
      ConditionFileIsExecutable = "${pkgs.keynav}/bin/keynav";
    };

    Service = {
      Type = "exec";
      ExecStart = "${pkgs.keynav}/bin/keynav";
      RestartSec = 10;
      Restart = "always";
    };

    Install = {WantedBy = ["wm.target"];};
  };

  systemd.user.services.unclutter = {
    Unit = {
      Description = "Hide mouse cursor with unclutter";
      Documentation = "man:unclutter-xfixes(1)";
      ConditionFileIsExecutable = "${pkgs.unclutter-xfixes}/bin/unclutter";
    };

    Service = {
      Type = "exec";
      ExecStart = "${pkgs.unclutter-xfixes}/bin/unclutter --timeout 1";
      RestartSec = 10;
      Restart = "always";
    };

    Install = {WantedBy = ["wm.target"];};
  };
}
