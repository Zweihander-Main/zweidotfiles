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
    keynav
    xmouseless
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
}
