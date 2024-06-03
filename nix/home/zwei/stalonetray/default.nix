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
    stalonetray
  ];

  xdg.configFile."stalonetray/stalonetrayrc".source = ./stalonetrayrc;

  systemd.user.services.tray = {
    Unit = {
      Description = "System tray using stalonetray";
      PartOf = ["graphical-session.target"];
      ConditionFileIsExecutable = "${pkgs.stalonetray}/bin/stalonetray";
      ConditionPathExists = "%h/.config/stalonetray/stalonetrayrc";
    };

    Service = {
      Type = "exec";
      ExecStart = "${pkgs.stalonetray}/bin/stalonetray -c %h/.config/stalonetray/stalonetrayrc";
      RestartSec = 5;
      Restart = "on-failure";
      ExecStartPost = "${pkgs.coreutils}/bin/sleep 1";
    };

    Install = {WantedBy = ["wm.target"];};
  };
}
