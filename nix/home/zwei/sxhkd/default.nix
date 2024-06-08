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
    sxhkd
  ];

  xdg.configFile."sxhkd/sxhkdrc".source = ./sxhkdrc;

  systemd.user.services.sxhkd = {
    Unit = {
      Description = "simple X hotkey daemon";
      Documentation = "man:sxhkd(1)";
      PartOf = ["graphical-session.target"];
      ConditionFileIsExecutable = "${pkgs.sxhkd}/bin/sxhkd";
    };

    Service = {
      Type = "exec";
      ExecStart = "${pkgs.sxhkd}/bin/sxhkd";
      RestartSec = 10;
      Restart = "on-failure";
    };

    Install = {WantedBy = ["wm.target"];};
  };
}
