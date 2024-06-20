{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  home.packages = with pkgs; [
    (mkIf (!config.hostAttr.preinstalled.udiskie) udiskie)
  ];

  systemd.user.services.udiskie = {
    Unit = {
      Description = "Auto mounting with Udiskie -- usb/disk manager";
      Documentation = "man:udiskie(8)";
      Wants = ["tray.service"];
      After = ["tray.service"];
    };

    Service = {
      Type = "exec";
      ExecStart = "udiskie";
      RestartSec = 5;
      Restart = "always";
    };

    Install = {WantedBy = ["wm.target"];};
  };

  xdg.configFile."udiskie/config.yml".source = ./config.yml;
}
